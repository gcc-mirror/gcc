/* Run a stand-alone AMD GCN kernel.

   Copyright 2017 Mentor Graphics Corporation
   Copyright 2018-2019 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* This program will run a compiled stand-alone GCN kernel on a GPU.

   The kernel entry point's signature must use a standard main signature:

     int main(int argc, char **argv)
*/

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <dlfcn.h>
#include <unistd.h>
#include <elf.h>
#include <signal.h>

/* These probably won't be in elf.h for a while.  */
#ifndef R_AMDGPU_NONE
#define R_AMDGPU_NONE		0
#define R_AMDGPU_ABS32_LO	1	/* (S + A) & 0xFFFFFFFF  */
#define R_AMDGPU_ABS32_HI	2	/* (S + A) >> 32  */
#define R_AMDGPU_ABS64		3	/* S + A  */
#define R_AMDGPU_REL32		4	/* S + A - P  */
#define R_AMDGPU_REL64		5	/* S + A - P  */
#define R_AMDGPU_ABS32		6	/* S + A  */
#define R_AMDGPU_GOTPCREL	7	/* G + GOT + A - P  */
#define R_AMDGPU_GOTPCREL32_LO	8	/* (G + GOT + A - P) & 0xFFFFFFFF  */
#define R_AMDGPU_GOTPCREL32_HI	9	/* (G + GOT + A - P) >> 32  */
#define R_AMDGPU_REL32_LO	10	/* (S + A - P) & 0xFFFFFFFF  */
#define R_AMDGPU_REL32_HI	11	/* (S + A - P) >> 32  */
#define reserved		12
#define R_AMDGPU_RELATIVE64	13	/* B + A  */
#endif

#include "hsa.h"

#ifndef HSA_RUNTIME_LIB
#define HSA_RUNTIME_LIB "libhsa-runtime64.so"
#endif

#ifndef VERSION_STRING
#define VERSION_STRING "(version unknown)"
#endif

bool debug = false;

hsa_agent_t device = { 0 };
hsa_queue_t *queue = NULL;
uint64_t init_array_kernel = 0;
uint64_t fini_array_kernel = 0;
uint64_t main_kernel = 0;
hsa_executable_t executable = { 0 };

hsa_region_t kernargs_region = { 0 };
hsa_region_t heap_region = { 0 };
uint32_t kernarg_segment_size = 0;
uint32_t group_segment_size = 0;
uint32_t private_segment_size = 0;

static void
usage (const char *progname)
{
  printf ("Usage: %s [options] kernel [kernel-args]\n\n"
	  "Options:\n"
	  "  --help\n"
	  "  --version\n"
	  "  --debug\n", progname);
}

static void
version (const char *progname)
{
  printf ("%s " VERSION_STRING "\n", progname);
}

/* As an HSA runtime is dlopened, following structure defines the necessary
   function pointers.
   Code adapted from libgomp.  */

struct hsa_runtime_fn_info
{
  /* HSA runtime.  */
  hsa_status_t (*hsa_status_string_fn) (hsa_status_t status,
					const char **status_string);
  hsa_status_t (*hsa_agent_get_info_fn) (hsa_agent_t agent,
					 hsa_agent_info_t attribute,
					 void *value);
  hsa_status_t (*hsa_init_fn) (void);
  hsa_status_t (*hsa_iterate_agents_fn)
    (hsa_status_t (*callback) (hsa_agent_t agent, void *data), void *data);
  hsa_status_t (*hsa_region_get_info_fn) (hsa_region_t region,
					  hsa_region_info_t attribute,
					  void *value);
  hsa_status_t (*hsa_queue_create_fn)
    (hsa_agent_t agent, uint32_t size, hsa_queue_type_t type,
     void (*callback) (hsa_status_t status, hsa_queue_t *source, void *data),
     void *data, uint32_t private_segment_size,
     uint32_t group_segment_size, hsa_queue_t **queue);
  hsa_status_t (*hsa_agent_iterate_regions_fn)
    (hsa_agent_t agent,
     hsa_status_t (*callback) (hsa_region_t region, void *data), void *data);
  hsa_status_t (*hsa_executable_destroy_fn) (hsa_executable_t executable);
  hsa_status_t (*hsa_executable_create_fn)
    (hsa_profile_t profile, hsa_executable_state_t executable_state,
     const char *options, hsa_executable_t *executable);
  hsa_status_t (*hsa_executable_global_variable_define_fn)
    (hsa_executable_t executable, const char *variable_name, void *address);
  hsa_status_t (*hsa_executable_load_code_object_fn)
    (hsa_executable_t executable, hsa_agent_t agent,
     hsa_code_object_t code_object, const char *options);
  hsa_status_t (*hsa_executable_freeze_fn) (hsa_executable_t executable,
					    const char *options);
  hsa_status_t (*hsa_signal_create_fn) (hsa_signal_value_t initial_value,
					uint32_t num_consumers,
					const hsa_agent_t *consumers,
					hsa_signal_t *signal);
  hsa_status_t (*hsa_memory_allocate_fn) (hsa_region_t region, size_t size,
					  void **ptr);
  hsa_status_t (*hsa_memory_assign_agent_fn) (void *ptr, hsa_agent_t agent,
					      hsa_access_permission_t access);
  hsa_status_t (*hsa_memory_copy_fn) (void *dst, const void *src,
				      size_t size);
  hsa_status_t (*hsa_memory_free_fn) (void *ptr);
  hsa_status_t (*hsa_signal_destroy_fn) (hsa_signal_t signal);
  hsa_status_t (*hsa_executable_get_symbol_fn)
    (hsa_executable_t executable, const char *module_name,
     const char *symbol_name, hsa_agent_t agent, int32_t call_convention,
     hsa_executable_symbol_t *symbol);
  hsa_status_t (*hsa_executable_symbol_get_info_fn)
    (hsa_executable_symbol_t executable_symbol,
     hsa_executable_symbol_info_t attribute, void *value);
  void (*hsa_signal_store_relaxed_fn) (hsa_signal_t signal,
				       hsa_signal_value_t value);
  hsa_signal_value_t (*hsa_signal_wait_acquire_fn)
    (hsa_signal_t signal, hsa_signal_condition_t condition,
     hsa_signal_value_t compare_value, uint64_t timeout_hint,
     hsa_wait_state_t wait_state_hint);
  hsa_signal_value_t (*hsa_signal_wait_relaxed_fn)
    (hsa_signal_t signal, hsa_signal_condition_t condition,
     hsa_signal_value_t compare_value, uint64_t timeout_hint,
     hsa_wait_state_t wait_state_hint);
  hsa_status_t (*hsa_queue_destroy_fn) (hsa_queue_t *queue);
  hsa_status_t (*hsa_code_object_deserialize_fn)
    (void *serialized_code_object, size_t serialized_code_object_size,
     const char *options, hsa_code_object_t *code_object);
  uint64_t (*hsa_queue_load_write_index_relaxed_fn)
    (const hsa_queue_t *queue);
  void (*hsa_queue_store_write_index_relaxed_fn)
    (const hsa_queue_t *queue, uint64_t value);
  hsa_status_t (*hsa_shut_down_fn) ();
};

/* HSA runtime functions that are initialized in init_hsa_context.
   Code adapted from libgomp.  */

static struct hsa_runtime_fn_info hsa_fns;

#define DLSYM_FN(function)					 \
  *(void**)(&hsa_fns.function##_fn) = dlsym (handle, #function); \
  if (hsa_fns.function##_fn == NULL)				 \
    goto fail;

static void
init_hsa_runtime_functions (void)
{
  void *handle = dlopen (HSA_RUNTIME_LIB, RTLD_LAZY);
  if (handle == NULL)
    {
      fprintf (stderr,
	       "The HSA runtime is required to run GCN kernels on hardware.\n"
	       "%s: File not found or could not be opened\n",
	       HSA_RUNTIME_LIB);
      exit (1);
    }

  DLSYM_FN (hsa_status_string)
  DLSYM_FN (hsa_agent_get_info)
  DLSYM_FN (hsa_init)
  DLSYM_FN (hsa_iterate_agents)
  DLSYM_FN (hsa_region_get_info)
  DLSYM_FN (hsa_queue_create)
  DLSYM_FN (hsa_agent_iterate_regions)
  DLSYM_FN (hsa_executable_destroy)
  DLSYM_FN (hsa_executable_create)
  DLSYM_FN (hsa_executable_global_variable_define)
  DLSYM_FN (hsa_executable_load_code_object)
  DLSYM_FN (hsa_executable_freeze)
  DLSYM_FN (hsa_signal_create)
  DLSYM_FN (hsa_memory_allocate)
  DLSYM_FN (hsa_memory_assign_agent)
  DLSYM_FN (hsa_memory_copy)
  DLSYM_FN (hsa_memory_free)
  DLSYM_FN (hsa_signal_destroy)
  DLSYM_FN (hsa_executable_get_symbol)
  DLSYM_FN (hsa_executable_symbol_get_info)
  DLSYM_FN (hsa_signal_wait_acquire)
  DLSYM_FN (hsa_signal_wait_relaxed)
  DLSYM_FN (hsa_signal_store_relaxed)
  DLSYM_FN (hsa_queue_destroy)
  DLSYM_FN (hsa_code_object_deserialize)
  DLSYM_FN (hsa_queue_load_write_index_relaxed)
  DLSYM_FN (hsa_queue_store_write_index_relaxed)
  DLSYM_FN (hsa_shut_down)

  return;

fail:
  fprintf (stderr, "Failed to find HSA functions in " HSA_RUNTIME_LIB "\n");
  exit (1);
}

#undef DLSYM_FN

/* Report a fatal error STR together with the HSA error corresponding to
   STATUS and terminate execution of the current process.  */

static void
hsa_fatal (const char *str, hsa_status_t status)
{
  const char *hsa_error_msg;
  hsa_fns.hsa_status_string_fn (status, &hsa_error_msg);
  fprintf (stderr, "%s: FAILED\nHSA Runtime message: %s\n", str,
	   hsa_error_msg);
  exit (1);
}

/* Helper macros to ensure we check the return values from the HSA Runtime.
   These just keep the rest of the code a bit cleaner.  */

#define XHSA_CMP(FN, CMP, MSG)		   \
  do {					   \
    hsa_status_t status = (FN);		   \
    if (!(CMP))				   \
      hsa_fatal ((MSG), status);	   \
    else if (debug)			   \
      fprintf (stderr, "%s: OK\n", (MSG)); \
  } while (0)
#define XHSA(FN, MSG) XHSA_CMP(FN, status == HSA_STATUS_SUCCESS, MSG)

/* Callback of hsa_iterate_agents.
   Called once for each available device, and returns "break" when a
   suitable one has been found.  */

static hsa_status_t
get_gpu_agent (hsa_agent_t agent, void *data __attribute__ ((unused)))
{
  hsa_device_type_t device_type;
  XHSA (hsa_fns.hsa_agent_get_info_fn (agent, HSA_AGENT_INFO_DEVICE,
				       &device_type),
	"Get agent type");

  /* Select only GPU devices.  */
  /* TODO: support selecting from multiple GPUs.  */
  if (HSA_DEVICE_TYPE_GPU == device_type)
    {
      device = agent;
      return HSA_STATUS_INFO_BREAK;
    }

  /* The device was not suitable.  */
  return HSA_STATUS_SUCCESS;
}

/* Callback of hsa_iterate_regions.
   Called once for each available memory region, and returns "break" when a
   suitable one has been found.  */

static hsa_status_t
get_memory_region (hsa_region_t region, hsa_region_t *retval,
		   hsa_region_global_flag_t kind)
{
  /* Reject non-global regions.  */
  hsa_region_segment_t segment;
  hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_SEGMENT, &segment);
  if (HSA_REGION_SEGMENT_GLOBAL != segment)
    return HSA_STATUS_SUCCESS;

  /* Find a region with the KERNARG flag set.  */
  hsa_region_global_flag_t flags;
  hsa_fns.hsa_region_get_info_fn (region, HSA_REGION_INFO_GLOBAL_FLAGS,
				  &flags);
  if (flags & kind)
    {
      *retval = region;
      return HSA_STATUS_INFO_BREAK;
    }

  /* The region was not suitable.  */
  return HSA_STATUS_SUCCESS;
}

static hsa_status_t
get_kernarg_region (hsa_region_t region, void *data __attribute__((unused)))
{
  return get_memory_region (region, &kernargs_region,
			    HSA_REGION_GLOBAL_FLAG_KERNARG);
}

static hsa_status_t
get_heap_region (hsa_region_t region, void *data __attribute__((unused)))
{
  return get_memory_region (region, &heap_region,
			    HSA_REGION_GLOBAL_FLAG_COARSE_GRAINED);
}

/* Initialize the HSA Runtime library and GPU device.  */

static void
init_device ()
{
  /* Load the shared library and find the API functions.  */
  init_hsa_runtime_functions ();

  /* Initialize the HSA Runtime.  */
  XHSA (hsa_fns.hsa_init_fn (),
	"Initialize run-time");

  /* Select a suitable device.
     The call-back function, get_gpu_agent, does the selection.  */
  XHSA_CMP (hsa_fns.hsa_iterate_agents_fn (get_gpu_agent, NULL),
	    status == HSA_STATUS_SUCCESS || status == HSA_STATUS_INFO_BREAK,
	    "Find a device");

  /* Initialize the queue used for launching kernels.  */
  uint32_t queue_size = 0;
  XHSA (hsa_fns.hsa_agent_get_info_fn (device, HSA_AGENT_INFO_QUEUE_MAX_SIZE,
				       &queue_size),
	"Find max queue size");
  XHSA (hsa_fns.hsa_queue_create_fn (device, queue_size,
				     HSA_QUEUE_TYPE_SINGLE, NULL,
				     NULL, UINT32_MAX, UINT32_MAX, &queue),
	"Set up a device queue");

  /* Select a memory region for the kernel arguments.
     The call-back function, get_kernarg_region, does the selection.  */
  XHSA_CMP (hsa_fns.hsa_agent_iterate_regions_fn (device, get_kernarg_region,
						  NULL),
	    status == HSA_STATUS_SUCCESS || status == HSA_STATUS_INFO_BREAK,
	    "Locate kernargs memory");

  /* Select a memory region for the kernel heap.
     The call-back function, get_heap_region, does the selection.  */
  XHSA_CMP (hsa_fns.hsa_agent_iterate_regions_fn (device, get_heap_region,
						  NULL),
	    status == HSA_STATUS_SUCCESS || status == HSA_STATUS_INFO_BREAK,
	    "Locate device memory");
}


/* Read a whole input file.
   Code copied from mkoffload. */

static char *
read_file (const char *filename, size_t *plen)
{
  size_t alloc = 16384;
  size_t base = 0;
  char *buffer;

  FILE *stream = fopen (filename, "rb");
  if (!stream)
    {
      perror (filename);
      exit (1);
    }

  if (!fseek (stream, 0, SEEK_END))
    {
      /* Get the file size.  */
      long s = ftell (stream);
      if (s >= 0)
	alloc = s + 100;
      fseek (stream, 0, SEEK_SET);
    }
  buffer = malloc (alloc);

  for (;;)
    {
      size_t n = fread (buffer + base, 1, alloc - base - 1, stream);

      if (!n)
	break;
      base += n;
      if (base + 1 == alloc)
	{
	  alloc *= 2;
	  buffer = realloc (buffer, alloc);
	}
    }
  buffer[base] = 0;
  *plen = base;

  fclose (stream);

  return buffer;
}

/* Read a HSA Code Object (HSACO) from file, and load it into the device.  */

static void
load_image (const char *filename)
{
  size_t image_size;
  Elf64_Ehdr *image = (void *) read_file (filename, &image_size);

  /* An "executable" consists of one or more code objects.  */
  XHSA (hsa_fns.hsa_executable_create_fn (HSA_PROFILE_FULL,
					  HSA_EXECUTABLE_STATE_UNFROZEN, "",
					  &executable),
	"Initialize GCN executable");

  /* Hide relocations from the HSA runtime loader.
     Keep a copy of the unmodified section headers to use later.  */
  Elf64_Shdr *image_sections =
    (Elf64_Shdr *) ((char *) image + image->e_shoff);
  Elf64_Shdr *sections = malloc (sizeof (Elf64_Shdr) * image->e_shnum);
  memcpy (sections, image_sections, sizeof (Elf64_Shdr) * image->e_shnum);
  for (int i = image->e_shnum - 1; i >= 0; i--)
    {
      if (image_sections[i].sh_type == SHT_RELA
	  || image_sections[i].sh_type == SHT_REL)
	/* Change section type to something harmless.  */
	image_sections[i].sh_type = SHT_NOTE;
    }

  /* Add the HSACO to the executable.  */
  hsa_code_object_t co = { 0 };
  XHSA (hsa_fns.hsa_code_object_deserialize_fn (image, image_size, NULL, &co),
	"Deserialize GCN code object");
  XHSA (hsa_fns.hsa_executable_load_code_object_fn (executable, device, co,
						    ""),
	"Load GCN code object");

  /* We're done modifying he executable.  */
  XHSA (hsa_fns.hsa_executable_freeze_fn (executable, ""),
	"Freeze GCN executable");

  /* Locate the "_init_array" function, and read the kernel's properties.  */
  hsa_executable_symbol_t symbol;
  XHSA (hsa_fns.hsa_executable_get_symbol_fn (executable, NULL, "_init_array",
					      device, 0, &symbol),
	"Find '_init_array' function");
  XHSA (hsa_fns.hsa_executable_symbol_get_info_fn
	    (symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_OBJECT, &init_array_kernel),
	"Extract '_init_array' kernel object kernel object");

  /* Locate the "_fini_array" function, and read the kernel's properties.  */
  XHSA (hsa_fns.hsa_executable_get_symbol_fn (executable, NULL, "_fini_array",
					      device, 0, &symbol),
	"Find '_fini_array' function");
  XHSA (hsa_fns.hsa_executable_symbol_get_info_fn
	    (symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_OBJECT, &fini_array_kernel),
	"Extract '_fini_array' kernel object kernel object");

  /* Locate the "main" function, and read the kernel's properties.  */
  XHSA (hsa_fns.hsa_executable_get_symbol_fn (executable, NULL, "main",
					      device, 0, &symbol),
	"Find 'main' function");
  XHSA (hsa_fns.hsa_executable_symbol_get_info_fn
	    (symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_OBJECT, &main_kernel),
	"Extract 'main' kernel object");
  XHSA (hsa_fns.hsa_executable_symbol_get_info_fn
	    (symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_KERNARG_SEGMENT_SIZE,
	     &kernarg_segment_size),
	"Extract kernarg segment size");
  XHSA (hsa_fns.hsa_executable_symbol_get_info_fn
	    (symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_GROUP_SEGMENT_SIZE,
	     &group_segment_size),
	"Extract group segment size");
  XHSA (hsa_fns.hsa_executable_symbol_get_info_fn
	    (symbol, HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_PRIVATE_SEGMENT_SIZE,
	     &private_segment_size),
	"Extract private segment size");

  /* Find main function in ELF, and calculate actual load offset.  */
  Elf64_Addr load_offset;
  XHSA (hsa_fns.hsa_executable_symbol_get_info_fn
	    (symbol, HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ADDRESS,
	     &load_offset),
	"Extract 'main' symbol address");
  for (int i = 0; i < image->e_shnum; i++)
    if (sections[i].sh_type == SHT_SYMTAB)
      {
	Elf64_Shdr *strtab = &sections[sections[i].sh_link];
	char *strings = (char *) image + strtab->sh_offset;

	for (size_t offset = 0;
	     offset < sections[i].sh_size;
	     offset += sections[i].sh_entsize)
	  {
	    Elf64_Sym *sym = (Elf64_Sym *) ((char *) image
					    + sections[i].sh_offset + offset);
	    if (strcmp ("main", strings + sym->st_name) == 0)
	      {
		load_offset -= sym->st_value;
		goto found_main;
	      }
	  }
      }
  /* We only get here when main was not found.
     This should never happen.  */
  fprintf (stderr, "Error: main function not found.\n");
  abort ();
found_main:;

  /* Find dynamic symbol table.  */
  Elf64_Shdr *dynsym = NULL;
  for (int i = 0; i < image->e_shnum; i++)
    if (sections[i].sh_type == SHT_DYNSYM)
      {
	dynsym = &sections[i];
	break;
      }

  /* Fix up relocations.  */
  for (int i = 0; i < image->e_shnum; i++)
    {
      if (sections[i].sh_type == SHT_RELA)
	for (size_t offset = 0;
	     offset < sections[i].sh_size;
	     offset += sections[i].sh_entsize)
	  {
	    Elf64_Rela *reloc = (Elf64_Rela *) ((char *) image
						+ sections[i].sh_offset
						+ offset);
	    Elf64_Sym *sym =
	      (dynsym
	       ? (Elf64_Sym *) ((char *) image
				+ dynsym->sh_offset
				+ (dynsym->sh_entsize
				   * ELF64_R_SYM (reloc->r_info))) : NULL);

	    int64_t S = (sym ? sym->st_value : 0);
	    int64_t P = reloc->r_offset + load_offset;
	    int64_t A = reloc->r_addend;
	    int64_t B = load_offset;
	    int64_t V, size;
	    switch (ELF64_R_TYPE (reloc->r_info))
	      {
	      case R_AMDGPU_ABS32_LO:
		V = (S + A) & 0xFFFFFFFF;
		size = 4;
		break;
	      case R_AMDGPU_ABS32_HI:
		V = (S + A) >> 32;
		size = 4;
		break;
	      case R_AMDGPU_ABS64:
		V = S + A;
		size = 8;
		break;
	      case R_AMDGPU_REL32:
		V = S + A - P;
		size = 4;
		break;
	      case R_AMDGPU_REL64:
		/* FIXME
		   LLD seems to emit REL64 where the the assembler has ABS64.
		   This is clearly wrong because it's not what the compiler
		   is expecting.  Let's assume, for now, that it's a bug.
		   In any case, GCN kernels are always self contained and
		   therefore relative relocations will have been resolved
		   already, so this should be a safe workaround.  */
		V = S + A /* - P */ ;
		size = 8;
		break;
	      case R_AMDGPU_ABS32:
		V = S + A;
		size = 4;
		break;
	      /* TODO R_AMDGPU_GOTPCREL */
	      /* TODO R_AMDGPU_GOTPCREL32_LO */
	      /* TODO R_AMDGPU_GOTPCREL32_HI */
	      case R_AMDGPU_REL32_LO:
		V = (S + A - P) & 0xFFFFFFFF;
		size = 4;
		break;
	      case R_AMDGPU_REL32_HI:
		V = (S + A - P) >> 32;
		size = 4;
		break;
	      case R_AMDGPU_RELATIVE64:
		V = B + A;
		size = 8;
		break;
	      default:
		fprintf (stderr, "Error: unsupported relocation type.\n");
		exit (1);
	      }
	    XHSA (hsa_fns.hsa_memory_copy_fn ((void *) P, &V, size),
		  "Fix up relocation");
	  }
    }
}

/* Allocate some device memory from the kernargs region.
   The returned address will be 32-bit (with excess zeroed on 64-bit host),
   and accessible via the same address on both host and target (via
   __flat_scalar GCN address space).  */

static void *
device_malloc (size_t size, hsa_region_t region)
{
  void *result;
  XHSA (hsa_fns.hsa_memory_allocate_fn (region, size, &result),
	"Allocate device memory");
  return result;
}

/* These are the device pointers that will be transferred to the target.
   The HSA Runtime points the kernargs register here.
   They correspond to function signature:
       int main (int argc, char *argv[], int *return_value)
   The compiler expects this, for kernel functions, and will
   automatically assign the exit value to *return_value.  */
struct kernargs
{
  /* Kernargs.  */
  int32_t argc;
  int64_t argv;
  int64_t out_ptr;
  int64_t heap_ptr;

  /* Output data.  */
  struct output
  {
    int return_value;
    unsigned int next_output;
    struct printf_data
    {
      int written;
      char msg[128];
      int type;
      union
      {
	int64_t ivalue;
	double dvalue;
	char text[128];
      };
    } queue[1024];
    unsigned int consumed;
  } output_data;
};

struct heap
{
  int64_t size;
  char data[0];
} heap;

/* Print any console output from the kernel.
   We print all entries from "consumed" to the next entry without a "written"
   flag, or "next_output" is reached.  The buffer is circular, but the
   indices are absolute.  It is assumed the kernel will stop writing data
   if "next_output" wraps (becomes smaller than "consumed").  */
void
gomp_print_output (struct kernargs *kernargs, bool final)
{
  unsigned int limit = (sizeof (kernargs->output_data.queue)
			/ sizeof (kernargs->output_data.queue[0]));

  unsigned int from = __atomic_load_n (&kernargs->output_data.consumed,
				       __ATOMIC_ACQUIRE);
  unsigned int to = kernargs->output_data.next_output;

  if (from > to)
    {
      /* Overflow.  */
      if (final)
	printf ("GCN print buffer overflowed.\n");
      return;
    }

  unsigned int i;
  for (i = from; i < to; i++)
    {
      struct printf_data *data = &kernargs->output_data.queue[i%limit];

      if (!data->written && !final)
	break;

      switch (data->type)
	{
	case 0:
	  printf ("%.128s%ld\n", data->msg, data->ivalue);
	  break;
	case 1:
	  printf ("%.128s%f\n", data->msg, data->dvalue);
	  break;
	case 2:
	  printf ("%.128s%.128s\n", data->msg, data->text);
	  break;
	case 3:
	  printf ("%.128s%.128s", data->msg, data->text);
	  break;
	default:
	  printf ("GCN print buffer error!\n");
	  break;
	}

      data->written = 0;
      __atomic_store_n (&kernargs->output_data.consumed, i+1,
			__ATOMIC_RELEASE);
    }
  fflush (stdout);
}

/* Execute an already-loaded kernel on the device.  */

static void
run (uint64_t kernel, void *kernargs)
{
  /* A "signal" is used to launch and monitor the kernel.  */
  hsa_signal_t signal;
  XHSA (hsa_fns.hsa_signal_create_fn (1, 0, NULL, &signal),
	"Create signal");

  /* Configure for a single-worker kernel.  */
  uint64_t index = hsa_fns.hsa_queue_load_write_index_relaxed_fn (queue);
  const uint32_t queueMask = queue->size - 1;
  hsa_kernel_dispatch_packet_t *dispatch_packet =
    &(((hsa_kernel_dispatch_packet_t *) (queue->base_address))[index &
							       queueMask]);
  dispatch_packet->setup |= 3 << HSA_KERNEL_DISPATCH_PACKET_SETUP_DIMENSIONS;
  dispatch_packet->workgroup_size_x = (uint16_t) 1;
  dispatch_packet->workgroup_size_y = (uint16_t) 64;
  dispatch_packet->workgroup_size_z = (uint16_t) 1;
  dispatch_packet->grid_size_x = 1;
  dispatch_packet->grid_size_y = 64;
  dispatch_packet->grid_size_z = 1;
  dispatch_packet->completion_signal = signal;
  dispatch_packet->kernel_object = kernel;
  dispatch_packet->kernarg_address = (void *) kernargs;
  dispatch_packet->private_segment_size = private_segment_size;
  dispatch_packet->group_segment_size = group_segment_size;

  uint16_t header = 0;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_ACQUIRE_FENCE_SCOPE;
  header |= HSA_FENCE_SCOPE_SYSTEM << HSA_PACKET_HEADER_RELEASE_FENCE_SCOPE;
  header |= HSA_PACKET_TYPE_KERNEL_DISPATCH << HSA_PACKET_HEADER_TYPE;

  __atomic_store_n ((uint32_t *) dispatch_packet,
		    header | (dispatch_packet->setup << 16),
		    __ATOMIC_RELEASE);

  if (debug)
    fprintf (stderr, "Launch kernel\n");

  hsa_fns.hsa_queue_store_write_index_relaxed_fn (queue, index + 1);
  hsa_fns.hsa_signal_store_relaxed_fn (queue->doorbell_signal, index);
  /* Kernel running ......  */
  while (hsa_fns.hsa_signal_wait_relaxed_fn (signal, HSA_SIGNAL_CONDITION_LT,
					     1, 1000000,
					     HSA_WAIT_STATE_ACTIVE) != 0)
    {
      usleep (10000);
      gomp_print_output (kernargs, false);
    }

  gomp_print_output (kernargs, true);

  if (debug)
    fprintf (stderr, "Kernel exited\n");

  XHSA (hsa_fns.hsa_signal_destroy_fn (signal),
	"Clean up signal");
}

int
main (int argc, char *argv[])
{
  int kernel_arg = 0;
  for (int i = 1; i < argc; i++)
    {
      if (!strcmp (argv[i], "--help"))
	{
	  usage (argv[0]);
	  return 0;
	}
      else if (!strcmp (argv[i], "--version"))
	{
	  version (argv[0]);
	  return 0;
	}
      else if (!strcmp (argv[i], "--debug"))
	debug = true;
      else if (argv[i][0] == '-')
	{
	  usage (argv[0]);
	  return 1;
	}
      else
	{
	  kernel_arg = i;
	  break;
	}
    }

  if (!kernel_arg)
    {
      /* No kernel arguments were found.  */
      usage (argv[0]);
      return 1;
    }

  /* The remaining arguments are for the GCN kernel.  */
  int kernel_argc = argc - kernel_arg;
  char **kernel_argv = &argv[kernel_arg];

  init_device ();
  load_image (kernel_argv[0]);

  /* Calculate size of function parameters + argv data.  */
  size_t args_size = 0;
  for (int i = 0; i < kernel_argc; i++)
    args_size += strlen (kernel_argv[i]) + 1;

  /* Allocate device memory for both function parameters and the argv
     data.  */
  struct kernargs *kernargs = device_malloc (sizeof (*kernargs),
					     kernargs_region);
  struct argdata
  {
    int64_t argv_data[kernel_argc];
    char strings[args_size];
  } *args = device_malloc (sizeof (struct argdata), kernargs_region);

  size_t heap_size = 10 * 1024 * 1024;	/* 10MB.  */
  struct heap *heap = device_malloc (heap_size, heap_region);
  XHSA (hsa_fns.hsa_memory_assign_agent_fn (heap, device,
					    HSA_ACCESS_PERMISSION_RW),
	"Assign heap to device agent");

  /* Write the data to the target.  */
  kernargs->argc = kernel_argc;
  kernargs->argv = (int64_t) args->argv_data;
  kernargs->out_ptr = (int64_t) &kernargs->output_data;
  kernargs->output_data.return_value = 0xcafe0000; /* Default return value. */
  kernargs->output_data.next_output = 0;
  for (unsigned i = 0; i < (sizeof (kernargs->output_data.queue)
			    / sizeof (kernargs->output_data.queue[0])); i++)
    kernargs->output_data.queue[i].written = 0;
  kernargs->output_data.consumed = 0;
  int offset = 0;
  for (int i = 0; i < kernel_argc; i++)
    {
      size_t arg_len = strlen (kernel_argv[i]) + 1;
      args->argv_data[i] = (int64_t) &args->strings[offset];
      memcpy (&args->strings[offset], kernel_argv[i], arg_len + 1);
      offset += arg_len;
    }
  kernargs->heap_ptr = (int64_t) heap;
  hsa_fns.hsa_memory_copy_fn (&heap->size, &heap_size, sizeof (heap_size));

  /* Run constructors on the GPU.  */
  run (init_array_kernel, kernargs);

  /* Run the kernel on the GPU.  */
  run (main_kernel, kernargs);
  unsigned int return_value =
    (unsigned int) kernargs->output_data.return_value;

  /* Run destructors on the GPU.  */
  run (fini_array_kernel, kernargs);

  unsigned int upper = (return_value & ~0xffff) >> 16;
  if (upper == 0xcafe)
    {
      printf ("Kernel exit value was never set\n");
      return_value = 0xff;
    }
  else if (upper == 0xffff)
    ; /* Set by exit.  */
  else if (upper == 0)
    ; /* Set by return from main.  */
  else
    printf ("Possible kernel exit value corruption, 2 most significant bytes "
	    "aren't 0xffff, 0xcafe, or 0: 0x%x\n", return_value);

  if (upper == 0xffff)
    {
      unsigned int signal = (return_value >> 8) & 0xff;
      if (signal == SIGABRT)
	printf ("Kernel aborted\n");
      else if (signal != 0)
	printf ("Kernel received unkown signal\n");
    }

  if (debug)
    printf ("Kernel exit value: %d\n", return_value & 0xff);

  /* Clean shut down.  */
  XHSA (hsa_fns.hsa_memory_free_fn (kernargs),
	"Clean up device memory");
  XHSA (hsa_fns.hsa_executable_destroy_fn (executable),
	"Clean up GCN executable");
  XHSA (hsa_fns.hsa_queue_destroy_fn (queue),
	"Clean up device queue");
  XHSA (hsa_fns.hsa_shut_down_fn (),
	"Shut down run-time");

  return return_value & 0xff;
}
