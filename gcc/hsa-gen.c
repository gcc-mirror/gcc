/* A pass for lowering gimple to HSAIL
   Copyright (C) 2013-2016 Free Software Foundation, Inc.
   Contributed by Martin Jambor <mjambor@suse.cz> and
   Martin Liska <mliska@suse.cz>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "is-a.h"
#include "hash-table.h"
#include "vec.h"
#include "tree.h"
#include "tree-pass.h"
#include "cfg.h"
#include "function.h"
#include "basic-block.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "bitmap.h"
#include "dumpfile.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "alloc-pool.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-dfa.h"
#include "ssa-iterators.h"
#include "cgraph.h"
#include "print-tree.h"
#include "symbol-summary.h"
#include "hsa.h"
#include "cfghooks.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "cfganal.h"
#include "builtins.h"
#include "params.h"
#include "gomp-constants.h"
#include "internal-fn.h"
#include "builtins.h"
#include "stor-layout.h"

/* Print a warning message and set that we have seen an error.  */

#define HSA_SORRY_ATV(location, message, ...) \
  do \
  { \
    hsa_fail_cfun (); \
    if (warning_at (EXPR_LOCATION (hsa_cfun->m_decl), OPT_Whsa, \
		    HSA_SORRY_MSG)) \
      inform (location, message, __VA_ARGS__); \
  } \
  while (false);

/* Same as previous, but highlight a location.  */

#define HSA_SORRY_AT(location, message) \
  do \
  { \
    hsa_fail_cfun (); \
    if (warning_at (EXPR_LOCATION (hsa_cfun->m_decl), OPT_Whsa, \
		    HSA_SORRY_MSG)) \
      inform (location, message); \
  } \
  while (false);

/* Default number of threads used by kernel dispatch.  */

#define HSA_DEFAULT_NUM_THREADS 64

/* Following structures are defined in the final version
   of HSA specification.  */

/* HSA queue packet is shadow structure, originally provided by AMD.  */

struct hsa_queue_packet
{
  uint16_t header;
  uint16_t setup;
  uint16_t workgroup_size_x;
  uint16_t workgroup_size_y;
  uint16_t workgroup_size_z;
  uint16_t reserved0;
  uint32_t grid_size_x;
  uint32_t grid_size_y;
  uint32_t grid_size_z;
  uint32_t private_segment_size;
  uint32_t group_segment_size;
  uint64_t kernel_object;
  void *kernarg_address;
  uint64_t reserved2;
  uint64_t completion_signal;
};

/* HSA queue is shadow structure, originally provided by AMD.  */

struct hsa_queue
{
  int type;
  uint32_t features;
  void *base_address;
  uint64_t doorbell_signal;
  uint32_t size;
  uint32_t reserved1;
  uint64_t id;
};

/* Alloc pools for allocating basic hsa structures such as operands,
   instructions and other basic entities.  */
static object_allocator<hsa_op_address> *hsa_allocp_operand_address;
static object_allocator<hsa_op_immed> *hsa_allocp_operand_immed;
static object_allocator<hsa_op_reg> *hsa_allocp_operand_reg;
static object_allocator<hsa_op_code_list> *hsa_allocp_operand_code_list;
static object_allocator<hsa_op_operand_list> *hsa_allocp_operand_operand_list;
static object_allocator<hsa_insn_basic> *hsa_allocp_inst_basic;
static object_allocator<hsa_insn_phi> *hsa_allocp_inst_phi;
static object_allocator<hsa_insn_mem> *hsa_allocp_inst_mem;
static object_allocator<hsa_insn_atomic> *hsa_allocp_inst_atomic;
static object_allocator<hsa_insn_signal> *hsa_allocp_inst_signal;
static object_allocator<hsa_insn_seg> *hsa_allocp_inst_seg;
static object_allocator<hsa_insn_cmp> *hsa_allocp_inst_cmp;
static object_allocator<hsa_insn_br> *hsa_allocp_inst_br;
static object_allocator<hsa_insn_sbr> *hsa_allocp_inst_sbr;
static object_allocator<hsa_insn_call> *hsa_allocp_inst_call;
static object_allocator<hsa_insn_arg_block> *hsa_allocp_inst_arg_block;
static object_allocator<hsa_insn_comment> *hsa_allocp_inst_comment;
static object_allocator<hsa_insn_queue> *hsa_allocp_inst_queue;
static object_allocator<hsa_insn_srctype> *hsa_allocp_inst_srctype;
static object_allocator<hsa_insn_packed> *hsa_allocp_inst_packed;
static object_allocator<hsa_insn_cvt> *hsa_allocp_inst_cvt;
static object_allocator<hsa_insn_alloca> *hsa_allocp_inst_alloca;
static object_allocator<hsa_bb> *hsa_allocp_bb;

/* List of pointers to all instructions that come from an object allocator.  */
static vec <hsa_insn_basic *> hsa_instructions;

/* List of pointers to all operands that come from an object allocator.  */
static vec <hsa_op_base *> hsa_operands;

hsa_symbol::hsa_symbol ()
  : m_decl (NULL_TREE), m_name (NULL), m_name_number (0),
    m_directive_offset (0), m_type (BRIG_TYPE_NONE),
    m_segment (BRIG_SEGMENT_NONE), m_linkage (BRIG_LINKAGE_NONE), m_dim (0),
    m_cst_value (NULL), m_global_scope_p (false), m_seen_error (false),
    m_allocation (BRIG_ALLOCATION_AUTOMATIC)
{
}


hsa_symbol::hsa_symbol (BrigType16_t type, BrigSegment8_t segment,
			BrigLinkage8_t linkage, bool global_scope_p,
			BrigAllocation allocation)
  : m_decl (NULL_TREE), m_name (NULL), m_name_number (0),
    m_directive_offset (0), m_type (type), m_segment (segment),
    m_linkage (linkage), m_dim (0), m_cst_value (NULL),
    m_global_scope_p (global_scope_p), m_seen_error (false),
    m_allocation (allocation)
{
}

unsigned HOST_WIDE_INT
hsa_symbol::total_byte_size ()
{
  unsigned HOST_WIDE_INT s
    = hsa_type_bit_size (~BRIG_TYPE_ARRAY_MASK & m_type);
  gcc_assert (s % BITS_PER_UNIT == 0);
  s /= BITS_PER_UNIT;

  if (m_dim)
    s *= m_dim;

  return s;
}

/* Forward declaration.  */

static BrigType16_t
hsa_type_for_tree_type (const_tree type, unsigned HOST_WIDE_INT *dim_p,
			bool min32int);

void
hsa_symbol::fillup_for_decl (tree decl)
{
  m_decl = decl;
  m_type = hsa_type_for_tree_type (TREE_TYPE (decl), &m_dim, false);

  if (hsa_seen_error ())
    m_seen_error = true;
}

/* Constructor of class representing global HSA function/kernel information and
   state.  FNDECL is function declaration, KERNEL_P is true if the function
   is going to become a HSA kernel.  If the function has body, SSA_NAMES_COUNT
   should be set to number of SSA names used in the function.  */

hsa_function_representation::hsa_function_representation
  (tree fdecl, bool kernel_p, unsigned ssa_names_count)
  : m_name (NULL),
    m_reg_count (0), m_input_args (vNULL),
    m_output_arg (NULL), m_spill_symbols (vNULL), m_global_symbols (vNULL),
    m_private_variables (vNULL), m_called_functions (vNULL),
    m_called_internal_fns (vNULL), m_hbb_count (0),
    m_in_ssa (true), m_kern_p (kernel_p), m_declaration_p (false),
    m_decl (fdecl), m_internal_fn (NULL), m_shadow_reg (NULL),
    m_kernel_dispatch_count (0), m_maximum_omp_data_size (0),
    m_seen_error (false), m_temp_symbol_count (0), m_ssa_map ()
{
  int sym_init_len = (vec_safe_length (cfun->local_decls) / 2) + 1;;
  m_local_symbols = new hash_table <hsa_noop_symbol_hasher> (sym_init_len);
  m_ssa_map.safe_grow_cleared (ssa_names_count);
}

/* Constructor of class representing HSA function information that
   is derived for an internal function.  */
hsa_function_representation::hsa_function_representation (hsa_internal_fn *fn)
  : m_reg_count (0), m_input_args (vNULL),
    m_output_arg (NULL), m_local_symbols (NULL),
    m_spill_symbols (vNULL), m_global_symbols (vNULL),
    m_private_variables (vNULL), m_called_functions (vNULL),
    m_called_internal_fns (vNULL), m_hbb_count (0),
    m_in_ssa (true), m_kern_p (false), m_declaration_p (true), m_decl (NULL),
    m_internal_fn (fn), m_shadow_reg (NULL), m_kernel_dispatch_count (0),
    m_maximum_omp_data_size (0), m_seen_error (false), m_temp_symbol_count (0),
    m_ssa_map () {}

/* Destructor of class holding function/kernel-wide information and state.  */

hsa_function_representation::~hsa_function_representation ()
{
  /* Kernel names are deallocated at the end of BRIG output when deallocating
     hsa_decl_kernel_mapping.  */
  if (!m_kern_p || m_seen_error)
    free (m_name);

  for (unsigned i = 0; i < m_input_args.length (); i++)
    delete m_input_args[i];
  m_input_args.release ();

  delete m_output_arg;
  delete m_local_symbols;

  for (unsigned i = 0; i < m_spill_symbols.length (); i++)
    delete m_spill_symbols[i];
  m_spill_symbols.release ();

  hsa_symbol *sym;
  for (unsigned i = 0; i < m_global_symbols.iterate (i, &sym); i++)
    if (sym->m_linkage != BRIG_ALLOCATION_PROGRAM)
      delete sym;
  m_global_symbols.release ();

  for (unsigned i = 0; i < m_private_variables.length (); i++)
    delete m_private_variables[i];
  m_private_variables.release ();
  m_called_functions.release ();
  m_ssa_map.release ();

  for (unsigned i = 0; i < m_called_internal_fns.length (); i++)
    delete m_called_internal_fns[i];
}

hsa_op_reg *
hsa_function_representation::get_shadow_reg ()
{
  /* If we compile a function with kernel dispatch and does not set
     an optimization level, the function won't be inlined and
     we return NULL.  */
  if (!m_kern_p)
    return NULL;

  if (m_shadow_reg)
    return m_shadow_reg;

  /* Append the shadow argument.  */
  hsa_symbol *shadow = new hsa_symbol (BRIG_TYPE_U64, BRIG_SEGMENT_KERNARG,
				       BRIG_LINKAGE_FUNCTION);
  m_input_args.safe_push (shadow);
  shadow->m_name = "hsa_runtime_shadow";

  hsa_op_reg *r = new hsa_op_reg (BRIG_TYPE_U64);
  hsa_op_address *addr = new hsa_op_address (shadow);

  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_LD, BRIG_TYPE_U64, r, addr);
  hsa_bb_for_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun))->append_insn (mem);
  m_shadow_reg = r;

  return r;
}

bool hsa_function_representation::has_shadow_reg_p ()
{
  return m_shadow_reg != NULL;
}

void
hsa_function_representation::init_extra_bbs ()
{
  hsa_init_new_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  hsa_init_new_bb (EXIT_BLOCK_PTR_FOR_FN (cfun));
}

hsa_symbol *
hsa_function_representation::create_hsa_temporary (BrigType16_t type)
{
  hsa_symbol *s = new hsa_symbol (type, BRIG_SEGMENT_PRIVATE,
				  BRIG_LINKAGE_FUNCTION);
  s->m_name_number = m_temp_symbol_count++;

  hsa_cfun->m_private_variables.safe_push (s);
  return s;
}

BrigLinkage8_t
hsa_function_representation::get_linkage ()
{
  if (m_internal_fn)
    return BRIG_LINKAGE_PROGRAM;

  return m_kern_p || TREE_PUBLIC (m_decl) ?
    BRIG_LINKAGE_PROGRAM : BRIG_LINKAGE_MODULE;
}

/* Hash map of simple OMP builtins.  */
static hash_map <nofree_string_hash, omp_simple_builtin> *omp_simple_builtins
  = NULL;

/* Warning messages for OMP builtins.  */

#define HSA_WARN_LOCK_ROUTINE "support for HSA does not implement OpenMP " \
  "lock routines"
#define HSA_WARN_TIMING_ROUTINE "support for HSA does not implement OpenMP " \
  "timing routines"
#define HSA_WARN_MEMORY_ROUTINE "OpenMP device memory library routines have " \
  "undefined semantics within target regions, support for HSA ignores them"
#define HSA_WARN_AFFINITY "Support for HSA does not implement OpenMP " \
  "affinity feateres"

/* Initialize hash map with simple OMP builtins.  */

static void
hsa_init_simple_builtins ()
{
  if (omp_simple_builtins != NULL)
    return;

  omp_simple_builtins
    = new hash_map <nofree_string_hash, omp_simple_builtin> ();

  omp_simple_builtin omp_builtins[] =
    {
      omp_simple_builtin ("omp_get_initial_device", NULL, false,
			  new hsa_op_immed (GOMP_DEVICE_HOST,
					    (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_is_initial_device", NULL, false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_dynamic", NULL, false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_set_dynamic", NULL, false, NULL),
      omp_simple_builtin ("omp_init_lock", HSA_WARN_LOCK_ROUTINE, true),
      omp_simple_builtin ("omp_init_lock_with_hint", HSA_WARN_LOCK_ROUTINE,
			  true),
      omp_simple_builtin ("omp_init_nest_lock_with_hint", HSA_WARN_LOCK_ROUTINE,
			  true),
      omp_simple_builtin ("omp_destroy_lock", HSA_WARN_LOCK_ROUTINE, true),
      omp_simple_builtin ("omp_set_lock", HSA_WARN_LOCK_ROUTINE, true),
      omp_simple_builtin ("omp_unset_lock", HSA_WARN_LOCK_ROUTINE, true),
      omp_simple_builtin ("omp_test_lock", HSA_WARN_LOCK_ROUTINE, true),
      omp_simple_builtin ("omp_get_wtime", HSA_WARN_TIMING_ROUTINE, true),
      omp_simple_builtin ("omp_get_wtick", HSA_WARN_TIMING_ROUTINE, true),
      omp_simple_builtin ("omp_target_alloc", HSA_WARN_MEMORY_ROUTINE, false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_U64)),
      omp_simple_builtin ("omp_target_free", HSA_WARN_MEMORY_ROUTINE, false),
      omp_simple_builtin ("omp_target_is_present", HSA_WARN_MEMORY_ROUTINE,
			  false,
			  new hsa_op_immed (-1, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_target_memcpy", HSA_WARN_MEMORY_ROUTINE, false,
			  new hsa_op_immed (-1, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_target_memcpy_rect", HSA_WARN_MEMORY_ROUTINE,
			  false,
			  new hsa_op_immed (-1, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_target_associate_ptr", HSA_WARN_MEMORY_ROUTINE,
			  false,
			  new hsa_op_immed (-1, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_target_disassociate_ptr",
			  HSA_WARN_MEMORY_ROUTINE,
			  false,
			  new hsa_op_immed (-1, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_set_max_active_levels",
			  "Support for HSA only allows only one active level, "
			  "call to omp_set_max_active_levels will be ignored "
			  "in the generated HSAIL",
			  false, NULL),
      omp_simple_builtin ("omp_get_max_active_levels", NULL, false,
			  new hsa_op_immed (1, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_in_final", NULL, false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_proc_bind", HSA_WARN_AFFINITY, false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_num_places", HSA_WARN_AFFINITY, false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_place_num_procs", HSA_WARN_AFFINITY, false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_place_proc_ids", HSA_WARN_AFFINITY, false,
			  NULL),
      omp_simple_builtin ("omp_get_place_num", HSA_WARN_AFFINITY, false,
			  new hsa_op_immed (-1, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_partition_num_places", HSA_WARN_AFFINITY,
			  false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_partition_place_nums", HSA_WARN_AFFINITY,
			  false, NULL),
      omp_simple_builtin ("omp_set_default_device",
			  "omp_set_default_device has undefined semantics "
			  "within target regions, support for HSA ignores it",
			  false, NULL),
      omp_simple_builtin ("omp_get_default_device",
			  "omp_get_default_device has undefined semantics "
			  "within target regions, support for HSA ignores it",
			  false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_num_devices",
			  "omp_get_num_devices has undefined semantics "
			  "within target regions, support for HSA ignores it",
			  false,
			  new hsa_op_immed (0, (BrigType16_t) BRIG_TYPE_S32)),
      omp_simple_builtin ("omp_get_num_procs", NULL, true, NULL),
      omp_simple_builtin ("omp_get_cancellation", NULL, true, NULL),
      omp_simple_builtin ("omp_set_nested", NULL, true, NULL),
      omp_simple_builtin ("omp_get_nested", NULL, true, NULL),
      omp_simple_builtin ("omp_set_schedule", NULL, true, NULL),
      omp_simple_builtin ("omp_get_schedule", NULL, true, NULL),
      omp_simple_builtin ("omp_get_thread_limit", NULL, true, NULL),
      omp_simple_builtin ("omp_get_team_size", NULL, true, NULL),
      omp_simple_builtin ("omp_get_ancestor_thread_num", NULL, true, NULL),
      omp_simple_builtin ("omp_get_max_task_priority", NULL, true, NULL)
    };

  unsigned count = sizeof (omp_builtins) / sizeof (omp_simple_builtin);

  for (unsigned i = 0; i < count; i++)
    omp_simple_builtins->put (omp_builtins[i].m_name, omp_builtins[i]);
}

/* Allocate HSA structures that we need only while generating with this.  */

static void
hsa_init_data_for_cfun ()
{
  hsa_init_compilation_unit_data ();
  hsa_allocp_operand_address
    = new object_allocator<hsa_op_address> ("HSA address operands");
  hsa_allocp_operand_immed
    = new object_allocator<hsa_op_immed> ("HSA immediate operands");
  hsa_allocp_operand_reg
    = new object_allocator<hsa_op_reg> ("HSA register operands");
  hsa_allocp_operand_code_list
    = new object_allocator<hsa_op_code_list> ("HSA code list operands");
  hsa_allocp_operand_operand_list
    = new object_allocator<hsa_op_operand_list> ("HSA operand list operands");
  hsa_allocp_inst_basic
    = new object_allocator<hsa_insn_basic> ("HSA basic instructions");
  hsa_allocp_inst_phi
    = new object_allocator<hsa_insn_phi> ("HSA phi operands");
  hsa_allocp_inst_mem
    = new object_allocator<hsa_insn_mem> ("HSA memory instructions");
  hsa_allocp_inst_atomic
    = new object_allocator<hsa_insn_atomic> ("HSA atomic instructions");
  hsa_allocp_inst_signal
    = new object_allocator<hsa_insn_signal> ("HSA signal instructions");
  hsa_allocp_inst_seg
    = new object_allocator<hsa_insn_seg> ("HSA segment conversion "
					  "instructions");
  hsa_allocp_inst_cmp
    = new object_allocator<hsa_insn_cmp> ("HSA comparison instructions");
  hsa_allocp_inst_br
    = new object_allocator<hsa_insn_br> ("HSA branching instructions");
  hsa_allocp_inst_sbr
    = new object_allocator<hsa_insn_sbr> ("HSA switch branching instructions");
  hsa_allocp_inst_call
    = new object_allocator<hsa_insn_call> ("HSA call instructions");
  hsa_allocp_inst_arg_block
    = new object_allocator<hsa_insn_arg_block> ("HSA arg block instructions");
  hsa_allocp_inst_comment
    = new object_allocator<hsa_insn_comment> ("HSA comment instructions");
  hsa_allocp_inst_queue
    = new object_allocator<hsa_insn_queue> ("HSA queue instructions");
  hsa_allocp_inst_srctype
    = new object_allocator<hsa_insn_srctype> ("HSA source type instructions");
  hsa_allocp_inst_packed
    = new object_allocator<hsa_insn_packed> ("HSA packed instructions");
  hsa_allocp_inst_cvt
    = new object_allocator<hsa_insn_cvt> ("HSA convert instructions");
  hsa_allocp_inst_alloca
    = new object_allocator<hsa_insn_alloca> ("HSA alloca instructions");
  hsa_allocp_bb = new object_allocator<hsa_bb> ("HSA basic blocks");
}

/* Deinitialize HSA subsystem and free all allocated memory.  */

static void
hsa_deinit_data_for_cfun (void)
{
  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    if (bb->aux)
      {
	hsa_bb *hbb = hsa_bb_for_bb (bb);
	hbb->~hsa_bb ();
	bb->aux = NULL;
      }

  for (unsigned int i = 0; i < hsa_operands.length (); i++)
    hsa_destroy_operand (hsa_operands[i]);

  hsa_operands.release ();

  for (unsigned i = 0; i < hsa_instructions.length (); i++)
    hsa_destroy_insn (hsa_instructions[i]);

  hsa_instructions.release ();

  if (omp_simple_builtins != NULL)
    {
      delete omp_simple_builtins;
      omp_simple_builtins = NULL;
    }

  delete hsa_allocp_operand_address;
  delete hsa_allocp_operand_immed;
  delete hsa_allocp_operand_reg;
  delete hsa_allocp_operand_code_list;
  delete hsa_allocp_operand_operand_list;
  delete hsa_allocp_inst_basic;
  delete hsa_allocp_inst_phi;
  delete hsa_allocp_inst_atomic;
  delete hsa_allocp_inst_mem;
  delete hsa_allocp_inst_signal;
  delete hsa_allocp_inst_seg;
  delete hsa_allocp_inst_cmp;
  delete hsa_allocp_inst_br;
  delete hsa_allocp_inst_sbr;
  delete hsa_allocp_inst_call;
  delete hsa_allocp_inst_arg_block;
  delete hsa_allocp_inst_comment;
  delete hsa_allocp_inst_queue;
  delete hsa_allocp_inst_srctype;
  delete hsa_allocp_inst_packed;
  delete hsa_allocp_inst_cvt;
  delete hsa_allocp_inst_alloca;
  delete hsa_allocp_bb;
  delete hsa_cfun;
}

/* Return the type which holds addresses in the given SEGMENT.  */

static BrigType16_t
hsa_get_segment_addr_type (BrigSegment8_t segment)
{
  switch (segment)
    {
    case BRIG_SEGMENT_NONE:
      gcc_unreachable ();

    case BRIG_SEGMENT_FLAT:
    case BRIG_SEGMENT_GLOBAL:
    case BRIG_SEGMENT_READONLY:
    case BRIG_SEGMENT_KERNARG:
      return hsa_machine_large_p () ? BRIG_TYPE_U64 : BRIG_TYPE_U32;

    case BRIG_SEGMENT_GROUP:
    case BRIG_SEGMENT_PRIVATE:
    case BRIG_SEGMENT_SPILL:
    case BRIG_SEGMENT_ARG:
      return BRIG_TYPE_U32;
    }
  gcc_unreachable ();
}

/* Return integer brig type according to provided SIZE in bytes.  If SIGN
   is set to true, return signed integer type.  */

static BrigType16_t
get_integer_type_by_bytes (unsigned size, bool sign)
{
  if (sign)
    switch (size)
      {
      case 1:
	return BRIG_TYPE_S8;
      case 2:
	return BRIG_TYPE_S16;
      case 4:
	return BRIG_TYPE_S32;
      case 8:
	return BRIG_TYPE_S64;
      default:
	break;
      }
  else
    switch (size)
      {
      case 1:
	return BRIG_TYPE_U8;
      case 2:
	return BRIG_TYPE_U16;
      case 4:
	return BRIG_TYPE_U32;
      case 8:
	return BRIG_TYPE_U64;
      default:
	break;
      }

  return 0;
}

/* Return HSA type for tree TYPE, which has to fit into BrigType16_t.  Pointers
   are assumed to use flat addressing.  If min32int is true, always expand
   integer types to one that has at least 32 bits.  */

static BrigType16_t
hsa_type_for_scalar_tree_type (const_tree type, bool min32int)
{
  HOST_WIDE_INT bsize;
  const_tree base;
  BrigType16_t res = BRIG_TYPE_NONE;

  gcc_checking_assert (TYPE_P (type));
  gcc_checking_assert (!AGGREGATE_TYPE_P (type));
  if (POINTER_TYPE_P (type))
    return hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT);

  if (TREE_CODE (type) == VECTOR_TYPE || TREE_CODE (type) == COMPLEX_TYPE)
    base = TREE_TYPE (type);
  else
    base = type;

  if (!tree_fits_uhwi_p (TYPE_SIZE (base)))
    {
      HSA_SORRY_ATV (EXPR_LOCATION (type),
		     "support for HSA does not implement huge or "
		     "variable-sized type %T", type);
      return res;
    }

  bsize = tree_to_uhwi (TYPE_SIZE (base));
  unsigned byte_size = bsize / BITS_PER_UNIT;
  if (INTEGRAL_TYPE_P (base))
    res = get_integer_type_by_bytes (byte_size, !TYPE_UNSIGNED (base));
  else if (SCALAR_FLOAT_TYPE_P (base))
    {
      switch (bsize)
	{
	case 16:
	  res = BRIG_TYPE_F16;
	  break;
	case 32:
	  res = BRIG_TYPE_F32;
	  break;
	case 64:
	  res = BRIG_TYPE_F64;
	  break;
	default:
	  break;
	}
    }

  if (res == BRIG_TYPE_NONE)
    {
      HSA_SORRY_ATV (EXPR_LOCATION (type),
		     "support for HSA does not implement type %T", type);
      return res;
    }

  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      HOST_WIDE_INT tsize = tree_to_uhwi (TYPE_SIZE (type));

      if (bsize == tsize)
	{
	  HSA_SORRY_ATV (EXPR_LOCATION (type),
			 "support for HSA does not implement a vector type "
			 "where a type and unit size are equal: %T", type);
	  return res;
	}

      switch (tsize)
	{
	case 32:
	  res |= BRIG_TYPE_PACK_32;
	  break;
	case 64:
	  res |= BRIG_TYPE_PACK_64;
	  break;
	case 128:
	  res |= BRIG_TYPE_PACK_128;
	  break;
	default:
	  HSA_SORRY_ATV (EXPR_LOCATION (type),
			 "support for HSA does not implement type %T", type);
	}
    }

  if (min32int)
    {
      /* Registers/immediate operands can only be 32bit or more except for
	 f16.  */
      if (res == BRIG_TYPE_U8 || res == BRIG_TYPE_U16)
	res = BRIG_TYPE_U32;
      else if (res == BRIG_TYPE_S8 || res == BRIG_TYPE_S16)
	res = BRIG_TYPE_S32;
    }

  if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      unsigned bsize = 2 * hsa_type_bit_size (res);
      res = hsa_bittype_for_bitsize (bsize);
    }

  return res;
}

/* Returns the BRIG type we need to load/store entities of TYPE.  */

static BrigType16_t
mem_type_for_type (BrigType16_t type)
{
  /* HSA has non-intuitive constraints on load/store types.  If it's
     a bit-type it _must_ be B128, if it's not a bit-type it must be
     64bit max.  So for loading entities of 128 bits (e.g. vectors)
     we have to to B128, while for loading the rest we have to use the
     input type (??? or maybe also flattened to a equally sized non-vector
     unsigned type?).  */
  if ((type & BRIG_TYPE_PACK_MASK) == BRIG_TYPE_PACK_128)
    return BRIG_TYPE_B128;
  else if (hsa_btype_p (type) || hsa_type_packed_p (type))
    {
      unsigned bitsize = hsa_type_bit_size (type);
      if (bitsize < 128)
	return hsa_uint_for_bitsize (bitsize);
      else
	return hsa_bittype_for_bitsize (bitsize);
    }
  return type;
}

/* Return HSA type for tree TYPE.  If it cannot fit into BrigType16_t, some
   kind of array will be generated, setting DIM appropriately.  Otherwise, it
   will be set to zero.  */

static BrigType16_t
hsa_type_for_tree_type (const_tree type, unsigned HOST_WIDE_INT *dim_p = NULL,
			bool min32int = false)
{
  gcc_checking_assert (TYPE_P (type));
  if (!tree_fits_uhwi_p (TYPE_SIZE_UNIT (type)))
    {
      HSA_SORRY_ATV (EXPR_LOCATION (type), "support for HSA does not "
		     "implement huge or variable-sized type %T", type);
      return BRIG_TYPE_NONE;
    }

  if (RECORD_OR_UNION_TYPE_P (type))
    {
      if (dim_p)
	*dim_p = tree_to_uhwi (TYPE_SIZE_UNIT (type));
      return BRIG_TYPE_U8 | BRIG_TYPE_ARRAY;
    }

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* We try to be nice and use the real base-type when this is an array of
	 scalars and only resort to an array of bytes if the type is more
	 complex.  */

      unsigned HOST_WIDE_INT dim = 1;

      while (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree domain = TYPE_DOMAIN (type);
	  if (!TYPE_MIN_VALUE (domain)
	      || !TYPE_MAX_VALUE (domain)
	      || !tree_fits_shwi_p (TYPE_MIN_VALUE (domain))
	      || !tree_fits_shwi_p (TYPE_MAX_VALUE (domain)))
	    {
	      HSA_SORRY_ATV (EXPR_LOCATION (type),
			     "support for HSA does not implement array %T with "
			     "unknown bounds", type);
	      return BRIG_TYPE_NONE;
	    }
	  HOST_WIDE_INT min = tree_to_shwi (TYPE_MIN_VALUE (domain));
	  HOST_WIDE_INT max = tree_to_shwi (TYPE_MAX_VALUE (domain));
	  dim = dim * (unsigned HOST_WIDE_INT) (max - min + 1);
	  type = TREE_TYPE (type);
	}

      BrigType16_t res;
      if (RECORD_OR_UNION_TYPE_P (type))
	{
	  dim = dim * tree_to_uhwi (TYPE_SIZE_UNIT (type));
	  res = BRIG_TYPE_U8;
	}
      else
	res = hsa_type_for_scalar_tree_type (type, false);

      if (dim_p)
	*dim_p = dim;
      return res | BRIG_TYPE_ARRAY;
    }

  /* Scalar case: */
  if (dim_p)
    *dim_p = 0;

  return hsa_type_for_scalar_tree_type (type, min32int);
}

/* Returns true if converting from STYPE into DTYPE needs the _CVT
   opcode.  If false a normal _MOV is enough.  */

static bool
hsa_needs_cvt (BrigType16_t dtype, BrigType16_t stype)
{
  if (hsa_btype_p (dtype))
    return false;

  /* float <-> int conversions are real converts.  */
  if (hsa_type_float_p (dtype) != hsa_type_float_p (stype))
    return true;
  /* When both types have different size, then we need CVT as well.  */
  if (hsa_type_bit_size (dtype) != hsa_type_bit_size (stype))
    return true;
  return false;
}

/* Lookup or create the associated hsa_symbol structure with a given VAR_DECL
   or lookup the hsa_structure corresponding to a PARM_DECL.  */

static hsa_symbol *
get_symbol_for_decl (tree decl)
{
  hsa_symbol **slot;
  hsa_symbol dummy (BRIG_TYPE_NONE, BRIG_SEGMENT_NONE, BRIG_LINKAGE_NONE);

  gcc_assert (TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL
	      || TREE_CODE (decl) == VAR_DECL);

  dummy.m_decl = decl;

  bool is_in_global_vars
    = TREE_CODE (decl) == VAR_DECL && is_global_var (decl);

  if (is_in_global_vars)
    slot = hsa_global_variable_symbols->find_slot (&dummy, INSERT);
  else
    slot = hsa_cfun->m_local_symbols->find_slot (&dummy, INSERT);

  gcc_checking_assert (slot);
  if (*slot)
    {
      /* If the symbol is problematic, mark current function also as
	 problematic.  */
      if ((*slot)->m_seen_error)
	hsa_fail_cfun ();

      return *slot;
    }
  else
    {
      hsa_symbol *sym;
      gcc_assert (TREE_CODE (decl) == VAR_DECL);

      if (is_in_global_vars)
	{
	  sym = new hsa_symbol (BRIG_TYPE_NONE, BRIG_SEGMENT_GLOBAL,
				BRIG_LINKAGE_PROGRAM, true,
				BRIG_ALLOCATION_PROGRAM);
	  hsa_cfun->m_global_symbols.safe_push (sym);
	}
      else
	{
	  /* PARM_DECL and RESULT_DECL should be already in m_local_symbols.  */
	  gcc_assert (TREE_CODE (decl) == VAR_DECL);

	  sym = new hsa_symbol (BRIG_TYPE_NONE, BRIG_SEGMENT_PRIVATE,
				BRIG_LINKAGE_FUNCTION);
	  hsa_cfun->m_private_variables.safe_push (sym);
	}

      sym->fillup_for_decl (decl);
      sym->m_name = hsa_get_declaration_name (decl);

      *slot = sym;
      return sym;
    }
}

/* For a given HSA function declaration, return a host
   function declaration.  */

tree
hsa_get_host_function (tree decl)
{
  hsa_function_summary *s
    = hsa_summaries->get (cgraph_node::get_create (decl));
  gcc_assert (s->m_kind != HSA_NONE);
  gcc_assert (s->m_gpu_implementation_p);

  return s->m_binded_function->decl;
}

/* Return true if function DECL has a host equivalent function.  */

static char *
get_brig_function_name (tree decl)
{
  tree d = decl;

  hsa_function_summary *s = hsa_summaries->get (cgraph_node::get_create (d));
  if (s->m_kind != HSA_NONE && s->m_gpu_implementation_p)
    d = s->m_binded_function->decl;

  /* IPA split can create a function that has no host equivalent.  */
  if (d == NULL)
    d = decl;

  char *name = xstrdup (hsa_get_declaration_name (d));
  hsa_sanitize_name (name);

  return name;
}

/* Create a spill symbol of type TYPE.  */

hsa_symbol *
hsa_get_spill_symbol (BrigType16_t type)
{
  hsa_symbol *sym = new hsa_symbol (type, BRIG_SEGMENT_SPILL,
				    BRIG_LINKAGE_FUNCTION);
  hsa_cfun->m_spill_symbols.safe_push (sym);
  return sym;
}

/* Create a symbol for a read-only string constant.  */
hsa_symbol *
hsa_get_string_cst_symbol (tree string_cst)
{
  gcc_checking_assert (TREE_CODE (string_cst) == STRING_CST);

  hsa_symbol **slot = hsa_cfun->m_string_constants_map.get (string_cst);
  if (slot)
    return *slot;

  hsa_op_immed *cst = new hsa_op_immed (string_cst);
  hsa_symbol *sym = new hsa_symbol (cst->m_type, BRIG_SEGMENT_GLOBAL,
				    BRIG_LINKAGE_MODULE, true,
				    BRIG_ALLOCATION_AGENT);
  sym->m_cst_value = cst;
  sym->m_dim = TREE_STRING_LENGTH (string_cst);
  sym->m_name_number = hsa_cfun->m_global_symbols.length ();

  hsa_cfun->m_global_symbols.safe_push (sym);
  hsa_cfun->m_string_constants_map.put (string_cst, sym);
  return sym;
}

/* Constructor of the ancestor of all operands.  K is BRIG kind that identified
   what the operator is.  */

hsa_op_base::hsa_op_base (BrigKind16_t k)
  : m_next (NULL), m_brig_op_offset (0), m_kind (k)
{
  hsa_operands.safe_push (this);
}

/* Constructor of ancestor of all operands which have a type.  K is BRIG kind
   that identified what the operator is.  T is the type of the operator.  */

hsa_op_with_type::hsa_op_with_type (BrigKind16_t k, BrigType16_t t)
  : hsa_op_base (k), m_type (t)
{
}

hsa_op_with_type *
hsa_op_with_type::get_in_type (BrigType16_t dtype, hsa_bb *hbb)
{
  if (m_type == dtype)
    return this;

  hsa_op_reg *dest;

  if (hsa_needs_cvt (dtype, m_type))
    {
      dest = new hsa_op_reg (dtype);
      hbb->append_insn (new hsa_insn_cvt (dest, this));
    }
  else
    {
      dest = new hsa_op_reg (m_type);
      hbb->append_insn (new hsa_insn_basic (2, BRIG_OPCODE_MOV,
					    dest->m_type, dest, this));

      /* We cannot simply for instance: 'mov_u32 $_3, 48 (s32)' because
	 type of the operand must be same as type of the instruction.  */
      dest->m_type = dtype;
    }

  return dest;
}

/* Constructor of class representing HSA immediate values.  TREE_VAL is the
   tree representation of the immediate value.  If min32int is true,
   always expand integer types to one that has at least 32 bits.  */

hsa_op_immed::hsa_op_immed (tree tree_val, bool min32int)
  : hsa_op_with_type (BRIG_KIND_OPERAND_CONSTANT_BYTES,
		      hsa_type_for_tree_type (TREE_TYPE (tree_val), NULL,
					      min32int)),
  m_brig_repr (NULL)
{
  if (hsa_seen_error ())
    return;

  gcc_checking_assert ((is_gimple_min_invariant (tree_val)
		       && (!POINTER_TYPE_P (TREE_TYPE (tree_val))
			   || TREE_CODE (tree_val) == INTEGER_CST))
		       || TREE_CODE (tree_val) == CONSTRUCTOR);
  m_tree_value = tree_val;
  m_brig_repr_size = hsa_get_imm_brig_type_len (m_type);

  if (TREE_CODE (m_tree_value) == STRING_CST)
    m_brig_repr_size = TREE_STRING_LENGTH (m_tree_value);
  else if (TREE_CODE (m_tree_value) == CONSTRUCTOR)
    {
      m_brig_repr_size
	= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (m_tree_value)));

      /* Verify that all elements of a constructor are constants.  */
      for (unsigned i = 0;
	   i < vec_safe_length (CONSTRUCTOR_ELTS (m_tree_value)); i++)
	{
	  tree v = CONSTRUCTOR_ELT (m_tree_value, i)->value;
	  if (!CONSTANT_CLASS_P (v))
	    {
	      HSA_SORRY_AT (EXPR_LOCATION (tree_val),
			    "HSA ctor should have only constants");
	      return;
	    }
	}
    }

  emit_to_buffer (m_tree_value);
}

/* Constructor of class representing HSA immediate values.  INTEGER_VALUE is the
   integer representation of the immediate value.  TYPE is BRIG type.  */

hsa_op_immed::hsa_op_immed (HOST_WIDE_INT integer_value, BrigType16_t type)
  : hsa_op_with_type (BRIG_KIND_OPERAND_CONSTANT_BYTES, type),
    m_tree_value (NULL), m_brig_repr (NULL)
{
  gcc_assert (hsa_type_integer_p (type));
  m_int_value = integer_value;
  m_brig_repr_size = hsa_type_bit_size (type) / BITS_PER_UNIT;

  hsa_bytes bytes;

  switch (m_brig_repr_size)
    {
    case 1:
      bytes.b8 = (uint8_t) m_int_value;
      break;
    case 2:
      bytes.b16 = (uint16_t) m_int_value;
      break;
    case 4:
      bytes.b32 = (uint32_t) m_int_value;
      break;
    case 8:
      bytes.b64 = (uint64_t) m_int_value;
      break;
    default:
      gcc_unreachable ();
    }

  m_brig_repr = XNEWVEC (char, m_brig_repr_size);
  memcpy (m_brig_repr, &bytes, m_brig_repr_size);
}

hsa_op_immed::hsa_op_immed ()
  : hsa_op_with_type (BRIG_KIND_NONE, BRIG_TYPE_NONE), m_brig_repr (NULL)
{
}

/* New operator to allocate immediate operands from pool alloc.  */

void *
hsa_op_immed::operator new (size_t)
{
  return hsa_allocp_operand_immed->allocate_raw ();
}

/* Destructor.  */

hsa_op_immed::~hsa_op_immed ()
{
  free (m_brig_repr);
}

/* Change type of the immediate value to T.  */

void
hsa_op_immed::set_type (BrigType16_t t)
{
  m_type = t;
}

/* Constructor of class representing HSA registers and pseudo-registers.  T is
   the BRIG type of the new register.  */

hsa_op_reg::hsa_op_reg (BrigType16_t t)
  : hsa_op_with_type (BRIG_KIND_OPERAND_REGISTER, t), m_gimple_ssa (NULL_TREE),
    m_def_insn (NULL), m_spill_sym (NULL), m_order (hsa_cfun->m_reg_count++),
    m_lr_begin (0), m_lr_end (0), m_reg_class (0), m_hard_num (0)
{
}

/* New operator to allocate a register from pool alloc.  */

void *
hsa_op_reg::operator new (size_t)
{
  return hsa_allocp_operand_reg->allocate_raw ();
}

/* Verify register operand.  */

void
hsa_op_reg::verify_ssa ()
{
  /* Verify that each HSA register has a definition assigned.
     Exceptions are VAR_DECL and PARM_DECL that are a default
     definition.  */
  gcc_checking_assert (m_def_insn
		       || (m_gimple_ssa != NULL
			   && (!SSA_NAME_VAR (m_gimple_ssa)
			       || (TREE_CODE (SSA_NAME_VAR (m_gimple_ssa))
				   != PARM_DECL))
			   && SSA_NAME_IS_DEFAULT_DEF (m_gimple_ssa)));

  /* Verify that every use of the register is really present
     in an instruction.  */
  for (unsigned i = 0; i < m_uses.length (); i++)
    {
      hsa_insn_basic *use = m_uses[i];

      bool is_visited = false;
      for (unsigned j = 0; j < use->operand_count (); j++)
	{
	  hsa_op_base *u = use->get_op (j);
	  hsa_op_address *addr; addr = dyn_cast <hsa_op_address *> (u);
	  if (addr && addr->m_reg)
	    u = addr->m_reg;

	  if (u == this)
	    {
	      bool r = !addr && use->op_output_p (j);

	      if (r)
		{
		  error ("HSA SSA name defined by instruction that is supposed "
			 "to be using it");
		  debug_hsa_operand (this);
		  debug_hsa_insn (use);
		  internal_error ("HSA SSA verification failed");
		}

	      is_visited = true;
	    }
	}

      if (!is_visited)
	{
	  error ("HSA SSA name not among operands of instruction that is "
		 "supposed to use it");
	  debug_hsa_operand (this);
	  debug_hsa_insn (use);
	  internal_error ("HSA SSA verification failed");
	}
    }
}

hsa_op_address::hsa_op_address (hsa_symbol *sym, hsa_op_reg *r,
				HOST_WIDE_INT offset)
  : hsa_op_base (BRIG_KIND_OPERAND_ADDRESS), m_symbol (sym), m_reg (r),
    m_imm_offset (offset)
{
}

hsa_op_address::hsa_op_address (hsa_symbol *sym, HOST_WIDE_INT offset)
  : hsa_op_base (BRIG_KIND_OPERAND_ADDRESS), m_symbol (sym), m_reg (NULL),
    m_imm_offset (offset)
{
}

hsa_op_address::hsa_op_address (hsa_op_reg *r, HOST_WIDE_INT offset)
  : hsa_op_base (BRIG_KIND_OPERAND_ADDRESS), m_symbol (NULL), m_reg (r),
    m_imm_offset (offset)
{
}

/* New operator to allocate address operands from pool alloc.  */

void *
hsa_op_address::operator new (size_t)
{
  return hsa_allocp_operand_address->allocate_raw ();
}

/* Constructor of an operand referring to HSAIL code.  */

hsa_op_code_ref::hsa_op_code_ref () : hsa_op_base (BRIG_KIND_OPERAND_CODE_REF),
  m_directive_offset (0)
{
}

/* Constructor of an operand representing a code list.  Set it up so that it
   can contain ELEMENTS number of elements.  */

hsa_op_code_list::hsa_op_code_list (unsigned elements)
  : hsa_op_base (BRIG_KIND_OPERAND_CODE_LIST)
{
  m_offsets.create (1);
  m_offsets.safe_grow_cleared (elements);
}

/* New operator to allocate code list operands from pool alloc.  */

void *
hsa_op_code_list::operator new (size_t)
{
  return hsa_allocp_operand_code_list->allocate_raw ();
}

/* Constructor of an operand representing an operand list.
   Set it up so that it can contain ELEMENTS number of elements.  */

hsa_op_operand_list::hsa_op_operand_list (unsigned elements)
  : hsa_op_base (BRIG_KIND_OPERAND_OPERAND_LIST)
{
  m_offsets.create (elements);
  m_offsets.safe_grow (elements);
}

/* New operator to allocate operand list operands from pool alloc.  */

void *
hsa_op_operand_list::operator new (size_t)
{
  return hsa_allocp_operand_operand_list->allocate_raw ();
}

hsa_op_operand_list::~hsa_op_operand_list ()
{
  m_offsets.release ();
}


hsa_op_reg *
hsa_function_representation::reg_for_gimple_ssa (tree ssa)
{
  hsa_op_reg *hreg;

  gcc_checking_assert (TREE_CODE (ssa) == SSA_NAME);
  if (m_ssa_map[SSA_NAME_VERSION (ssa)])
    return m_ssa_map[SSA_NAME_VERSION (ssa)];

  hreg = new hsa_op_reg (hsa_type_for_scalar_tree_type (TREE_TYPE (ssa),
							 true));
  hreg->m_gimple_ssa = ssa;
  m_ssa_map[SSA_NAME_VERSION (ssa)] = hreg;

  return hreg;
}

void
hsa_op_reg::set_definition (hsa_insn_basic *insn)
{
  if (hsa_cfun->m_in_ssa)
    {
      gcc_checking_assert (!m_def_insn);
      m_def_insn = insn;
    }
  else
    m_def_insn = NULL;
}

/* Constructor of the class which is the bases of all instructions and directly
   represents the most basic ones.  NOPS is the number of operands that the
   operand vector will contain (and which will be cleared).  OP is the opcode
   of the instruction.  This constructor does not set type.  */

hsa_insn_basic::hsa_insn_basic (unsigned nops, int opc)
  : m_prev (NULL),
    m_next (NULL), m_bb (NULL), m_opcode (opc), m_number (0),
    m_type (BRIG_TYPE_NONE), m_brig_offset (0)
{
  if (nops > 0)
    m_operands.safe_grow_cleared (nops);

  hsa_instructions.safe_push (this);
}

/* Make OP the operand number INDEX of operands of this instruction.  If OP is a
   register or an address containing a register, then either set the definition
   of the register to this instruction if it an output operand or add this
   instruction to the uses if it is an input one.  */

void
hsa_insn_basic::set_op (int index, hsa_op_base *op)
{
  /* Each address operand is always use.  */
  hsa_op_address *addr = dyn_cast <hsa_op_address *> (op);
  if (addr && addr->m_reg)
    addr->m_reg->m_uses.safe_push (this);
  else
    {
      hsa_op_reg *reg = dyn_cast <hsa_op_reg *> (op);
      if (reg)
	{
	  if (op_output_p (index))
	    reg->set_definition (this);
	  else
	    reg->m_uses.safe_push (this);
	}
    }

  m_operands[index] = op;
}

/* Get INDEX-th operand of the instruction.  */

hsa_op_base *
hsa_insn_basic::get_op (int index)
{
  return m_operands[index];
}

/* Get address of INDEX-th operand of the instruction.  */

hsa_op_base **
hsa_insn_basic::get_op_addr (int index)
{
  return &m_operands[index];
}

/* Get number of operands of the instruction.  */
unsigned int
hsa_insn_basic::operand_count ()
{
  return m_operands.length ();
}

/* Constructor of the class which is the bases of all instructions and directly
   represents the most basic ones.  NOPS is the number of operands that the
   operand vector will contain (and which will be cleared).  OPC is the opcode
   of the instruction, T is the type of the instruction.  */

hsa_insn_basic::hsa_insn_basic (unsigned nops, int opc, BrigType16_t t,
				hsa_op_base *arg0, hsa_op_base *arg1,
				hsa_op_base *arg2, hsa_op_base *arg3)
 : m_prev (NULL), m_next (NULL), m_bb (NULL), m_opcode (opc),m_number (0),
   m_type (t),  m_brig_offset (0)
{
  if (nops > 0)
    m_operands.safe_grow_cleared (nops);

  if (arg0 != NULL)
    {
      gcc_checking_assert (nops >= 1);
      set_op (0, arg0);
    }

  if (arg1 != NULL)
    {
      gcc_checking_assert (nops >= 2);
      set_op (1, arg1);
    }

  if (arg2 != NULL)
    {
      gcc_checking_assert (nops >= 3);
      set_op (2, arg2);
    }

  if (arg3 != NULL)
    {
      gcc_checking_assert (nops >= 4);
      set_op (3, arg3);
    }

  hsa_instructions.safe_push (this);
}

/* New operator to allocate basic instruction from pool alloc.  */

void *
hsa_insn_basic::operator new (size_t)
{
  return hsa_allocp_inst_basic->allocate_raw ();
}

/* Verify the instruction.  */

void
hsa_insn_basic::verify ()
{
  hsa_op_address *addr;
  hsa_op_reg *reg;

  /* Iterate all register operands and verify that the instruction
     is set in uses of the register.  */
  for (unsigned i = 0; i < operand_count (); i++)
    {
      hsa_op_base *use = get_op (i);

      if ((addr = dyn_cast <hsa_op_address *> (use)) && addr->m_reg)
	{
	  gcc_assert (addr->m_reg->m_def_insn != this);
	  use = addr->m_reg;
	}

      if ((reg = dyn_cast <hsa_op_reg *> (use)) && !op_output_p (i))
	{
	  unsigned j;
	  for (j = 0; j < reg->m_uses.length (); j++)
	    {
	      if (reg->m_uses[j] == this)
		break;
	    }

	  if (j == reg->m_uses.length ())
	    {
	      error ("HSA instruction uses a register but is not among "
		     "recorded register uses");
	      debug_hsa_operand (reg);
	      debug_hsa_insn (this);
	      internal_error ("HSA instruction verification failed");
	    }
	}
    }
}

/* Constructor of an instruction representing a PHI node.  NOPS is the number
   of operands (equal to the number of predecessors).  */

hsa_insn_phi::hsa_insn_phi (unsigned nops, hsa_op_reg *dst)
  : hsa_insn_basic (nops, HSA_OPCODE_PHI), m_dest (dst)
{
  dst->set_definition (this);
}

/* New operator to allocate PHI instruction from pool alloc.  */

void *
hsa_insn_phi::operator new (size_t)
{
  return hsa_allocp_inst_phi->allocate_raw ();
}

/* Constructor of class representing instruction for conditional jump, CTRL is
   the control register determining whether the jump will be carried out, the
   new instruction is automatically added to its uses list.  */

hsa_insn_br::hsa_insn_br (hsa_op_reg *ctrl)
  : hsa_insn_basic (1, BRIG_OPCODE_CBR, BRIG_TYPE_B1, ctrl),
    m_width (BRIG_WIDTH_1)
{
}

/* New operator to allocate branch instruction from pool alloc.  */

void *
hsa_insn_br::operator new (size_t)
{
  return hsa_allocp_inst_br->allocate_raw ();
}

/* Constructor of class representing instruction for switch jump, CTRL is
   the index register.  */

hsa_insn_sbr::hsa_insn_sbr (hsa_op_reg *index, unsigned jump_count)
  : hsa_insn_basic (1, BRIG_OPCODE_SBR, BRIG_TYPE_B1, index),
    m_width (BRIG_WIDTH_1), m_jump_table (vNULL), m_default_bb (NULL),
    m_label_code_list (new hsa_op_code_list (jump_count))
{
}

/* New operator to allocate switch branch instruction from pool alloc.  */

void *
hsa_insn_sbr::operator new (size_t)
{
  return hsa_allocp_inst_sbr->allocate_raw ();
}

/* Replace all occurrences of OLD_BB with NEW_BB in the statements
   jump table.  */

void
hsa_insn_sbr::replace_all_labels (basic_block old_bb, basic_block new_bb)
{
  for (unsigned i = 0; i < m_jump_table.length (); i++)
    if (m_jump_table[i] == old_bb)
      m_jump_table[i] = new_bb;
}

hsa_insn_sbr::~hsa_insn_sbr ()
{
  m_jump_table.release ();
}

/* Constructor of comparison instruction.  CMP is the comparison operation and T
   is the result type.  */

hsa_insn_cmp::hsa_insn_cmp (BrigCompareOperation8_t cmp, BrigType16_t t,
			    hsa_op_base *arg0, hsa_op_base *arg1,
			    hsa_op_base *arg2)
  : hsa_insn_basic (3 , BRIG_OPCODE_CMP, t, arg0, arg1, arg2), m_compare (cmp)
{
}

/* New operator to allocate compare instruction from pool alloc.  */

void *
hsa_insn_cmp::operator new (size_t)
{
  return hsa_allocp_inst_cmp->allocate_raw ();
}

/* Constructor of classes representing memory accesses.  OPC is the opcode (must
   be BRIG_OPCODE_ST or BRIG_OPCODE_LD) and T is the type.  The instruction
   operands are provided as ARG0 and ARG1.  */

hsa_insn_mem::hsa_insn_mem (int opc, BrigType16_t t, hsa_op_base *arg0,
			    hsa_op_base *arg1)
  : hsa_insn_basic (2, opc, t, arg0, arg1),
    m_align (hsa_natural_alignment (t)), m_equiv_class (0)
{
  gcc_checking_assert (opc == BRIG_OPCODE_LD || opc == BRIG_OPCODE_ST);
}

/* Constructor for descendants allowing different opcodes and number of
   operands, it passes its arguments directly to hsa_insn_basic
   constructor.  The instruction operands are provided as ARG[0-3].  */


hsa_insn_mem::hsa_insn_mem (unsigned nops, int opc, BrigType16_t t,
			    hsa_op_base *arg0, hsa_op_base *arg1,
			    hsa_op_base *arg2, hsa_op_base *arg3)
  : hsa_insn_basic (nops, opc, t, arg0, arg1, arg2, arg3),
    m_align (hsa_natural_alignment (t)), m_equiv_class (0)
{
}

/* New operator to allocate memory instruction from pool alloc.  */

void *
hsa_insn_mem::operator new (size_t)
{
  return hsa_allocp_inst_mem->allocate_raw ();
}

/* Constructor of class representing atomic instructions and signals.  OPC is
   the principal opcode, aop is the specific atomic operation opcode.  T is the
   type of the instruction.  The instruction operands
   are provided as ARG[0-3].  */

hsa_insn_atomic::hsa_insn_atomic (int nops, int opc,
				  enum BrigAtomicOperation aop,
				  BrigType16_t t, BrigMemoryOrder memorder,
				  hsa_op_base *arg0,
				  hsa_op_base *arg1, hsa_op_base *arg2,
				  hsa_op_base *arg3)
  : hsa_insn_mem (nops, opc, t, arg0, arg1, arg2, arg3), m_atomicop (aop),
    m_memoryorder (memorder),
    m_memoryscope (BRIG_MEMORY_SCOPE_SYSTEM)
{
  gcc_checking_assert (opc == BRIG_OPCODE_ATOMICNORET ||
		       opc == BRIG_OPCODE_ATOMIC ||
		       opc == BRIG_OPCODE_SIGNAL ||
		       opc == BRIG_OPCODE_SIGNALNORET);
}

/* New operator to allocate signal instruction from pool alloc.  */

void *
hsa_insn_atomic::operator new (size_t)
{
  return hsa_allocp_inst_atomic->allocate_raw ();
}

/* Constructor of class representing signal instructions.  OPC is the prinicpal
   opcode, sop is the specific signal operation opcode.  T is the type of the
   instruction.  The instruction operands are provided as ARG[0-3].  */

hsa_insn_signal::hsa_insn_signal (int nops, int opc,
				  enum BrigAtomicOperation sop,
				  BrigType16_t t, hsa_op_base *arg0,
				  hsa_op_base *arg1, hsa_op_base *arg2,
				  hsa_op_base *arg3)
  : hsa_insn_atomic (nops, opc, sop, t, BRIG_MEMORY_ORDER_SC_ACQUIRE_RELEASE,
		     arg0, arg1, arg2, arg3)
{
}

/* New operator to allocate signal instruction from pool alloc.  */

void *
hsa_insn_signal::operator new (size_t)
{
  return hsa_allocp_inst_signal->allocate_raw ();
}

/* Constructor of class representing segment conversion instructions.  OPC is
   the opcode which must be either BRIG_OPCODE_STOF or BRIG_OPCODE_FTOS.  DEST
   and SRCT are destination and source types respectively, SEG is the segment
   we are converting to or from.  The instruction operands are
   provided as ARG0 and ARG1.  */

hsa_insn_seg::hsa_insn_seg (int opc, BrigType16_t dest, BrigType16_t srct,
			    BrigSegment8_t seg, hsa_op_base *arg0,
			    hsa_op_base *arg1)
  : hsa_insn_basic (2, opc, dest, arg0, arg1), m_src_type (srct),
    m_segment (seg)
{
  gcc_checking_assert (opc == BRIG_OPCODE_STOF || opc == BRIG_OPCODE_FTOS);
}

/* New operator to allocate address conversion instruction from pool alloc.  */

void *
hsa_insn_seg::operator new (size_t)
{
  return hsa_allocp_inst_seg->allocate_raw ();
}

/* Constructor of class representing a call instruction.  CALLEE is the tree
   representation of the function being called.  */

hsa_insn_call::hsa_insn_call (tree callee)
  : hsa_insn_basic (0, BRIG_OPCODE_CALL), m_called_function (callee),
    m_output_arg (NULL), m_args_code_list (NULL), m_result_code_list (NULL)
{
}

hsa_insn_call::hsa_insn_call (hsa_internal_fn *fn)
  : hsa_insn_basic (0, BRIG_OPCODE_CALL), m_called_function (NULL),
    m_called_internal_fn (fn), m_output_arg (NULL), m_args_code_list (NULL),
    m_result_code_list (NULL)
{
}

/* New operator to allocate call instruction from pool alloc.  */

void *
hsa_insn_call::operator new (size_t)
{
  return hsa_allocp_inst_call->allocate_raw ();
}

hsa_insn_call::~hsa_insn_call ()
{
  for (unsigned i = 0; i < m_input_args.length (); i++)
    delete m_input_args[i];

  delete m_output_arg;

  m_input_args.release ();
  m_input_arg_insns.release ();
}

/* Constructor of class representing the argument block required to invoke
   a call in HSAIL.  */
hsa_insn_arg_block::hsa_insn_arg_block (BrigKind brig_kind,
					hsa_insn_call * call)
  : hsa_insn_basic (0, HSA_OPCODE_ARG_BLOCK), m_kind (brig_kind),
    m_call_insn (call)
{
}

/* New operator to allocate argument block instruction from pool alloc.  */

void *
hsa_insn_arg_block::operator new (size_t)
{
  return hsa_allocp_inst_arg_block->allocate_raw ();
}

hsa_insn_comment::hsa_insn_comment (const char *s)
  : hsa_insn_basic (0, BRIG_KIND_DIRECTIVE_COMMENT)
{
  unsigned l = strlen (s);

  /* Append '// ' to the string.  */
  char *buf = XNEWVEC (char, l + 4);
  sprintf (buf, "// %s", s);
  m_comment = buf;
}

/* New operator to allocate comment instruction from pool alloc.  */

void *
hsa_insn_comment::operator new (size_t)
{
  return hsa_allocp_inst_comment->allocate_raw ();
}

hsa_insn_comment::~hsa_insn_comment ()
{
  gcc_checking_assert (m_comment);
  free (m_comment);
  m_comment = NULL;
}

/* Constructor of class representing the queue instruction in HSAIL.  */
hsa_insn_queue::hsa_insn_queue (int nops, BrigOpcode opcode)
  : hsa_insn_basic (nops, opcode, BRIG_TYPE_U64)
{
}

/* New operator to allocate source type instruction from pool alloc.  */

void *
hsa_insn_srctype::operator new (size_t)
{
  return hsa_allocp_inst_srctype->allocate_raw ();
}

/* Constructor of class representing the source type instruction in HSAIL.  */

hsa_insn_srctype::hsa_insn_srctype (int nops, BrigOpcode opcode,
				    BrigType16_t destt, BrigType16_t srct,
				    hsa_op_base *arg0, hsa_op_base *arg1,
				    hsa_op_base *arg2 = NULL)
  : hsa_insn_basic (nops, opcode, destt, arg0, arg1, arg2),
    m_source_type (srct)
{}

/* New operator to allocate packed instruction from pool alloc.  */

void *
hsa_insn_packed::operator new (size_t)
{
  return hsa_allocp_inst_packed->allocate_raw ();
}

/* Constructor of class representing the packed instruction in HSAIL.  */

hsa_insn_packed::hsa_insn_packed (int nops, BrigOpcode opcode,
				  BrigType16_t destt, BrigType16_t srct,
				  hsa_op_base *arg0, hsa_op_base *arg1,
				  hsa_op_base *arg2)
  : hsa_insn_srctype (nops, opcode, destt, srct, arg0, arg1, arg2)
{
  m_operand_list = new hsa_op_operand_list (nops - 1);
}

/* New operator to allocate convert instruction from pool alloc.  */

void *
hsa_insn_cvt::operator new (size_t)
{
  return hsa_allocp_inst_cvt->allocate_raw ();
}

/* Constructor of class representing the convert instruction in HSAIL.  */

hsa_insn_cvt::hsa_insn_cvt (hsa_op_with_type *dest, hsa_op_with_type *src)
  : hsa_insn_basic (2, BRIG_OPCODE_CVT, dest->m_type, dest, src)
{
}

/* New operator to allocate alloca from pool alloc.  */

void *
hsa_insn_alloca::operator new (size_t)
{
  return hsa_allocp_inst_alloca->allocate_raw ();
}

/* Constructor of class representing the alloca in HSAIL.  */

hsa_insn_alloca::hsa_insn_alloca (hsa_op_with_type *dest,
				  hsa_op_with_type *size, unsigned alignment)
  : hsa_insn_basic (2, BRIG_OPCODE_ALLOCA, dest->m_type, dest, size),
    m_align (BRIG_ALIGNMENT_8)
{
  gcc_assert (dest->m_type == BRIG_TYPE_U32);
  if (alignment)
    m_align = hsa_alignment_encoding (alignment);
}

/* Append an instruction INSN into the basic block.  */

void
hsa_bb::append_insn (hsa_insn_basic *insn)
{
  gcc_assert (insn->m_opcode != 0 || insn->operand_count () == 0);
  gcc_assert (!insn->m_bb);

  insn->m_bb = m_bb;
  insn->m_prev = m_last_insn;
  insn->m_next = NULL;
  if (m_last_insn)
    m_last_insn->m_next = insn;
  m_last_insn = insn;
  if (!m_first_insn)
    m_first_insn = insn;
}

/* Insert HSA instruction NEW_INSN immediately before an existing instruction
   OLD_INSN.  */

static void
hsa_insert_insn_before (hsa_insn_basic *new_insn, hsa_insn_basic *old_insn)
{
  hsa_bb *hbb = hsa_bb_for_bb (old_insn->m_bb);

  if (hbb->m_first_insn == old_insn)
    hbb->m_first_insn = new_insn;
  new_insn->m_prev = old_insn->m_prev;
  new_insn->m_next = old_insn;
  if (old_insn->m_prev)
    old_insn->m_prev->m_next = new_insn;
  old_insn->m_prev = new_insn;
}

/* Append HSA instruction NEW_INSN immediately after an existing instruction
   OLD_INSN.  */

static void
hsa_append_insn_after (hsa_insn_basic *new_insn, hsa_insn_basic *old_insn)
{
  hsa_bb *hbb = hsa_bb_for_bb (old_insn->m_bb);

  if (hbb->m_last_insn == old_insn)
    hbb->m_last_insn = new_insn;
  new_insn->m_prev = old_insn;
  new_insn->m_next = old_insn->m_next;
  if (old_insn->m_next)
    old_insn->m_next->m_prev = new_insn;
  old_insn->m_next = new_insn;
}

/* Return a register containing the calculated value of EXP which must be an
   expression consisting of PLUS_EXPRs, MULT_EXPRs, NOP_EXPRs, SSA_NAMEs and
   integer constants as returned by get_inner_reference.
   Newly generated HSA instructions will be appended to HBB.
   Perform all calculations in ADDRTYPE.  */

static hsa_op_with_type *
gen_address_calculation (tree exp, hsa_bb *hbb, BrigType16_t addrtype)
{
  int opcode;

  if (TREE_CODE (exp) == NOP_EXPR)
    exp = TREE_OPERAND (exp, 0);

  switch (TREE_CODE (exp))
    {
    case SSA_NAME:
      return hsa_cfun->reg_for_gimple_ssa (exp)->get_in_type (addrtype, hbb);

    case INTEGER_CST:
      {
       hsa_op_immed *imm = new hsa_op_immed (exp);
       if (addrtype != imm->m_type)
	 imm->m_type = addrtype;
       return imm;
      }

    case PLUS_EXPR:
      opcode = BRIG_OPCODE_ADD;
      break;

    case MULT_EXPR:
      opcode = BRIG_OPCODE_MUL;
      break;

    default:
      gcc_unreachable ();
    }

  hsa_op_reg *res = new hsa_op_reg (addrtype);
  hsa_insn_basic *insn = new hsa_insn_basic (3, opcode, addrtype);
  insn->set_op (0, res);

  hsa_op_with_type *op1 = gen_address_calculation (TREE_OPERAND (exp, 0), hbb,
						   addrtype);
  hsa_op_with_type *op2 = gen_address_calculation (TREE_OPERAND (exp, 1), hbb,
						   addrtype);
  insn->set_op (1, op1);
  insn->set_op (2, op2);

  hbb->append_insn (insn);
  return res;
}

/* If R1 is NULL, just return R2, otherwise append an instruction adding them
   to HBB and return the register holding the result.  */

static hsa_op_reg *
add_addr_regs_if_needed (hsa_op_reg *r1, hsa_op_reg *r2, hsa_bb *hbb)
{
  gcc_checking_assert (r2);
  if (!r1)
    return r2;

  hsa_op_reg *res = new hsa_op_reg (r1->m_type);
  gcc_assert (!hsa_needs_cvt (r1->m_type, r2->m_type));
  hsa_insn_basic *insn = new hsa_insn_basic (3, BRIG_OPCODE_ADD, res->m_type);
  insn->set_op (0, res);
  insn->set_op (1, r1);
  insn->set_op (2, r2);
  hbb->append_insn (insn);
  return res;
}

/* Helper of gen_hsa_addr.  Update *SYMBOL, *ADDRTYPE, *REG and *OFFSET to
   reflect BASE which is the first operand of a MEM_REF or a TARGET_MEM_REF.  */

static void
process_mem_base (tree base, hsa_symbol **symbol, BrigType16_t *addrtype,
		  hsa_op_reg **reg, offset_int *offset, hsa_bb *hbb)
{
  if (TREE_CODE (base) == SSA_NAME)
    {
      gcc_assert (!*reg);
      hsa_op_with_type *ssa
	= hsa_cfun->reg_for_gimple_ssa (base)->get_in_type (*addrtype, hbb);
      *reg = dyn_cast <hsa_op_reg *> (ssa);
    }
  else if (TREE_CODE (base) == ADDR_EXPR)
    {
      tree decl = TREE_OPERAND (base, 0);

      if (!DECL_P (decl) || TREE_CODE (decl) == FUNCTION_DECL)
	{
	  HSA_SORRY_AT (EXPR_LOCATION (base),
			"support for HSA does not implement a memory reference "
			"to a non-declaration type");
	  return;
	}

      gcc_assert (!*symbol);

      *symbol = get_symbol_for_decl (decl);
      *addrtype = hsa_get_segment_addr_type ((*symbol)->m_segment);
    }
  else if (TREE_CODE (base) == INTEGER_CST)
    *offset += wi::to_offset (base);
  else
    gcc_unreachable ();
}

/* Forward declaration of a function.  */

static void
gen_hsa_addr_insns (tree val, hsa_op_reg *dest, hsa_bb *hbb);

/* Generate HSA address operand for a given tree memory reference REF.  If
   instructions need to be created to calculate the address, they will be added
   to the end of HBB.  If a caller provider OUTPUT_BITSIZE and OUTPUT_BITPOS,
   the function assumes that the caller will handle possible
   bit-field references.  Otherwise if we reference a bit-field, sorry message
   is displayed.  */

static hsa_op_address *
gen_hsa_addr (tree ref, hsa_bb *hbb, HOST_WIDE_INT *output_bitsize = NULL,
	      HOST_WIDE_INT *output_bitpos = NULL)
{
  hsa_symbol *symbol = NULL;
  hsa_op_reg *reg = NULL;
  offset_int offset = 0;
  tree origref = ref;
  tree varoffset = NULL_TREE;
  BrigType16_t addrtype = hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT);
  HOST_WIDE_INT bitsize = 0, bitpos = 0;
  BrigType16_t flat_addrtype = hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT);

  if (TREE_CODE (ref) == STRING_CST)
    {
      symbol = hsa_get_string_cst_symbol (ref);
      goto out;
    }
  else if (TREE_CODE (ref) == BIT_FIELD_REF
	   && ((tree_to_uhwi (TREE_OPERAND (ref, 1)) % BITS_PER_UNIT) != 0
	       || (tree_to_uhwi (TREE_OPERAND (ref, 2)) % BITS_PER_UNIT) != 0))
    {
      HSA_SORRY_ATV (EXPR_LOCATION (origref),
		     "support for HSA does not implement "
		     "bit field references such as %E", ref);
      goto out;
    }

  if (handled_component_p (ref))
    {
      enum machine_mode mode;
      int unsignedp, volatilep, preversep;

      ref = get_inner_reference (ref, &bitsize, &bitpos, &varoffset, &mode,
				 &unsignedp, &preversep, &volatilep, false);

      offset = bitpos;
      offset = wi::rshift (offset, LOG2_BITS_PER_UNIT, SIGNED);
    }

  switch (TREE_CODE (ref))
    {
    case ADDR_EXPR:
      {
	addrtype = hsa_get_segment_addr_type (BRIG_SEGMENT_PRIVATE);
	symbol = hsa_cfun->create_hsa_temporary (flat_addrtype);
	hsa_op_reg *r = new hsa_op_reg (flat_addrtype);
	gen_hsa_addr_insns (ref, r, hbb);
	hbb->append_insn (new hsa_insn_mem (BRIG_OPCODE_ST, r->m_type,
					    r, new hsa_op_address (symbol)));

	break;
      }
    case SSA_NAME:
      {
	addrtype = hsa_get_segment_addr_type (BRIG_SEGMENT_PRIVATE);
	symbol = hsa_cfun->create_hsa_temporary (flat_addrtype);
	hsa_op_reg *r = hsa_cfun->reg_for_gimple_ssa (ref);

	hbb->append_insn (new hsa_insn_mem (BRIG_OPCODE_ST, r->m_type,
					    r, new hsa_op_address (symbol)));

	break;
      }
    case PARM_DECL:
    case VAR_DECL:
    case RESULT_DECL:
      gcc_assert (!symbol);
      symbol = get_symbol_for_decl (ref);
      addrtype = hsa_get_segment_addr_type (symbol->m_segment);
      break;

    case MEM_REF:
      process_mem_base (TREE_OPERAND (ref, 0), &symbol, &addrtype, &reg,
			&offset, hbb);

      if (!integer_zerop (TREE_OPERAND (ref, 1)))
	offset += wi::to_offset (TREE_OPERAND (ref, 1));
      break;

    case TARGET_MEM_REF:
      process_mem_base (TMR_BASE (ref), &symbol, &addrtype, &reg, &offset, hbb);
      if (TMR_INDEX (ref))
	{
	  hsa_op_reg *disp1;
	  hsa_op_base *idx = hsa_cfun->reg_for_gimple_ssa
	    (TMR_INDEX (ref))->get_in_type (addrtype, hbb);
	  if (TMR_STEP (ref) && !integer_onep (TMR_STEP (ref)))
	    {
	      disp1 = new hsa_op_reg (addrtype);
	      hsa_insn_basic *insn = new hsa_insn_basic (3, BRIG_OPCODE_MUL,
							 addrtype);

	      /* As step must respect addrtype, we overwrite the type
		 of an immediate value.  */
	      hsa_op_immed *step = new hsa_op_immed (TMR_STEP (ref));
	      step->m_type = addrtype;

	      insn->set_op (0, disp1);
	      insn->set_op (1, idx);
	      insn->set_op (2, step);
	      hbb->append_insn (insn);
	    }
	  else
	    disp1 = as_a <hsa_op_reg *> (idx);
	  reg = add_addr_regs_if_needed (reg, disp1, hbb);
	}
      if (TMR_INDEX2 (ref))
	{
	  if (TREE_CODE (TMR_INDEX2 (ref)) == SSA_NAME)
	    {
	      hsa_op_base *disp2 = hsa_cfun->reg_for_gimple_ssa
		(TMR_INDEX2 (ref))->get_in_type (addrtype, hbb);
	      reg = add_addr_regs_if_needed (reg, as_a <hsa_op_reg *> (disp2),
					     hbb);
	    }
	  else if (TREE_CODE (TMR_INDEX2 (ref)) == INTEGER_CST)
	    offset += wi::to_offset (TMR_INDEX2 (ref));
	  else
	    gcc_unreachable ();
	}
      offset += wi::to_offset (TMR_OFFSET (ref));
      break;
    case FUNCTION_DECL:
      HSA_SORRY_AT (EXPR_LOCATION (origref),
		    "support for HSA does not implement function pointers");
      goto out;
    default:
      HSA_SORRY_ATV (EXPR_LOCATION (origref), "support for HSA does "
		     "not implement memory access to %E", origref);
      goto out;
    }

  if (varoffset)
    {
      if (TREE_CODE (varoffset) == INTEGER_CST)
	offset += wi::to_offset (varoffset);
      else
	{
	  hsa_op_base *off_op = gen_address_calculation (varoffset, hbb,
							 addrtype);
	  reg = add_addr_regs_if_needed (reg, as_a <hsa_op_reg *> (off_op),
					 hbb);
	}
    }

  gcc_checking_assert ((symbol
			&& addrtype
			== hsa_get_segment_addr_type (symbol->m_segment))
		       || (!symbol
			   && addrtype
			   == hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT)));
out:
  HOST_WIDE_INT hwi_offset = offset.to_shwi ();

  /* Calculate remaining bitsize offset (if presented).  */
  bitpos %= BITS_PER_UNIT;
  /* If bitsize is a power of two that is greater or equal to BITS_PER_UNIT, it
     is not a reason to think this is a bit-field access.  */
  if (bitpos == 0
      && (bitsize >= BITS_PER_UNIT)
      && !(bitsize & (bitsize - 1)))
    bitsize = 0;

  if ((bitpos || bitsize) && (output_bitpos == NULL || output_bitsize == NULL))
    HSA_SORRY_ATV (EXPR_LOCATION (origref), "support for HSA does not "
		   "implement unhandled bit field reference such as %E", ref);

  if (output_bitsize != NULL && output_bitpos != NULL)
    {
      *output_bitsize = bitsize;
      *output_bitpos = bitpos;
    }

  return new hsa_op_address (symbol, reg, hwi_offset);
}

/* Generate HSA address for a function call argument of given TYPE.
   INDEX is used to generate corresponding name of the arguments.
   Special value -1 represents fact that result value is created.  */

static hsa_op_address *
gen_hsa_addr_for_arg (tree tree_type, int index)
{
  hsa_symbol *sym = new hsa_symbol (BRIG_TYPE_NONE, BRIG_SEGMENT_ARG,
				    BRIG_LINKAGE_ARG);
  sym->m_type = hsa_type_for_tree_type (tree_type, &sym->m_dim);

  if (index == -1) /* Function result.  */
    sym->m_name = "res";
  else /* Function call arguments.  */
    {
      sym->m_name = NULL;
      sym->m_name_number = index;
    }

  return new hsa_op_address (sym);
}

/* Generate HSA instructions that calculate address of VAL including all
   necessary conversions to flat addressing and place the result into DEST.
   Instructions are appended to HBB.  */

static void
gen_hsa_addr_insns (tree val, hsa_op_reg *dest, hsa_bb *hbb)
{
  /* Handle cases like tmp = NULL, where we just emit a move instruction
     to a register.  */
  if (TREE_CODE (val) == INTEGER_CST)
    {
      hsa_op_immed *c = new hsa_op_immed (val);
      hsa_insn_basic *insn = new hsa_insn_basic (2, BRIG_OPCODE_MOV,
						 dest->m_type, dest, c);
      hbb->append_insn (insn);
      return;
    }

  hsa_op_address *addr;

  gcc_assert (dest->m_type == hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT));
  if (TREE_CODE (val) == ADDR_EXPR)
    val = TREE_OPERAND (val, 0);
  addr = gen_hsa_addr (val, hbb);
  hsa_insn_basic *insn = new hsa_insn_basic (2, BRIG_OPCODE_LDA);
  insn->set_op (1, addr);
  if (addr->m_symbol && addr->m_symbol->m_segment != BRIG_SEGMENT_GLOBAL)
    {
      /* LDA produces segment-relative address, we need to convert
	 it to the flat one.  */
      hsa_op_reg *tmp;
      tmp = new hsa_op_reg (hsa_get_segment_addr_type
			    (addr->m_symbol->m_segment));
      hsa_insn_seg *seg;
      seg = new hsa_insn_seg (BRIG_OPCODE_STOF,
			      hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT),
			      tmp->m_type, addr->m_symbol->m_segment, dest,
			      tmp);

      insn->set_op (0, tmp);
      insn->m_type = tmp->m_type;
      hbb->append_insn (insn);
      hbb->append_insn (seg);
    }
  else
    {
      insn->set_op (0, dest);
      insn->m_type = hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT);
      hbb->append_insn (insn);
    }
}

/* Return an HSA register or HSA immediate value operand corresponding to
   gimple operand OP.  */

static hsa_op_with_type *
hsa_reg_or_immed_for_gimple_op (tree op, hsa_bb *hbb)
{
  hsa_op_reg *tmp;

  if (TREE_CODE (op) == SSA_NAME)
    tmp = hsa_cfun->reg_for_gimple_ssa (op);
  else if (!POINTER_TYPE_P (TREE_TYPE (op)))
    return new hsa_op_immed (op);
  else
    {
      tmp = new hsa_op_reg (hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT));
      gen_hsa_addr_insns (op, tmp, hbb);
    }
  return tmp;
}

/* Create a simple movement instruction with register destination DEST and
   register or immediate source SRC and append it to the end of HBB.  */

void
hsa_build_append_simple_mov (hsa_op_reg *dest, hsa_op_base *src, hsa_bb *hbb)
{
  hsa_insn_basic *insn = new hsa_insn_basic (2, BRIG_OPCODE_MOV, dest->m_type,
					     dest, src);
  if (hsa_op_reg *sreg = dyn_cast <hsa_op_reg *> (src))
    gcc_assert (hsa_type_bit_size (dest->m_type)
		== hsa_type_bit_size (sreg->m_type));
  else
    gcc_assert (hsa_type_bit_size (dest->m_type)
		== hsa_type_bit_size (as_a <hsa_op_immed *> (src)->m_type));

  hbb->append_insn (insn);
}

/* Generate HSAIL instructions loading a bit field into register DEST.
   VALUE_REG is a register of a SSA name that is used in the bit field
   reference.  To identify a bit field BITPOS is offset to the loaded memory
   and BITSIZE is number of bits of the bit field.
   Add instructions to HBB.  */

static void
gen_hsa_insns_for_bitfield (hsa_op_reg *dest, hsa_op_reg *value_reg,
			    HOST_WIDE_INT bitsize, HOST_WIDE_INT bitpos,
			    hsa_bb *hbb)
{
  unsigned type_bitsize = hsa_type_bit_size (dest->m_type);
  unsigned left_shift = type_bitsize - (bitsize + bitpos);
  unsigned right_shift = left_shift + bitpos;

  if (left_shift)
    {
      hsa_op_reg *value_reg_2 = new hsa_op_reg (dest->m_type);
      hsa_op_immed *c = new hsa_op_immed (left_shift, BRIG_TYPE_U32);

      hsa_insn_basic *lshift
	= new hsa_insn_basic (3, BRIG_OPCODE_SHL, value_reg_2->m_type,
			      value_reg_2, value_reg, c);

      hbb->append_insn (lshift);

      value_reg = value_reg_2;
    }

  if (right_shift)
    {
      hsa_op_reg *value_reg_2 = new hsa_op_reg (dest->m_type);
      hsa_op_immed *c = new hsa_op_immed (right_shift, BRIG_TYPE_U32);

      hsa_insn_basic *rshift
	= new hsa_insn_basic (3, BRIG_OPCODE_SHR, value_reg_2->m_type,
			      value_reg_2, value_reg, c);

      hbb->append_insn (rshift);

      value_reg = value_reg_2;
    }

    hsa_insn_basic *assignment
      = new hsa_insn_basic (2, BRIG_OPCODE_MOV, dest->m_type, dest, value_reg);
    hbb->append_insn (assignment);
}


/* Generate HSAIL instructions loading a bit field into register DEST.  ADDR is
   prepared memory address which is used to load the bit field.  To identify a
   bit field BITPOS is offset to the loaded memory and BITSIZE is number of
   bits of the bit field.  Add instructions to HBB.  Load must be performed in
   alignment ALIGN.  */

static void
gen_hsa_insns_for_bitfield_load (hsa_op_reg *dest, hsa_op_address *addr,
				 HOST_WIDE_INT bitsize, HOST_WIDE_INT bitpos,
				 hsa_bb *hbb, BrigAlignment8_t align)
{
  hsa_op_reg *value_reg = new hsa_op_reg (dest->m_type);
  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_LD, dest->m_type, value_reg,
					addr);
  mem->set_align (align);
  hbb->append_insn (mem);
  gen_hsa_insns_for_bitfield (dest, value_reg, bitsize, bitpos, hbb);
}

/* Return the alignment of base memory accesses we issue to perform bit-field
   memory access REF.  */

static BrigAlignment8_t
hsa_bitmemref_alignment (tree ref)
{
  unsigned HOST_WIDE_INT bit_offset = 0;

  while (true)
    {
      if (TREE_CODE (ref) == BIT_FIELD_REF)
	{
	  if (!tree_fits_uhwi_p (TREE_OPERAND (ref, 2)))
	    return BRIG_ALIGNMENT_1;
	  bit_offset += tree_to_uhwi (TREE_OPERAND (ref, 2));
	}
      else if (TREE_CODE (ref) == COMPONENT_REF
	       && DECL_BIT_FIELD (TREE_OPERAND (ref, 1)))
	bit_offset += int_bit_position (TREE_OPERAND (ref, 1));
      else
	break;
      ref = TREE_OPERAND (ref, 0);
    }

  unsigned HOST_WIDE_INT bits = bit_offset % BITS_PER_UNIT;
  unsigned HOST_WIDE_INT byte_bits = bit_offset - bits;
  BrigAlignment8_t base = hsa_alignment_encoding (get_object_alignment (ref));
  if (byte_bits == 0)
    return base;
  return MIN (base, hsa_alignment_encoding (byte_bits & -byte_bits));
}

/* Generate HSAIL instructions loading something into register DEST.  RHS is
   tree representation of the loaded data, which are loaded as type TYPE.  Add
   instructions to HBB.  */

static void
gen_hsa_insns_for_load (hsa_op_reg *dest, tree rhs, tree type, hsa_bb *hbb)
{
  /* The destination SSA name will give us the type.  */
  if (TREE_CODE (rhs) == VIEW_CONVERT_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  if (TREE_CODE (rhs) == SSA_NAME)
    {
      hsa_op_reg *src = hsa_cfun->reg_for_gimple_ssa (rhs);
      hsa_build_append_simple_mov (dest, src, hbb);
    }
  else if (is_gimple_min_invariant (rhs)
	   || TREE_CODE (rhs) == ADDR_EXPR)
    {
      if (POINTER_TYPE_P (TREE_TYPE (rhs)))
	{
	  if (dest->m_type != hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT))
	    {
	      HSA_SORRY_ATV (EXPR_LOCATION (rhs),
			     "support for HSA does not implement conversion "
			     "of %E to the requested non-pointer type.", rhs);
	      return;
	    }

	  gen_hsa_addr_insns (rhs, dest, hbb);
	}
      else if (TREE_CODE (rhs) == COMPLEX_CST)
	{
	  hsa_op_immed *real_part = new hsa_op_immed (TREE_REALPART (rhs));
	  hsa_op_immed *imag_part = new hsa_op_immed (TREE_IMAGPART (rhs));

	  hsa_op_reg *real_part_reg
	    = new hsa_op_reg (hsa_type_for_scalar_tree_type (TREE_TYPE (type),
							     true));
	  hsa_op_reg *imag_part_reg
	    = new hsa_op_reg (hsa_type_for_scalar_tree_type (TREE_TYPE (type),
							     true));

	  hsa_build_append_simple_mov (real_part_reg, real_part, hbb);
	  hsa_build_append_simple_mov (imag_part_reg, imag_part, hbb);

	  BrigType16_t src_type = hsa_bittype_for_type (real_part_reg->m_type);

	  hsa_insn_packed *insn
	    = new hsa_insn_packed (3, BRIG_OPCODE_COMBINE, dest->m_type,
				   src_type, dest, real_part_reg,
				   imag_part_reg);
	  hbb->append_insn (insn);
	}
      else
	{
	  hsa_op_immed *imm = new hsa_op_immed (rhs);
	  hsa_build_append_simple_mov (dest, imm, hbb);
	}
    }
  else if (TREE_CODE (rhs) == REALPART_EXPR || TREE_CODE (rhs) == IMAGPART_EXPR)
    {
      tree pack_type = TREE_TYPE (TREE_OPERAND (rhs, 0));

      hsa_op_reg *packed_reg
	= new hsa_op_reg (hsa_type_for_scalar_tree_type (pack_type, true));

      tree complex_rhs = TREE_OPERAND (rhs, 0);
      gen_hsa_insns_for_load (packed_reg, complex_rhs, TREE_TYPE (complex_rhs),
			      hbb);

      hsa_op_reg *real_reg
	= new hsa_op_reg (hsa_type_for_scalar_tree_type (type, true));

      hsa_op_reg *imag_reg
	= new hsa_op_reg (hsa_type_for_scalar_tree_type (type, true));

      BrigKind16_t brig_type = packed_reg->m_type;
      hsa_insn_packed *packed
	= new hsa_insn_packed (3, BRIG_OPCODE_EXPAND,
			       hsa_bittype_for_type (real_reg->m_type),
	 brig_type, real_reg, imag_reg, packed_reg);

      hbb->append_insn (packed);

      hsa_op_reg *source = TREE_CODE (rhs) == REALPART_EXPR ?
	real_reg : imag_reg;

      hsa_insn_basic *insn = new hsa_insn_basic (2, BRIG_OPCODE_MOV,
						 dest->m_type, dest, source);

      hbb->append_insn (insn);
    }
  else if (TREE_CODE (rhs) == BIT_FIELD_REF
	   && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
    {
      tree ssa_name = TREE_OPERAND (rhs, 0);
      HOST_WIDE_INT bitsize = tree_to_uhwi (TREE_OPERAND (rhs, 1));
      HOST_WIDE_INT bitpos = tree_to_uhwi (TREE_OPERAND (rhs, 2));

      hsa_op_reg *imm_value = hsa_cfun->reg_for_gimple_ssa (ssa_name);
      gen_hsa_insns_for_bitfield (dest, imm_value, bitsize, bitpos, hbb);
    }
  else if (DECL_P (rhs) || TREE_CODE (rhs) == MEM_REF
	   || TREE_CODE (rhs) == TARGET_MEM_REF
	   || handled_component_p (rhs))
    {
      HOST_WIDE_INT bitsize, bitpos;

      /* Load from memory.  */
      hsa_op_address *addr;
      addr = gen_hsa_addr (rhs, hbb, &bitsize, &bitpos);

      /* Handle load of a bit field.  */
      if (bitsize > 64)
	{
	  HSA_SORRY_AT (EXPR_LOCATION (rhs),
			"support for HSA does not implement load from a bit "
			"field bigger than 64 bits");
	  return;
	}

      if (bitsize || bitpos)
	gen_hsa_insns_for_bitfield_load (dest, addr, bitsize, bitpos, hbb,
					 hsa_bitmemref_alignment (rhs));
      else
	{
	  BrigType16_t mtype;
	  /* Not dest->m_type, that's possibly extended.  */
	  mtype = mem_type_for_type (hsa_type_for_scalar_tree_type (type,
								    false));
	  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_LD, mtype, dest,
						addr);
	  mem->set_align (hsa_alignment_encoding (get_object_alignment (rhs)));
	  hbb->append_insn (mem);
	}
    }
  else
    HSA_SORRY_ATV (EXPR_LOCATION (rhs),
		   "support for HSA does not implement loading "
		   "of expression %E",
		   rhs);
}

/* Return number of bits necessary for representation of a bit field,
   starting at BITPOS with size of BITSIZE.  */

static unsigned
get_bitfield_size (unsigned bitpos, unsigned bitsize)
{
  unsigned s = bitpos + bitsize;
  unsigned sizes[] = {8, 16, 32, 64};

  for (unsigned i = 0; i < 4; i++)
    if (s <= sizes[i])
      return sizes[i];

  gcc_unreachable ();
  return 0;
}

/* Generate HSAIL instructions storing into memory.  LHS is the destination of
   the store, SRC is the source operand.  Add instructions to HBB.  */

static void
gen_hsa_insns_for_store (tree lhs, hsa_op_base *src, hsa_bb *hbb)
{
  HOST_WIDE_INT bitsize = 0, bitpos = 0;
  BrigAlignment8_t req_align;
  BrigType16_t mtype;
  mtype = mem_type_for_type (hsa_type_for_scalar_tree_type (TREE_TYPE (lhs),
							    false));
  hsa_op_address *addr;
  addr = gen_hsa_addr (lhs, hbb, &bitsize, &bitpos);

  /* Handle store to a bit field.  */
  if (bitsize > 64)
    {
      HSA_SORRY_AT (EXPR_LOCATION (lhs),
		    "support for HSA does not implement store to a bit field "
		    "bigger than 64 bits");
      return;
    }

  unsigned type_bitsize = get_bitfield_size (bitpos, bitsize);

  /* HSAIL does not support MOV insn with 16-bits integers.  */
  if (type_bitsize < 32)
    type_bitsize = 32;

  if (bitpos || (bitsize && type_bitsize != bitsize))
    {
      unsigned HOST_WIDE_INT mask = 0;
      BrigType16_t mem_type
	= get_integer_type_by_bytes (type_bitsize / BITS_PER_UNIT,
				     !TYPE_UNSIGNED (TREE_TYPE (lhs)));

      for (unsigned i = 0; i < type_bitsize; i++)
	if (i < bitpos || i >= bitpos + bitsize)
	  mask |= ((unsigned HOST_WIDE_INT)1 << i);

      hsa_op_reg *value_reg = new hsa_op_reg (mem_type);

      req_align = hsa_bitmemref_alignment (lhs);
      /* Load value from memory.  */
      hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_LD, mem_type,
					    value_reg, addr);
      mem->set_align (req_align);
      hbb->append_insn (mem);

      /* AND the loaded value with prepared mask.  */
      hsa_op_reg *cleared_reg = new hsa_op_reg (mem_type);

      BrigType16_t t
	= get_integer_type_by_bytes (type_bitsize / BITS_PER_UNIT, false);
      hsa_op_immed *c = new hsa_op_immed (mask, t);

      hsa_insn_basic *clearing
	= new hsa_insn_basic (3, BRIG_OPCODE_AND, mem_type, cleared_reg,
			      value_reg, c);
      hbb->append_insn (clearing);

      /* Shift to left a value that is going to be stored.  */
      hsa_op_reg *new_value_reg = new hsa_op_reg (mem_type);

      hsa_insn_basic *basic = new hsa_insn_basic (2, BRIG_OPCODE_MOV, mem_type,
						  new_value_reg, src);
      hbb->append_insn (basic);

      if (bitpos)
	{
	  hsa_op_reg *shifted_value_reg = new hsa_op_reg (mem_type);
	  c = new hsa_op_immed (bitpos, BRIG_TYPE_U32);

	  hsa_insn_basic *basic
	    = new hsa_insn_basic (3, BRIG_OPCODE_SHL, mem_type,
				  shifted_value_reg, new_value_reg, c);
	  hbb->append_insn (basic);

	  new_value_reg = shifted_value_reg;
	}

      /* OR the prepared value with prepared chunk loaded from memory.  */
      hsa_op_reg *prepared_reg= new hsa_op_reg (mem_type);
      basic = new hsa_insn_basic (3, BRIG_OPCODE_OR, mem_type, prepared_reg,
				  new_value_reg, cleared_reg);
      hbb->append_insn (basic);

      src = prepared_reg;
      mtype = mem_type;
    }
  else
    req_align = hsa_alignment_encoding (get_object_alignment (lhs));

  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_ST, mtype, src, addr);
  mem->set_align (req_align);

  /* The HSAIL verifier has another constraint: if the source is an immediate
     then it must match the destination type.  If it's a register the low bits
     will be used for sub-word stores.  We're always allocating new operands so
     we can modify the above in place.  */
  if (hsa_op_immed *imm = dyn_cast <hsa_op_immed *> (src))
    {
      if (!hsa_type_packed_p (imm->m_type))
	imm->m_type = mem->m_type;
      else
	{
	  /* ...and all vector immediates apparently need to be vectors of
	     unsigned bytes.  */
	  unsigned bs = hsa_type_bit_size (imm->m_type);
	  gcc_assert (bs == hsa_type_bit_size (mem->m_type));
	  switch (bs)
	    {
	    case 32:
	      imm->m_type = BRIG_TYPE_U8X4;
	      break;
	    case 64:
	      imm->m_type = BRIG_TYPE_U8X8;
	      break;
	    case 128:
	      imm->m_type = BRIG_TYPE_U8X16;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
    }

  hbb->append_insn (mem);
}

/* Generate memory copy instructions that are going to be used
   for copying a HSA symbol SRC_SYMBOL (or SRC_REG) to TARGET memory,
   represented by pointer in a register.  */

static void
gen_hsa_memory_copy (hsa_bb *hbb, hsa_op_address *target, hsa_op_address *src,
		     unsigned size)
{
  hsa_op_address *addr;
  hsa_insn_mem *mem;

  unsigned offset = 0;

  while (size)
    {
      unsigned s;
      if (size >= 8)
	s = 8;
      else if (size >= 4)
	s = 4;
      else if (size >= 2)
	s = 2;
      else
	s = 1;

      BrigType16_t t = get_integer_type_by_bytes (s, false);

      hsa_op_reg *tmp = new hsa_op_reg (t);
      addr = new hsa_op_address (src->m_symbol, src->m_reg,
				 src->m_imm_offset + offset);
      mem = new hsa_insn_mem (BRIG_OPCODE_LD, t, tmp, addr);
      hbb->append_insn (mem);

      addr = new hsa_op_address (target->m_symbol, target->m_reg,
				 target->m_imm_offset + offset);
      mem = new hsa_insn_mem (BRIG_OPCODE_ST, t, tmp, addr);
      hbb->append_insn (mem);
      offset += s;
      size -= s;
    }
}

/* Create a memset mask that is created by copying a CONSTANT byte value
   to an integer of BYTE_SIZE bytes.  */

static unsigned HOST_WIDE_INT
build_memset_value (unsigned HOST_WIDE_INT constant, unsigned byte_size)
{
  if (constant == 0)
    return 0;

  HOST_WIDE_INT v = constant;

  for (unsigned i = 1; i < byte_size; i++)
    v |= constant << (8 * i);

  return v;
}

/* Generate memory set instructions that are going to be used
   for setting a CONSTANT byte value to TARGET memory of SIZE bytes.  */

static void
gen_hsa_memory_set (hsa_bb *hbb, hsa_op_address *target,
		    unsigned HOST_WIDE_INT constant,
		    unsigned size)
{
  hsa_op_address *addr;
  hsa_insn_mem *mem;

  unsigned offset = 0;

  while (size)
    {
      unsigned s;
      if (size >= 8)
	s = 8;
      else if (size >= 4)
	s = 4;
      else if (size >= 2)
	s = 2;
      else
	s = 1;

      addr = new hsa_op_address (target->m_symbol, target->m_reg,
				 target->m_imm_offset + offset);

      BrigType16_t t = get_integer_type_by_bytes (s, false);
      HOST_WIDE_INT c = build_memset_value (constant, s);

      mem = new hsa_insn_mem (BRIG_OPCODE_ST, t, new hsa_op_immed (c, t),
			      addr);
      hbb->append_insn (mem);
      offset += s;
      size -= s;
    }
}

/* Generate HSAIL instructions for a single assignment
   of an empty constructor to an ADDR_LHS.  Constructor is passed as a
   tree RHS and all instructions are appended to HBB.  */

void
gen_hsa_ctor_assignment (hsa_op_address *addr_lhs, tree rhs, hsa_bb *hbb)
{
  if (vec_safe_length (CONSTRUCTOR_ELTS (rhs)))
    {
      HSA_SORRY_AT (EXPR_LOCATION (rhs),
		    "support for HSA does not implement load from constructor");
      return;
    }

  unsigned size = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (rhs)));
  gen_hsa_memory_set (hbb, addr_lhs, 0, size);
}

/* Generate HSA instructions for a single assignment of RHS to LHS.
   HBB is the basic block they will be appended to.  */

static void
gen_hsa_insns_for_single_assignment (tree lhs, tree rhs, hsa_bb *hbb)
{
  if (TREE_CODE (lhs) == SSA_NAME)
    {
      hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
      if (hsa_seen_error ())
	return;

      gen_hsa_insns_for_load (dest, rhs, TREE_TYPE (lhs), hbb);
    }
  else if (TREE_CODE (rhs) == SSA_NAME
	   || (is_gimple_min_invariant (rhs) && TREE_CODE (rhs) != STRING_CST))
    {
      /* Store to memory.  */
      hsa_op_base *src = hsa_reg_or_immed_for_gimple_op (rhs, hbb);
      if (hsa_seen_error ())
	return;

      gen_hsa_insns_for_store (lhs, src, hbb);
    }
  else
    {
      hsa_op_address *addr_lhs = gen_hsa_addr (lhs, hbb);

      if (TREE_CODE (rhs) == CONSTRUCTOR)
	gen_hsa_ctor_assignment (addr_lhs, rhs, hbb);
      else
	{
	  hsa_op_address *addr_rhs = gen_hsa_addr (rhs, hbb);

	  unsigned size = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (rhs)));
	  gen_hsa_memory_copy (hbb, addr_lhs, addr_rhs, size);
	}
    }
}

/* Prepend before INSN a load from spill symbol of SPILL_REG.  Return the
   register into which we loaded.  If this required another register to convert
   from a B1 type, return it in *PTMP2, otherwise store NULL into it.  We
   assume we are out of SSA so the returned register does not have its
   definition set.  */

hsa_op_reg *
hsa_spill_in (hsa_insn_basic *insn, hsa_op_reg *spill_reg, hsa_op_reg **ptmp2)
{
  hsa_symbol *spill_sym = spill_reg->m_spill_sym;
  hsa_op_reg *reg = new hsa_op_reg (spill_sym->m_type);
  hsa_op_address *addr = new hsa_op_address (spill_sym);

  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_LD, spill_sym->m_type,
					reg, addr);
  hsa_insert_insn_before (mem, insn);

  *ptmp2 = NULL;
  if (spill_reg->m_type == BRIG_TYPE_B1)
    {
      hsa_insn_basic *cvtinsn;
      *ptmp2 = reg;
      reg = new hsa_op_reg (spill_reg->m_type);

      cvtinsn = new hsa_insn_cvt (reg, *ptmp2);
      hsa_insert_insn_before (cvtinsn, insn);
    }
  return reg;
}

/* Append after INSN a store to spill symbol of SPILL_REG.  Return the register
   from which we stored.  If this required another register to convert to a B1
   type, return it in *PTMP2, otherwise store NULL into it.  We assume we are
   out of SSA so the returned register does not have its use updated.  */

hsa_op_reg *
hsa_spill_out (hsa_insn_basic *insn, hsa_op_reg *spill_reg, hsa_op_reg **ptmp2)
{
  hsa_symbol *spill_sym = spill_reg->m_spill_sym;
  hsa_op_reg *reg = new hsa_op_reg (spill_sym->m_type);
  hsa_op_address *addr = new hsa_op_address (spill_sym);
  hsa_op_reg *returnreg;

  *ptmp2 = NULL;
  returnreg = reg;
  if (spill_reg->m_type == BRIG_TYPE_B1)
    {
      hsa_insn_basic *cvtinsn;
      *ptmp2 = new hsa_op_reg (spill_sym->m_type);
      reg->m_type = spill_reg->m_type;

      cvtinsn = new hsa_insn_cvt (*ptmp2, returnreg);
      hsa_append_insn_after (cvtinsn, insn);
      insn = cvtinsn;
      reg = *ptmp2;
    }

  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_ST, spill_sym->m_type, reg,
					addr);
  hsa_append_insn_after (mem, insn);
  return returnreg;
}

/* Generate a comparison instruction that will compare LHS and RHS with
   comparison specified by CODE and put result into register DEST.  DEST has to
   have its type set already but must not have its definition set yet.
   Generated instructions will be added to HBB.  */

static void
gen_hsa_cmp_insn_from_gimple (enum tree_code code, tree lhs, tree rhs,
			      hsa_op_reg *dest, hsa_bb *hbb)
{
  BrigCompareOperation8_t compare;

  switch (code)
    {
    case LT_EXPR:
      compare = BRIG_COMPARE_LT;
      break;
    case LE_EXPR:
      compare = BRIG_COMPARE_LE;
      break;
    case GT_EXPR:
      compare = BRIG_COMPARE_GT;
      break;
    case GE_EXPR:
      compare = BRIG_COMPARE_GE;
      break;
    case EQ_EXPR:
      compare = BRIG_COMPARE_EQ;
      break;
    case NE_EXPR:
      compare = BRIG_COMPARE_NE;
      break;
    case UNORDERED_EXPR:
      compare = BRIG_COMPARE_NAN;
      break;
    case ORDERED_EXPR:
      compare = BRIG_COMPARE_NUM;
      break;
    case UNLT_EXPR:
      compare = BRIG_COMPARE_LTU;
      break;
    case UNLE_EXPR:
      compare = BRIG_COMPARE_LEU;
      break;
    case UNGT_EXPR:
      compare = BRIG_COMPARE_GTU;
      break;
    case UNGE_EXPR:
      compare = BRIG_COMPARE_GEU;
      break;
    case UNEQ_EXPR:
      compare = BRIG_COMPARE_EQU;
      break;
    case LTGT_EXPR:
      compare = BRIG_COMPARE_NEU;
      break;

    default:
      HSA_SORRY_ATV (EXPR_LOCATION (lhs),
		     "support for HSA does not implement comparison tree "
		     "code %s\n", get_tree_code_name (code));
      return;
    }

  /* CMP instruction returns e.g. 0xffffffff (for a 32-bit with integer)
     as a result of comparison.  */

  BrigType16_t dest_type = hsa_type_integer_p (dest->m_type)
    ? (BrigType16_t) BRIG_TYPE_B1 : dest->m_type;

  hsa_insn_cmp *cmp = new hsa_insn_cmp (compare, dest_type);
  cmp->set_op (1, hsa_reg_or_immed_for_gimple_op (lhs, hbb));
  cmp->set_op (2, hsa_reg_or_immed_for_gimple_op (rhs, hbb));

  hbb->append_insn (cmp);
  cmp->set_output_in_type (dest, 0, hbb);
}

/* Generate an unary instruction with OPCODE and append it to a basic block
   HBB.  The instruction uses DEST as a destination and OP1
   as a single operand.  */

static void
gen_hsa_unary_operation (BrigOpcode opcode, hsa_op_reg *dest,
			 hsa_op_with_type *op1, hsa_bb *hbb)
{
  gcc_checking_assert (dest);
  hsa_insn_basic *insn;

  if (opcode == BRIG_OPCODE_MOV && hsa_needs_cvt (dest->m_type, op1->m_type))
    insn = new hsa_insn_cvt (dest, op1);
  else if (opcode == BRIG_OPCODE_FIRSTBIT || opcode == BRIG_OPCODE_LASTBIT)
    insn = new hsa_insn_srctype (2, opcode, BRIG_TYPE_U32, op1->m_type, NULL,
				 op1);
  else
    {
      insn = new hsa_insn_basic (2, opcode, dest->m_type, dest, op1);

      if (opcode == BRIG_OPCODE_ABS || opcode == BRIG_OPCODE_NEG)
	{
	  /* ABS and NEG only exist in _s form :-/  */
	  if (insn->m_type == BRIG_TYPE_U32)
	    insn->m_type = BRIG_TYPE_S32;
	  else if (insn->m_type == BRIG_TYPE_U64)
	    insn->m_type = BRIG_TYPE_S64;
	}
    }

  hbb->append_insn (insn);

  if (opcode == BRIG_OPCODE_FIRSTBIT || opcode == BRIG_OPCODE_LASTBIT)
    insn->set_output_in_type (dest, 0, hbb);
}

/* Generate a binary instruction with OPCODE and append it to a basic block
   HBB.  The instruction uses DEST as a destination and operands OP1
   and OP2.  */

static void
gen_hsa_binary_operation (int opcode, hsa_op_reg *dest,
			  hsa_op_base *op1, hsa_op_base *op2, hsa_bb *hbb)
{
  gcc_checking_assert (dest);

  if ((opcode == BRIG_OPCODE_SHL || opcode == BRIG_OPCODE_SHR)
      && is_a <hsa_op_immed *> (op2))
    {
      hsa_op_immed *i = dyn_cast <hsa_op_immed *> (op2);
      i->set_type (BRIG_TYPE_U32);
    }
  if ((opcode == BRIG_OPCODE_OR
       || opcode == BRIG_OPCODE_XOR
       || opcode == BRIG_OPCODE_AND)
      && is_a <hsa_op_immed *> (op2))
    {
      hsa_op_immed *i = dyn_cast <hsa_op_immed *> (op2);
      i->set_type (hsa_unsigned_type_for_type (i->m_type));
    }

  hsa_insn_basic *insn = new hsa_insn_basic (3, opcode, dest->m_type, dest,
					     op1, op2);
  hbb->append_insn (insn);
}

/* Generate HSA instructions for a single assignment.  HBB is the basic block
   they will be appended to.  */

static void
gen_hsa_insns_for_operation_assignment (gimple *assign, hsa_bb *hbb)
{
  tree_code code = gimple_assign_rhs_code (assign);
  gimple_rhs_class rhs_class = get_gimple_rhs_class (gimple_expr_code (assign));

  tree lhs = gimple_assign_lhs (assign);
  tree rhs1 = gimple_assign_rhs1 (assign);
  tree rhs2 = gimple_assign_rhs2 (assign);
  tree rhs3 = gimple_assign_rhs3 (assign);

  BrigOpcode opcode;

  switch (code)
    {
    CASE_CONVERT:
    case FLOAT_EXPR:
      /* The opcode is changed to BRIG_OPCODE_CVT if BRIG types
	 needs a conversion.  */
      opcode = BRIG_OPCODE_MOV;
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      opcode = BRIG_OPCODE_ADD;
      break;
    case MINUS_EXPR:
      opcode = BRIG_OPCODE_SUB;
      break;
    case MULT_EXPR:
      opcode = BRIG_OPCODE_MUL;
      break;
    case MULT_HIGHPART_EXPR:
      opcode = BRIG_OPCODE_MULHI;
      break;
    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
      opcode = BRIG_OPCODE_DIV;
      break;
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      HSA_SORRY_AT (gimple_location (assign),
		    "support for HSA does not implement CEIL_DIV_EXPR, "
		    "FLOOR_DIV_EXPR or ROUND_DIV_EXPR");
      return;
    case TRUNC_MOD_EXPR:
      opcode = BRIG_OPCODE_REM;
      break;
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      HSA_SORRY_AT (gimple_location (assign),
		    "support for HSA does not implement CEIL_MOD_EXPR, "
		    "FLOOR_MOD_EXPR or ROUND_MOD_EXPR");
      return;
    case NEGATE_EXPR:
      opcode = BRIG_OPCODE_NEG;
      break;
    case MIN_EXPR:
      opcode = BRIG_OPCODE_MIN;
      break;
    case MAX_EXPR:
      opcode = BRIG_OPCODE_MAX;
      break;
    case ABS_EXPR:
      opcode = BRIG_OPCODE_ABS;
      break;
    case LSHIFT_EXPR:
      opcode = BRIG_OPCODE_SHL;
      break;
    case RSHIFT_EXPR:
      opcode = BRIG_OPCODE_SHR;
      break;
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      {
	hsa_insn_basic *insn = NULL;
	int code1 = code == LROTATE_EXPR ? BRIG_OPCODE_SHL : BRIG_OPCODE_SHR;
	int code2 = code != LROTATE_EXPR ? BRIG_OPCODE_SHL : BRIG_OPCODE_SHR;
	BrigType16_t btype = hsa_type_for_scalar_tree_type (TREE_TYPE (lhs),
							    true);

	hsa_op_with_type *src = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);
	hsa_op_reg *op1 = new hsa_op_reg (btype);
	hsa_op_reg *op2 = new hsa_op_reg (btype);
	hsa_op_with_type *shift1 = hsa_reg_or_immed_for_gimple_op (rhs2, hbb);

	tree type = TREE_TYPE (rhs2);
	unsigned HOST_WIDE_INT bitsize = TREE_INT_CST_LOW (TYPE_SIZE (type));

	hsa_op_with_type *shift2 = NULL;
	if (TREE_CODE (rhs2) == INTEGER_CST)
	  shift2 = new hsa_op_immed (bitsize - tree_to_uhwi (rhs2),
				     BRIG_TYPE_U32);
	else if (TREE_CODE (rhs2) == SSA_NAME)
	  {
	    hsa_op_reg *s = hsa_cfun->reg_for_gimple_ssa (rhs2);
	    hsa_op_reg *d = new hsa_op_reg (s->m_type);
	    hsa_op_immed *size_imm = new hsa_op_immed (bitsize, BRIG_TYPE_U32);

	    insn = new hsa_insn_basic (3, BRIG_OPCODE_SUB, d->m_type,
				       d, s, size_imm);
	    hbb->append_insn (insn);

	    shift2 = d;
	  }
	else
	  gcc_unreachable ();

	hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
	gen_hsa_binary_operation (code1, op1, src, shift1, hbb);
	gen_hsa_binary_operation (code2, op2, src, shift2, hbb);
	gen_hsa_binary_operation (BRIG_OPCODE_OR, dest, op1, op2, hbb);

	return;
      }
    case BIT_IOR_EXPR:
      opcode = BRIG_OPCODE_OR;
      break;
    case BIT_XOR_EXPR:
      opcode = BRIG_OPCODE_XOR;
      break;
    case BIT_AND_EXPR:
      opcode = BRIG_OPCODE_AND;
      break;
    case BIT_NOT_EXPR:
      opcode = BRIG_OPCODE_NOT;
      break;
    case FIX_TRUNC_EXPR:
      {
	hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
	hsa_op_with_type *v = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);

	if (hsa_needs_cvt (dest->m_type, v->m_type))
	  {
	    hsa_op_reg *tmp = new hsa_op_reg (v->m_type);

	    hsa_insn_basic *insn = new hsa_insn_basic (2, BRIG_OPCODE_TRUNC,
						       tmp->m_type, tmp, v);
	    hbb->append_insn (insn);

	    hsa_insn_basic *cvtinsn = new hsa_insn_cvt (dest, tmp);
	    hbb->append_insn (cvtinsn);
	  }
	else
	  {
	    hsa_insn_basic *insn = new hsa_insn_basic (2, BRIG_OPCODE_TRUNC,
						       dest->m_type, dest, v);
	    hbb->append_insn (insn);
	  }

	return;
      }
      opcode = BRIG_OPCODE_TRUNC;
      break;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
      {
	hsa_op_reg *dest
	  = hsa_cfun->reg_for_gimple_ssa (gimple_assign_lhs (assign));

	gen_hsa_cmp_insn_from_gimple (code, rhs1, rhs2, dest, hbb);
	return;
      }
    case COND_EXPR:
      {
	hsa_op_reg *dest
	  = hsa_cfun->reg_for_gimple_ssa (gimple_assign_lhs (assign));
	hsa_op_with_type *ctrl = NULL;
	tree cond = rhs1;

	if (CONSTANT_CLASS_P (cond) || TREE_CODE (cond) == SSA_NAME)
	  ctrl = hsa_reg_or_immed_for_gimple_op (cond, hbb);
	else
	  {
	    hsa_op_reg *r = new hsa_op_reg (BRIG_TYPE_B1);

	    gen_hsa_cmp_insn_from_gimple (TREE_CODE (cond),
				  TREE_OPERAND (cond, 0),
				  TREE_OPERAND (cond, 1),
				  r, hbb);

	    ctrl = r;
	  }

	hsa_op_with_type *op2 = hsa_reg_or_immed_for_gimple_op (rhs2, hbb);
	hsa_op_with_type *op3 = hsa_reg_or_immed_for_gimple_op (rhs3, hbb);

	BrigType16_t utype = hsa_unsigned_type_for_type (dest->m_type);
	if (is_a <hsa_op_immed *> (op2))
	  op2->m_type = utype;
	if (is_a <hsa_op_immed *> (op3))
	  op3->m_type = utype;

	hsa_insn_basic *insn
	  = new hsa_insn_basic (4, BRIG_OPCODE_CMOV,
				hsa_bittype_for_type (dest->m_type),
				dest, ctrl, op2, op3);

	hbb->append_insn (insn);
	return;
      }
    case COMPLEX_EXPR:
      {
	hsa_op_reg *dest
	  = hsa_cfun->reg_for_gimple_ssa (gimple_assign_lhs (assign));
	hsa_op_with_type *rhs1_reg = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);
	hsa_op_with_type *rhs2_reg = hsa_reg_or_immed_for_gimple_op (rhs2, hbb);

	if (hsa_seen_error ())
	  return;

	BrigType16_t src_type = hsa_bittype_for_type (rhs1_reg->m_type);
	rhs1_reg = rhs1_reg->get_in_type (src_type, hbb);
	rhs2_reg = rhs2_reg->get_in_type (src_type, hbb);

	hsa_insn_packed *insn
	  = new hsa_insn_packed (3, BRIG_OPCODE_COMBINE, dest->m_type, src_type,
				 dest, rhs1_reg, rhs2_reg);
	hbb->append_insn (insn);

	return;
      }
    default:
      /* Implement others as we come across them.  */
      HSA_SORRY_ATV (gimple_location (assign),
		     "support for HSA does not implement operation %s",
		     get_tree_code_name (code));
      return;
    }


  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (gimple_assign_lhs (assign));

  hsa_op_with_type *op1 = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);
  hsa_op_with_type *op2 = rhs2 != NULL_TREE ?
    hsa_reg_or_immed_for_gimple_op (rhs2, hbb) : NULL;

  if (hsa_seen_error ())
    return;

  switch (rhs_class)
    {
    case GIMPLE_TERNARY_RHS:
      gcc_unreachable ();
      return;

      /* Fall through */
    case GIMPLE_BINARY_RHS:
      gen_hsa_binary_operation (opcode, dest, op1, op2, hbb);
      break;
      /* Fall through */
    case GIMPLE_UNARY_RHS:
      gen_hsa_unary_operation (opcode, dest, op1, hbb);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Generate HSA instructions for a given gimple condition statement COND.
   Instructions will be appended to HBB, which also needs to be the
   corresponding structure to the basic_block of COND.  */

static void
gen_hsa_insns_for_cond_stmt (gimple *cond, hsa_bb *hbb)
{
  hsa_op_reg *ctrl = new hsa_op_reg (BRIG_TYPE_B1);
  hsa_insn_br *cbr;

  gen_hsa_cmp_insn_from_gimple (gimple_cond_code (cond),
				gimple_cond_lhs (cond),
				gimple_cond_rhs (cond),
				ctrl, hbb);

  cbr = new hsa_insn_br (ctrl);
  hbb->append_insn (cbr);
}

/* Maximum number of elements in a jump table for an HSA SBR instruction.  */

#define HSA_MAXIMUM_SBR_LABELS	16

/* Return lowest value of a switch S that is handled in a non-default
   label.  */

static tree
get_switch_low (gswitch *s)
{
  unsigned labels = gimple_switch_num_labels (s);
  gcc_checking_assert (labels >= 1);

  return CASE_LOW (gimple_switch_label (s, 1));
}

/* Return highest value of a switch S that is handled in a non-default
   label.  */

static tree
get_switch_high (gswitch *s)
{
  unsigned labels = gimple_switch_num_labels (s);

  /* Compare last label to maximum number of labels.  */
  tree label = gimple_switch_label (s, labels - 1);
  tree low = CASE_LOW (label);
  tree high = CASE_HIGH (label);

  return high != NULL_TREE ? high : low;
}

static tree
get_switch_size (gswitch *s)
{
  return int_const_binop (MINUS_EXPR, get_switch_high (s), get_switch_low (s));
}

/* Generate HSA instructions for a given gimple switch.
   Instructions will be appended to HBB.  */

static void
gen_hsa_insns_for_switch_stmt (gswitch *s, hsa_bb *hbb)
{
  function *func = DECL_STRUCT_FUNCTION (current_function_decl);
  tree index_tree = gimple_switch_index (s);
  tree lowest = get_switch_low (s);

  hsa_op_reg *index = hsa_cfun->reg_for_gimple_ssa (index_tree);
  hsa_op_reg *sub_index = new hsa_op_reg (index->m_type);
  hbb->append_insn (new hsa_insn_basic (3, BRIG_OPCODE_SUB, sub_index->m_type,
					sub_index, index,
					new hsa_op_immed (lowest)));

  hsa_op_base *tmp = sub_index->get_in_type (BRIG_TYPE_U64, hbb);
  sub_index = as_a <hsa_op_reg *> (tmp);
  unsigned labels = gimple_switch_num_labels (s);
  unsigned HOST_WIDE_INT size = tree_to_uhwi (get_switch_size (s));

  hsa_insn_sbr *sbr = new hsa_insn_sbr (sub_index, size + 1);
  tree default_label = gimple_switch_default_label (s);
  basic_block default_label_bb = label_to_block_fn (func,
						    CASE_LABEL (default_label));

  sbr->m_default_bb = default_label_bb;

  /* Prepare array with default label destination.  */
  for (unsigned HOST_WIDE_INT i = 0; i <= size; i++)
    sbr->m_jump_table.safe_push (default_label_bb);

  /* Iterate all labels and fill up the jump table.  */
  for (unsigned i = 1; i < labels; i++)
    {
      tree label = gimple_switch_label (s, i);
      basic_block bb = label_to_block_fn (func, CASE_LABEL (label));

      unsigned HOST_WIDE_INT sub_low
	= tree_to_uhwi (int_const_binop (MINUS_EXPR, CASE_LOW (label), lowest));

      unsigned HOST_WIDE_INT sub_high = sub_low;
      tree high = CASE_HIGH (label);
      if (high != NULL)
	sub_high = tree_to_uhwi (int_const_binop (MINUS_EXPR, high, lowest));

      for (unsigned HOST_WIDE_INT j = sub_low; j <= sub_high; j++)
	sbr->m_jump_table[j] = bb;
    }

  hbb->append_insn (sbr);
}

/* Verify that the function DECL can be handled by HSA.  */

static void
verify_function_arguments (tree decl)
{
  if (DECL_STATIC_CHAIN (decl))
    {
      HSA_SORRY_ATV (EXPR_LOCATION (decl),
		     "HSA does not support nested functions: %D", decl);
      return;
    }
  else if (!TYPE_ARG_TYPES (TREE_TYPE (decl)))
    {
      HSA_SORRY_ATV (EXPR_LOCATION (decl),
		     "HSA does not support functions with variadic arguments "
		     "(or unknown return type): %D", decl);
      return;
    }
}

/* Return BRIG type for FORMAL_ARG_TYPE.  If the formal argument type is NULL,
   return ACTUAL_ARG_TYPE.  */

static BrigType16_t
get_format_argument_type (tree formal_arg_type, BrigType16_t actual_arg_type)
{
  if (formal_arg_type == NULL)
    return actual_arg_type;

  BrigType16_t decl_type
    = hsa_type_for_scalar_tree_type (formal_arg_type, false);
  return mem_type_for_type (decl_type);
}

/* Generate HSA instructions for a direct call instruction.
   Instructions will be appended to HBB, which also needs to be the
   corresponding structure to the basic_block of STMT.  */

static void
gen_hsa_insns_for_direct_call (gimple *stmt, hsa_bb *hbb)
{
  tree decl = gimple_call_fndecl (stmt);
  verify_function_arguments (decl);
  if (hsa_seen_error ())
    return;

  hsa_insn_call *call_insn = new hsa_insn_call (decl);
  hsa_cfun->m_called_functions.safe_push (call_insn->m_called_function);

  /* Argument block start.  */
  hsa_insn_arg_block *arg_start
    = new hsa_insn_arg_block (BRIG_KIND_DIRECTIVE_ARG_BLOCK_START, call_insn);
  hbb->append_insn (arg_start);

  tree parm_type_chain = TYPE_ARG_TYPES (gimple_call_fntype (stmt));

  /* Preparation of arguments that will be passed to function.  */
  const unsigned args = gimple_call_num_args (stmt);
  for (unsigned i = 0; i < args; ++i)
    {
      tree parm = gimple_call_arg (stmt, (int)i);
      tree parm_decl_type = parm_type_chain != NULL_TREE
	? TREE_VALUE (parm_type_chain) : NULL_TREE;
      hsa_op_address *addr;

      if (AGGREGATE_TYPE_P (TREE_TYPE (parm)))
	{
	  addr = gen_hsa_addr_for_arg (TREE_TYPE (parm), i);
	  hsa_op_address *src = gen_hsa_addr (parm, hbb);
	  gen_hsa_memory_copy (hbb, addr, src,
			       addr->m_symbol->total_byte_size ());
	}
      else
	{
	  hsa_op_with_type *src = hsa_reg_or_immed_for_gimple_op (parm, hbb);

	  if (parm_decl_type != NULL && AGGREGATE_TYPE_P (parm_decl_type))
	    {
	      HSA_SORRY_AT (gimple_location (stmt),
			    "support for HSA does not implement an aggregate "
			    "formal argument in a function call, while actual "
			    "argument is not an aggregate");
	      return;
	    }

	  BrigType16_t formal_arg_type
	    = get_format_argument_type (parm_decl_type, src->m_type);
	  if (hsa_seen_error ())
	    return;

	  if (src->m_type != formal_arg_type)
	    src = src->get_in_type (formal_arg_type, hbb);

	  addr
	    = gen_hsa_addr_for_arg (parm_decl_type != NULL_TREE ?
				    parm_decl_type: TREE_TYPE (parm), i);
	  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_ST, formal_arg_type,
						src, addr);

	  hbb->append_insn (mem);
	}

      call_insn->m_input_args.safe_push (addr->m_symbol);
      if (parm_type_chain)
	parm_type_chain = TREE_CHAIN (parm_type_chain);
    }

  call_insn->m_args_code_list = new hsa_op_code_list (args);
  hbb->append_insn (call_insn);

  tree result_type = TREE_TYPE (TREE_TYPE (decl));

  tree result = gimple_call_lhs (stmt);
  hsa_insn_mem *result_insn = NULL;
  if (!VOID_TYPE_P (result_type))
    {
      hsa_op_address *addr = gen_hsa_addr_for_arg (result_type, -1);

      /* Even if result of a function call is unused, we have to emit
	 declaration for the result.  */
      if (result)
	{
	  tree lhs_type = TREE_TYPE (result);

	  if (hsa_seen_error ())
	    return;

	  if (AGGREGATE_TYPE_P (lhs_type))
	    {
	      hsa_op_address *result_addr = gen_hsa_addr (result, hbb);
	      gen_hsa_memory_copy (hbb, result_addr, addr,
				   addr->m_symbol->total_byte_size ());
	    }
	  else
	    {
	      BrigType16_t mtype
		= mem_type_for_type (hsa_type_for_scalar_tree_type (lhs_type,
								    false));

	      hsa_op_reg *dst = hsa_cfun->reg_for_gimple_ssa (result);
	      result_insn = new hsa_insn_mem (BRIG_OPCODE_LD, mtype, dst, addr);
	      hbb->append_insn (result_insn);
	    }
	}

      call_insn->m_output_arg = addr->m_symbol;
      call_insn->m_result_code_list = new hsa_op_code_list (1);
    }
  else
    {
      if (result)
	{
	  HSA_SORRY_AT (gimple_location (stmt),
			"support for HSA does not implement an assignment of "
			"return value from a void function");
	  return;
	}

      call_insn->m_result_code_list = new hsa_op_code_list (0);
    }

  /* Argument block end.  */
  hsa_insn_arg_block *arg_end
    = new hsa_insn_arg_block (BRIG_KIND_DIRECTIVE_ARG_BLOCK_END, call_insn);
  hbb->append_insn (arg_end);
}

/* Generate HSA instructions for a direct call of an internal fn.
   Instructions will be appended to HBB, which also needs to be the
   corresponding structure to the basic_block of STMT.  */

static void
gen_hsa_insns_for_call_of_internal_fn (gimple *stmt, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  tree lhs_type = TREE_TYPE (lhs);
  tree rhs1 = gimple_call_arg (stmt, 0);
  tree rhs1_type = TREE_TYPE (rhs1);
  enum internal_fn fn = gimple_call_internal_fn (stmt);
  hsa_internal_fn *ifn
    = new hsa_internal_fn (fn, tree_to_uhwi (TYPE_SIZE (rhs1_type)));
  hsa_insn_call *call_insn = new hsa_insn_call (ifn);

  gcc_checking_assert (FLOAT_TYPE_P (rhs1_type));

  if (!hsa_emitted_internal_decls->find (call_insn->m_called_internal_fn))
    hsa_cfun->m_called_internal_fns.safe_push (call_insn->m_called_internal_fn);

  hsa_insn_arg_block *arg_start
    = new hsa_insn_arg_block (BRIG_KIND_DIRECTIVE_ARG_BLOCK_START, call_insn);
  hbb->append_insn (arg_start);

  unsigned num_args = gimple_call_num_args (stmt);

  /* Function arguments.  */
  for (unsigned i = 0; i < num_args; i++)
    {
      tree parm = gimple_call_arg (stmt, (int)i);
      hsa_op_with_type *src = hsa_reg_or_immed_for_gimple_op (parm, hbb);

      hsa_op_address *addr = gen_hsa_addr_for_arg (TREE_TYPE (parm), i);
      hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_ST, src->m_type,
					    src, addr);

      call_insn->m_input_args.safe_push (addr->m_symbol);
      hbb->append_insn (mem);
    }

  call_insn->m_args_code_list = new hsa_op_code_list (num_args);
  hbb->append_insn (call_insn);

  /* Assign returned value.  */
  hsa_op_address *addr = gen_hsa_addr_for_arg (lhs_type, -1);

  call_insn->m_output_arg = addr->m_symbol;
  call_insn->m_result_code_list = new hsa_op_code_list (1);

  /* Argument block end.  */
  hsa_insn_arg_block *arg_end
    = new hsa_insn_arg_block (BRIG_KIND_DIRECTIVE_ARG_BLOCK_END, call_insn);
  hbb->append_insn (arg_end);
}

/* Generate HSA instructions for a return value instruction.
   Instructions will be appended to HBB, which also needs to be the
   corresponding structure to the basic_block of STMT.  */

static void
gen_hsa_insns_for_return (greturn *stmt, hsa_bb *hbb)
{
  tree retval = gimple_return_retval (stmt);
  if (retval)
    {
      hsa_op_address *addr = new hsa_op_address (hsa_cfun->m_output_arg);

      if (AGGREGATE_TYPE_P (TREE_TYPE (retval)))
	{
	  hsa_op_address *retval_addr = gen_hsa_addr (retval, hbb);
	  gen_hsa_memory_copy (hbb, addr, retval_addr,
			       hsa_cfun->m_output_arg->total_byte_size ());
	}
      else
	{
	  BrigType16_t t = hsa_type_for_scalar_tree_type (TREE_TYPE (retval),
							  false);
	  BrigType16_t mtype = mem_type_for_type (t);

	  /* Store of return value.  */
	  hsa_op_with_type *src = hsa_reg_or_immed_for_gimple_op (retval, hbb);
	  src = src->get_in_type (mtype, hbb);
	  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_ST, mtype, src,
						addr);
	  hbb->append_insn (mem);
	}
    }

  /* HSAIL return instruction emission.  */
  hsa_insn_basic *ret = new hsa_insn_basic (0, BRIG_OPCODE_RET);
  hbb->append_insn (ret);
}

/* Set OP_INDEX-th operand of the instruction to DEST, as the DEST
   can have a different type, conversion instructions are possibly
   appended to HBB.  */

void
hsa_insn_basic::set_output_in_type (hsa_op_reg *dest, unsigned op_index,
				    hsa_bb *hbb)
{
  hsa_insn_basic *insn;
  gcc_checking_assert (op_output_p (op_index));

  if (dest->m_type == m_type)
    {
      set_op (op_index, dest);
      return;
    }

  hsa_op_reg *tmp = new hsa_op_reg (m_type);
  set_op (op_index, tmp);

  if (hsa_needs_cvt (dest->m_type, m_type))
    insn = new hsa_insn_cvt (dest, tmp);
  else
    insn = new hsa_insn_basic (2, BRIG_OPCODE_MOV, dest->m_type,
			       dest, tmp->get_in_type (dest->m_type, hbb));

  hbb->append_insn (insn);
}

/* Generate instruction OPCODE to query a property of HSA grid along the
   given DIMENSION.  Store result into DEST and append the instruction to
   HBB.  */

static void
query_hsa_grid (hsa_op_reg *dest, BrigType16_t opcode, int dimension,
		hsa_bb *hbb)
{
  /* We're using just one-dimensional kernels, so hard-coded
     dimension X.  */
  hsa_op_immed *imm
    = new hsa_op_immed (dimension, (BrigKind16_t) BRIG_TYPE_U32);
  hsa_insn_basic *insn = new hsa_insn_basic (2, opcode, BRIG_TYPE_U32, NULL,
					     imm);
  hbb->append_insn (insn);
  insn->set_output_in_type (dest, 0, hbb);
}

/* Generate a special HSA-related instruction for gimple STMT.
   Instructions are appended to basic block HBB.  */

static void
query_hsa_grid (gimple *stmt, BrigOpcode16_t opcode, int dimension,
		hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (dyn_cast <gcall *> (stmt));
  if (lhs == NULL_TREE)
    return;

  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);

  query_hsa_grid (dest, opcode, dimension, hbb);
}

/* Emit instructions that set hsa_num_threads according to provided VALUE.
   Instructions are appended to basic block HBB.  */

static void
gen_set_num_threads (tree value, hsa_bb *hbb)
{
  hbb->append_insn (new hsa_insn_comment ("omp_set_num_threads"));
  hsa_op_with_type *src = hsa_reg_or_immed_for_gimple_op (value, hbb);

  src = src->get_in_type (hsa_num_threads->m_type, hbb);
  hsa_op_address *addr = new hsa_op_address (hsa_num_threads);

  hsa_insn_basic *basic
    = new hsa_insn_mem (BRIG_OPCODE_ST, hsa_num_threads->m_type, src, addr);
  hbb->append_insn (basic);
}

static GTY (()) tree hsa_kernel_dispatch_type = NULL;

/* Return byte offset of a FIELD_NAME in GOMP_hsa_kernel_dispatch which
   is defined in plugin-hsa.c.  */

static HOST_WIDE_INT
get_hsa_kernel_dispatch_offset (const char *field_name)
{
  if (hsa_kernel_dispatch_type == NULL)
    {
      /* Collection of information needed for a dispatch of a kernel from a
	 kernel.  Keep in sync with libgomp's plugin-hsa.c.  */

      hsa_kernel_dispatch_type = make_node (RECORD_TYPE);
      tree id_f1 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("queue"), ptr_type_node);
      DECL_CHAIN (id_f1) = NULL_TREE;
      tree id_f2 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("omp_data_memory"),
			       ptr_type_node);
      DECL_CHAIN (id_f2) = id_f1;
      tree id_f3 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("kernarg_address"),
			       ptr_type_node);
      DECL_CHAIN (id_f3) = id_f2;
      tree id_f4 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("object"),
			       uint64_type_node);
      DECL_CHAIN (id_f4) = id_f3;
      tree id_f5 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("signal"),
			       uint64_type_node);
      DECL_CHAIN (id_f5) = id_f4;
      tree id_f6 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("private_segment_size"),
			       uint32_type_node);
      DECL_CHAIN (id_f6) = id_f5;
      tree id_f7 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("group_segment_size"),
			       uint32_type_node);
      DECL_CHAIN (id_f7) = id_f6;
      tree id_f8 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("kernel_dispatch_count"),
			       uint64_type_node);
      DECL_CHAIN (id_f8) = id_f7;
      tree id_f9 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("debug"),
			       uint64_type_node);
      DECL_CHAIN (id_f9) = id_f8;
      tree id_f10 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				get_identifier ("omp_level"),
				uint64_type_node);
      DECL_CHAIN (id_f10) = id_f9;
      tree id_f11 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				get_identifier ("children_dispatches"),
				ptr_type_node);
      DECL_CHAIN (id_f11) = id_f10;
      tree id_f12 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			       get_identifier ("omp_num_threads"),
			       uint32_type_node);
      DECL_CHAIN (id_f12) = id_f11;


      finish_builtin_struct (hsa_kernel_dispatch_type, "__hsa_kernel_dispatch",
			     id_f12, NULL_TREE);
      TYPE_ARTIFICIAL (hsa_kernel_dispatch_type) = 1;
    }

  for (tree chain = TYPE_FIELDS (hsa_kernel_dispatch_type);
       chain != NULL_TREE; chain = TREE_CHAIN (chain))
    if (strcmp (field_name, IDENTIFIER_POINTER (DECL_NAME (chain))) == 0)
      return int_byte_position (chain);

  gcc_unreachable ();
}

/* Return an HSA register that will contain number of threads for
   a future dispatched kernel.  Instructions are added to HBB.  */

static hsa_op_reg *
gen_num_threads_for_dispatch (hsa_bb *hbb)
{
  /* Step 1) Assign to number of threads:
     MIN (HSA_DEFAULT_NUM_THREADS, hsa_num_threads).  */
  hsa_op_reg *threads = new hsa_op_reg (hsa_num_threads->m_type);
  hsa_op_address *addr = new hsa_op_address (hsa_num_threads);

  hbb->append_insn (new hsa_insn_mem (BRIG_OPCODE_LD, threads->m_type,
				      threads, addr));

  hsa_op_immed *limit = new hsa_op_immed (HSA_DEFAULT_NUM_THREADS,
					  BRIG_TYPE_U32);
  hsa_op_reg *r = new hsa_op_reg (BRIG_TYPE_B1);
  hsa_insn_cmp * cmp
    = new hsa_insn_cmp (BRIG_COMPARE_LT, r->m_type, r, threads, limit);
  hbb->append_insn (cmp);

  BrigType16_t btype = hsa_bittype_for_type (threads->m_type);
  hsa_op_reg *tmp = new hsa_op_reg (threads->m_type);

  hbb->append_insn (new hsa_insn_basic (4, BRIG_OPCODE_CMOV, btype, tmp, r,
					threads, limit));

  /* Step 2) If the number is equal to zero,
     return shadow->omp_num_threads.  */
  hsa_op_reg *shadow_reg_ptr = hsa_cfun->get_shadow_reg ();

  hsa_op_reg *shadow_thread_count = new hsa_op_reg (BRIG_TYPE_U32);
  addr
    = new hsa_op_address (shadow_reg_ptr,
			  get_hsa_kernel_dispatch_offset ("omp_num_threads"));
  hsa_insn_basic *basic
    = new hsa_insn_mem (BRIG_OPCODE_LD, shadow_thread_count->m_type,
			shadow_thread_count, addr);
  hbb->append_insn (basic);

  hsa_op_reg *tmp2 = new hsa_op_reg (threads->m_type);
  r = new hsa_op_reg (BRIG_TYPE_B1);
  hsa_op_immed *imm = new hsa_op_immed (0, shadow_thread_count->m_type);
  hbb->append_insn (new hsa_insn_cmp (BRIG_COMPARE_EQ, r->m_type, r, tmp, imm));
  hbb->append_insn (new hsa_insn_basic (4, BRIG_OPCODE_CMOV, btype, tmp2, r,
					shadow_thread_count, tmp));

  hsa_op_base *dest = tmp2->get_in_type (BRIG_TYPE_U16, hbb);

  return as_a <hsa_op_reg *> (dest);
}


/* Emit instructions that assign number of teams to lhs of gimple STMT.
   Instructions are appended to basic block HBB.  */

static void
gen_get_num_teams (gimple *stmt, hsa_bb *hbb)
{
  if (gimple_call_lhs (stmt) == NULL_TREE)
    return;

  hbb->append_insn (new hsa_insn_comment ("omp_get_num_teams"));

  tree lhs = gimple_call_lhs (stmt);
  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
  hsa_op_immed *one = new hsa_op_immed (1, dest->m_type);

  hsa_insn_basic *basic
    = new hsa_insn_basic (2, BRIG_OPCODE_MOV, dest->m_type, dest, one);

  hbb->append_insn (basic);
}

/* Emit instructions that assign a team number to lhs of gimple STMT.
   Instructions are appended to basic block HBB.  */

static void
gen_get_team_num (gimple *stmt, hsa_bb *hbb)
{
  if (gimple_call_lhs (stmt) == NULL_TREE)
    return;

  hbb->append_insn (new hsa_insn_comment ("omp_get_team_num"));

  tree lhs = gimple_call_lhs (stmt);
  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
  hsa_op_immed *zero = new hsa_op_immed (0, dest->m_type);

  hsa_insn_basic *basic
    = new hsa_insn_basic (2, BRIG_OPCODE_MOV, dest->m_type, dest, zero);

  hbb->append_insn (basic);
}

/* Emit instructions that get levels-var ICV to lhs of gimple STMT.
   Instructions are appended to basic block HBB.  */

static void
gen_get_level (gimple *stmt, hsa_bb *hbb)
{
  if (gimple_call_lhs (stmt) == NULL_TREE)
    return;

  hbb->append_insn (new hsa_insn_comment ("omp_get_level"));

  tree lhs = gimple_call_lhs (stmt);
  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);

  hsa_op_reg *shadow_reg_ptr = hsa_cfun->get_shadow_reg ();
  if (shadow_reg_ptr == NULL)
    {
      HSA_SORRY_AT (gimple_location (stmt),
		    "support for HSA does not implement omp_get_level called "
		    "from a function not being inlined within a kernel");
      return;
    }

  hsa_op_address *addr
    = new hsa_op_address (shadow_reg_ptr,
			  get_hsa_kernel_dispatch_offset ("omp_level"));

  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_LD, BRIG_TYPE_U64,
					(hsa_op_base *) NULL, addr);
  hbb->append_insn (mem);
  mem->set_output_in_type (dest, 0, hbb);
}

/* Emit instruction that implement omp_get_max_threads of gimple STMT.  */

static void
gen_get_max_threads (gimple *stmt, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  hbb->append_insn (new hsa_insn_comment ("omp_get_max_threads"));

  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
  hsa_op_with_type *num_theads_reg = gen_num_threads_for_dispatch (hbb)
    ->get_in_type (dest->m_type, hbb);
  hsa_build_append_simple_mov (dest, num_theads_reg, hbb);
}

/* Emit instructions that implement alloca builtin gimple STMT.
   Instructions are appended to basic block HBB.  */

static void
gen_hsa_alloca (gcall *call, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (call);
  if (lhs == NULL_TREE)
    return;

  built_in_function fn = DECL_FUNCTION_CODE (gimple_call_fndecl (call));

  gcc_checking_assert (fn == BUILT_IN_ALLOCA
		       || fn == BUILT_IN_ALLOCA_WITH_ALIGN);

  unsigned bit_alignment = 0;

  if (fn == BUILT_IN_ALLOCA_WITH_ALIGN)
    {
      tree alignment_tree = gimple_call_arg (call, 1);
      if (TREE_CODE (alignment_tree) != INTEGER_CST)
	{
	  HSA_SORRY_ATV (gimple_location (call),
			 "support for HSA does not implement "
			 "__builtin_alloca_with_align with a non-constant "
			 "alignment: %E", alignment_tree);
	}

      bit_alignment = tree_to_uhwi (alignment_tree);
    }

  tree rhs1 = gimple_call_arg (call, 0);
  hsa_op_with_type *size = hsa_reg_or_immed_for_gimple_op (rhs1, hbb)
    ->get_in_type (BRIG_TYPE_U32, hbb);
  hsa_op_with_type *dest = hsa_cfun->reg_for_gimple_ssa (lhs);

  hsa_op_reg *tmp
    = new hsa_op_reg (hsa_get_segment_addr_type (BRIG_SEGMENT_PRIVATE));
  hsa_insn_alloca *a = new hsa_insn_alloca (tmp, size, bit_alignment);
  hbb->append_insn (a);

  hsa_insn_seg *seg
    = new hsa_insn_seg (BRIG_OPCODE_STOF,
			hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT),
			tmp->m_type, BRIG_SEGMENT_PRIVATE, dest, tmp);
  hbb->append_insn (seg);
}

/* Emit instructions that implement clrsb builtin STMT:
   Returns the number of leading redundant sign bits in x, i.e. the number
   of bits following the most significant bit that are identical to it.
   There are no special cases for 0 or other values.
   Instructions are appended to basic block HBB.  */

static void
gen_hsa_clrsb (gcall *call, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (call);
  if (lhs == NULL_TREE)
    return;

  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
  tree rhs1 = gimple_call_arg (call, 0);
  hsa_op_with_type *arg = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);
  BrigType16_t bittype = hsa_bittype_for_type (arg->m_type);
  unsigned bitsize = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (rhs1)));

  /* FIRSTBIT instruction is defined just for 32 and 64-bits wide integers.  */
  gcc_checking_assert (bitsize == 32 || bitsize == 64);

  /* Set true to MOST_SIG if the most significant bit is set to one.  */
  hsa_op_immed *c = new hsa_op_immed (1ul << (bitsize - 1),
				      hsa_uint_for_bitsize (bitsize));

  hsa_op_reg *and_reg = new hsa_op_reg (bittype);
  gen_hsa_binary_operation (BRIG_OPCODE_AND, and_reg, arg, c, hbb);

  hsa_op_reg *most_sign = new hsa_op_reg (BRIG_TYPE_B1);
  hsa_insn_cmp *cmp
    = new hsa_insn_cmp (BRIG_COMPARE_EQ, most_sign->m_type, most_sign,
			and_reg, c);
  hbb->append_insn (cmp);

  /* If the most significant bit is one, negate the input.  Otherwise
     shift the input value to left by one bit.  */
  hsa_op_reg *arg_neg = new hsa_op_reg (arg->m_type);
  gen_hsa_unary_operation (BRIG_OPCODE_NEG, arg_neg, arg, hbb);

  hsa_op_reg *shifted_arg = new hsa_op_reg (arg->m_type);
  gen_hsa_binary_operation (BRIG_OPCODE_SHL, shifted_arg, arg,
			    new hsa_op_immed (1, BRIG_TYPE_U64), hbb);

  /* Assign the value that can be used for FIRSTBIT instruction according
     to the most significant bit.  */
  hsa_op_reg *tmp = new hsa_op_reg (bittype);
  hsa_insn_basic *cmov
    = new hsa_insn_basic (4, BRIG_OPCODE_CMOV, bittype, tmp, most_sign,
			  arg_neg, shifted_arg);
  hbb->append_insn (cmov);

  hsa_op_reg *leading_bits = new hsa_op_reg (BRIG_TYPE_S32);
  gen_hsa_unary_operation (BRIG_OPCODE_FIRSTBIT, leading_bits,
			   tmp->get_in_type (hsa_uint_for_bitsize (bitsize),
					     hbb), hbb);

  /* Set flag if the input value is equal to zero.  */
  hsa_op_reg *is_zero = new hsa_op_reg (BRIG_TYPE_B1);
  cmp = new hsa_insn_cmp (BRIG_COMPARE_EQ, is_zero->m_type, is_zero, arg,
			  new hsa_op_immed (0, arg->m_type));
  hbb->append_insn (cmp);

  /* Return the number of leading bits,
     or (bitsize - 1) if the input value is zero.  */
  cmov = new hsa_insn_basic (4, BRIG_OPCODE_CMOV, BRIG_TYPE_B32, NULL, is_zero,
			     new hsa_op_immed (bitsize - 1, BRIG_TYPE_U32),
			     leading_bits->get_in_type (BRIG_TYPE_B32, hbb));
  hbb->append_insn (cmov);
  cmov->set_output_in_type (dest, 0, hbb);
}

/* Emit instructions that implement ffs builtin STMT:
   Returns one plus the index of the least significant 1-bit of x,
   or if x is zero, returns zero.
   Instructions are appended to basic block HBB.  */

static void
gen_hsa_ffs (gcall *call, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (call);
  if (lhs == NULL_TREE)
    return;

  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);

  tree rhs1 = gimple_call_arg (call, 0);
  hsa_op_with_type *arg = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);

  hsa_op_reg *tmp = new hsa_op_reg (BRIG_TYPE_U32);
  hsa_insn_srctype *insn = new hsa_insn_srctype (2, BRIG_OPCODE_LASTBIT,
						 tmp->m_type, arg->m_type,
						 tmp, arg);
  hbb->append_insn (insn);

  hsa_insn_basic *addition
    = new hsa_insn_basic (3, BRIG_OPCODE_ADD, tmp->m_type, NULL, tmp,
			  new hsa_op_immed (1, tmp->m_type));
  hbb->append_insn (addition);
  addition->set_output_in_type (dest, 0, hbb);
}

static void
gen_hsa_popcount_to_dest (hsa_op_reg *dest, hsa_op_with_type *arg, hsa_bb *hbb)
{
  gcc_checking_assert (hsa_type_integer_p (arg->m_type));

  if (hsa_type_bit_size (arg->m_type) < 32)
    arg = arg->get_in_type (BRIG_TYPE_B32, hbb);

  if (!hsa_btype_p (arg->m_type))
    arg = arg->get_in_type (hsa_bittype_for_type (arg->m_type), hbb);

  hsa_insn_srctype *popcount
    = new hsa_insn_srctype (2, BRIG_OPCODE_POPCOUNT, BRIG_TYPE_U32,
			    arg->m_type, NULL, arg);
  hbb->append_insn (popcount);
  popcount->set_output_in_type (dest, 0, hbb);
}

/* Emit instructions that implement parity builtin STMT:
   Returns the parity of x, i.e. the number of 1-bits in x modulo 2.
   Instructions are appended to basic block HBB.  */

static void
gen_hsa_parity (gcall *call, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (call);
  if (lhs == NULL_TREE)
    return;

  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
  tree rhs1 = gimple_call_arg (call, 0);
  hsa_op_with_type *arg = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);

  hsa_op_reg *popcount = new hsa_op_reg (BRIG_TYPE_U32);
  gen_hsa_popcount_to_dest (popcount, arg, hbb);

  hsa_insn_basic *insn
    = new hsa_insn_basic (3, BRIG_OPCODE_REM, popcount->m_type, NULL, popcount,
			  new hsa_op_immed (2, popcount->m_type));
  hbb->append_insn (insn);
  insn->set_output_in_type (dest, 0, hbb);
}

/* Emit instructions that implement popcount builtin STMT.
   Instructions are appended to basic block HBB.  */

static void
gen_hsa_popcount (gcall *call, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (call);
  if (lhs == NULL_TREE)
    return;

  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
  tree rhs1 = gimple_call_arg (call, 0);
  hsa_op_with_type *arg = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);

  gen_hsa_popcount_to_dest (dest, arg, hbb);
}

/* Set VALUE to a shadow kernel debug argument and append a new instruction
   to HBB basic block.  */

static void
set_debug_value (hsa_bb *hbb, hsa_op_with_type *value)
{
  hsa_op_reg *shadow_reg_ptr = hsa_cfun->get_shadow_reg ();
  if (shadow_reg_ptr == NULL)
    return;

  hsa_op_address *addr
    = new hsa_op_address (shadow_reg_ptr,
			  get_hsa_kernel_dispatch_offset ("debug"));
  hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_ST, BRIG_TYPE_U64, value,
					addr);
  hbb->append_insn (mem);
}

void
omp_simple_builtin::generate (gimple *stmt, hsa_bb *hbb)
{
  if (m_sorry)
    {
      if (m_warning_message)
	HSA_SORRY_AT (gimple_location (stmt), m_warning_message)
      else
	HSA_SORRY_ATV (gimple_location (stmt),
		       "Support for HSA does not implement calls to %s\n",
		       m_name)
    }
  else if (m_warning_message != NULL)
    warning_at (gimple_location (stmt), OPT_Whsa, m_warning_message);

  if (m_return_value != NULL)
    {
      tree lhs = gimple_call_lhs (stmt);
      if (!lhs)
	return;

      hbb->append_insn (new hsa_insn_comment (m_name));

      hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
      hsa_op_with_type *op = m_return_value->get_in_type (dest->m_type, hbb);
      hsa_build_append_simple_mov (dest, op, hbb);
    }
}

/* If STMT is a call of a known library function, generate code to perform
   it and return true.  */

static bool
gen_hsa_insns_for_known_library_call (gimple *stmt, hsa_bb *hbb)
{
  bool handled = false;
  const char *name = hsa_get_declaration_name (gimple_call_fndecl (stmt));

  char *copy = NULL;
  size_t len = strlen (name);
  if (len > 0 && name[len - 1] == '_')
    {
      copy = XNEWVEC (char, len + 1);
      strcpy (copy, name);
      copy[len - 1] = '\0';
      name = copy;
    }

  /* Handle omp_* routines.  */
  if (strstr (name, "omp_") == name)
    {
      hsa_init_simple_builtins ();
      omp_simple_builtin *builtin = omp_simple_builtins->get (name);
      if (builtin)
	{
	  builtin->generate (stmt, hbb);
	  return true;
	}

      handled = true;
      if (strcmp (name, "omp_set_num_threads") == 0)
	gen_set_num_threads (gimple_call_arg (stmt, 0), hbb);
      else if (strcmp (name, "omp_get_thread_num") == 0)
	{
	  hbb->append_insn (new hsa_insn_comment (name));
	  query_hsa_grid (stmt, BRIG_OPCODE_WORKITEMABSID, 0, hbb);
	}
      else if (strcmp (name, "omp_get_num_threads") == 0)
	{
	  hbb->append_insn (new hsa_insn_comment (name));
	  query_hsa_grid (stmt, BRIG_OPCODE_GRIDSIZE, 0, hbb);
	}
      else if (strcmp (name, "omp_get_num_teams") == 0)
	gen_get_num_teams (stmt, hbb);
      else if (strcmp (name, "omp_get_team_num") == 0)
	gen_get_team_num (stmt, hbb);
      else if (strcmp (name, "omp_get_level") == 0)
	gen_get_level (stmt, hbb);
      else if (strcmp (name, "omp_get_active_level") == 0)
	gen_get_level (stmt, hbb);
      else if (strcmp (name, "omp_in_parallel") == 0)
	gen_get_level (stmt, hbb);
      else if (strcmp (name, "omp_get_max_threads") == 0)
	gen_get_max_threads (stmt, hbb);
      else
	handled = false;

      if (handled)
	{
	  if (copy)
	    free (copy);
	  return true;
	}
    }

  if (strcmp (name, "__hsa_set_debug_value") == 0)
    {
      handled = true;
      if (hsa_cfun->has_shadow_reg_p ())
	{
	  tree rhs1 = gimple_call_arg (stmt, 0);
	  hsa_op_with_type *src = hsa_reg_or_immed_for_gimple_op (rhs1, hbb);

	  src = src->get_in_type (BRIG_TYPE_U64, hbb);
	  set_debug_value (hbb, src);
	}
    }

  if (copy)
    free (copy);
  return handled;
}

/* Helper functions to create a single unary HSA operations out of calls to
   builtins.  OPCODE is the HSA operation to be generated.  STMT is a gimple
   call to a builtin.  HBB is the HSA BB to which the instruction should be
   added.  Note that nothing will be created if STMT does not have a LHS.  */

static void
gen_hsa_unaryop_for_builtin (BrigOpcode opcode, gimple *stmt, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;
  hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (lhs);
  hsa_op_with_type *op
    = hsa_reg_or_immed_for_gimple_op (gimple_call_arg (stmt, 0), hbb);
  gen_hsa_unary_operation (opcode, dest, op, hbb);
}

/* Helper functions to create a call to standard library if LHS of the
   STMT is used.  HBB is the HSA BB to which the instruction should be
   added.  */

static void
gen_hsa_unaryop_builtin_call (gimple *stmt, hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  if (gimple_call_internal_p (stmt))
    gen_hsa_insns_for_call_of_internal_fn (stmt, hbb);
  else
    gen_hsa_insns_for_direct_call (stmt, hbb);
}

/* Helper functions to create a single unary HSA operations out of calls to
   builtins (if unsafe math optimizations are enable). Otherwise, create
   a call to standard library function.
   OPCODE is the HSA operation to be generated.  STMT is a gimple
   call to a builtin.  HBB is the HSA BB to which the instruction should be
   added.  Note that nothing will be created if STMT does not have a LHS.  */

static void
gen_hsa_unaryop_or_call_for_builtin (BrigOpcode opcode, gimple *stmt,
				     hsa_bb *hbb)
{
  if (flag_unsafe_math_optimizations)
    gen_hsa_unaryop_for_builtin (opcode, stmt, hbb);
  else
    gen_hsa_unaryop_builtin_call (stmt, hbb);
}

/* Generate HSA address corresponding to a value VAL (as opposed to a memory
   reference tree), for example an SSA_NAME or an ADDR_EXPR.  HBB is the HSA BB
   to which the instruction should be added.  */

static hsa_op_address *
get_address_from_value (tree val, hsa_bb *hbb)
{
  switch (TREE_CODE (val))
    {
    case SSA_NAME:
      {
	BrigType16_t addrtype = hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT);
	hsa_op_base *reg
	  = hsa_cfun->reg_for_gimple_ssa (val)->get_in_type (addrtype, hbb);
	return new hsa_op_address (NULL, as_a <hsa_op_reg *> (reg), 0);
      }
    case ADDR_EXPR:
      return gen_hsa_addr (TREE_OPERAND (val, 0), hbb);

    case INTEGER_CST:
      if (tree_fits_shwi_p (val))
	return new hsa_op_address (NULL, NULL, tree_to_shwi (val));
      /* Otherwise fall-through */

    default:
      HSA_SORRY_ATV (EXPR_LOCATION (val),
		     "support for HSA does not implement memory access to %E",
		     val);
      return new hsa_op_address (NULL, NULL, 0);
    }
}

/* Return string for MEMMODEL.  */

static const char *
get_memory_order_name (unsigned memmodel)
{
  switch (memmodel & MEMMODEL_BASE_MASK)
    {
    case MEMMODEL_RELAXED:
      return "relaxed";
    case MEMMODEL_CONSUME:
      return "consume";
    case MEMMODEL_ACQUIRE:
      return "acquire";
    case MEMMODEL_RELEASE:
      return "release";
    case MEMMODEL_ACQ_REL:
      return "acq_rel";
    case MEMMODEL_SEQ_CST:
      return "seq_cst";
    default:
      return NULL;
    }
}

/* Return memory order according to predefined __atomic memory model
   constants.  LOCATION is provided to locate the problematic statement.  */

static BrigMemoryOrder
get_memory_order (unsigned memmodel, location_t location)
{
  switch (memmodel & MEMMODEL_BASE_MASK)
    {
    case MEMMODEL_RELAXED:
      return BRIG_MEMORY_ORDER_RELAXED;
    case MEMMODEL_CONSUME:
      /* HSA does not have an equivalent, but we can use the slightly stronger
	 ACQUIRE.  */
    case MEMMODEL_ACQUIRE:
      return BRIG_MEMORY_ORDER_SC_ACQUIRE;
    case MEMMODEL_RELEASE:
      return BRIG_MEMORY_ORDER_SC_RELEASE;
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      /* Callers implementing a simple load or store need to remove the release
	 or acquire part respectively.  */
      return BRIG_MEMORY_ORDER_SC_ACQUIRE_RELEASE;
    default:
      {
	const char *mmname = get_memory_order_name (memmodel);
	HSA_SORRY_ATV (location,
		       "support for HSA does not implement the specified "
		       " memory model%s %s",
		       mmname ? ": " : "", mmname ? mmname : "");
	return BRIG_MEMORY_ORDER_NONE;
      }
    }
}

/* Helper function to create an HSA atomic binary operation instruction out of
   calls to atomic builtins.  RET_ORIG is true if the built-in is the variant
   that return s the value before applying operation, and false if it should
   return the value after applying the operation (if it returns value at all).
   ACODE is the atomic operation code, STMT is a gimple call to a builtin.  HBB
   is the HSA BB to which the instruction should be added.  */

static void
gen_hsa_ternary_atomic_for_builtin (bool ret_orig,
 				    enum BrigAtomicOperation acode,
				    gimple *stmt,
				    hsa_bb *hbb)
{
  tree lhs = gimple_call_lhs (stmt);

  tree type = TREE_TYPE (gimple_call_arg (stmt, 1));
  BrigType16_t hsa_type = hsa_type_for_scalar_tree_type (type, false);
  BrigType16_t mtype = mem_type_for_type (hsa_type);
  tree model = gimple_call_arg (stmt, 2);

  if (!tree_fits_uhwi_p (model))
    {
      HSA_SORRY_ATV (gimple_location (stmt),
		     "support for HSA does not implement memory model %E",
		     model);
      return;
    }

  unsigned HOST_WIDE_INT mmodel = tree_to_uhwi (model);

  BrigMemoryOrder memorder = get_memory_order (mmodel, gimple_location (stmt));

  /* Certain atomic insns must have Bx memory types.  */
  switch (acode)
    {
    case BRIG_ATOMIC_LD:
    case BRIG_ATOMIC_ST:
    case BRIG_ATOMIC_AND:
    case BRIG_ATOMIC_OR:
    case BRIG_ATOMIC_XOR:
    case BRIG_ATOMIC_EXCH:
      mtype = hsa_bittype_for_type (mtype);
      break;
    default:
      break;
    }

  hsa_op_reg *dest;
  int nops, opcode;
  if (lhs)
    {
      if (ret_orig)
	dest = hsa_cfun->reg_for_gimple_ssa (lhs);
      else
	dest = new hsa_op_reg (hsa_type);
      opcode = BRIG_OPCODE_ATOMIC;
      nops = 3;
    }
  else
    {
      dest = NULL;
      opcode = BRIG_OPCODE_ATOMICNORET;
      nops = 2;
    }

  if (acode == BRIG_ATOMIC_ST)
    {
      if (memorder == BRIG_MEMORY_ORDER_SC_ACQUIRE_RELEASE)
	memorder = BRIG_MEMORY_ORDER_SC_RELEASE;

      if (memorder != BRIG_MEMORY_ORDER_RELAXED
	  && memorder != BRIG_MEMORY_ORDER_SC_RELEASE
	  && memorder != BRIG_MEMORY_ORDER_NONE)
	{
	  HSA_SORRY_ATV (gimple_location (stmt),
			 "support for HSA does not implement memory model for "
			 "ATOMIC_ST: %s", get_memory_order_name (mmodel));
	  return;
	}
    }

  hsa_insn_atomic *atominsn = new hsa_insn_atomic (nops, opcode, acode, mtype,
						   memorder);

  hsa_op_address *addr;
  addr = get_address_from_value (gimple_call_arg (stmt, 0), hbb);
  if (addr->m_symbol && addr->m_symbol->m_segment == BRIG_SEGMENT_PRIVATE)
    {
      HSA_SORRY_AT (gimple_location (stmt),
		    "HSA does not implement atomic operations in private "
		    "segment");
      return;
    }
  hsa_op_base *op = hsa_reg_or_immed_for_gimple_op (gimple_call_arg (stmt, 1),
						    hbb);

  if (lhs)
    {
      atominsn->set_op (0, dest);
      atominsn->set_op (1, addr);
      atominsn->set_op (2, op);
    }
  else
    {
      atominsn->set_op (0, addr);
      atominsn->set_op (1, op);
    }

  hbb->append_insn (atominsn);

  /* HSA does not natively support the variants that return the modified value,
     so re-do the operation again non-atomically if that is what was
     requested.  */
  if (lhs && !ret_orig)
    {
      int arith;
      switch (acode)
	{
	case BRIG_ATOMIC_ADD:
	  arith = BRIG_OPCODE_ADD;
	  break;
	case BRIG_ATOMIC_AND:
	  arith = BRIG_OPCODE_AND;
	  break;
	case BRIG_ATOMIC_OR:
	  arith = BRIG_OPCODE_OR;
	  break;
	case BRIG_ATOMIC_SUB:
	  arith = BRIG_OPCODE_SUB;
	  break;
	case BRIG_ATOMIC_XOR:
	  arith = BRIG_OPCODE_XOR;
	  break;
	default:
	  gcc_unreachable ();
	}
      hsa_op_reg *real_dest = hsa_cfun->reg_for_gimple_ssa (lhs);
      gen_hsa_binary_operation (arith, real_dest, dest, op, hbb);
    }
}

/* Generate HSA instructions for an internal fn.
   Instructions will be appended to HBB, which also needs to be the
   corresponding structure to the basic_block of STMT.  */

static void
gen_hsa_insn_for_internal_fn_call (gcall *stmt, hsa_bb *hbb)
{
  gcc_checking_assert (gimple_call_internal_fn (stmt));
  internal_fn fn = gimple_call_internal_fn (stmt);

  bool is_float_type_p = false;
  if (gimple_call_lhs (stmt) != NULL
      && TREE_TYPE (gimple_call_lhs (stmt)) == float_type_node)
    is_float_type_p = true;

  switch (fn)
    {
    case IFN_CEIL:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_CEIL, stmt, hbb);
      break;

    case IFN_FLOOR:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_FLOOR, stmt, hbb);
      break;

    case IFN_RINT:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_RINT, stmt, hbb);
      break;

    case IFN_SQRT:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_SQRT, stmt, hbb);
      break;

    case IFN_TRUNC:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_TRUNC, stmt, hbb);
      break;

    case IFN_COS:
      {
	if (is_float_type_p)
	  gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NCOS, stmt, hbb);
	else
	  gen_hsa_unaryop_builtin_call (stmt, hbb);

	break;
      }
    case IFN_EXP2:
      {
	if (is_float_type_p)
	  gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NEXP2, stmt, hbb);
	else
	  gen_hsa_unaryop_builtin_call (stmt, hbb);

	break;
      }

    case IFN_LOG2:
      {
	if (is_float_type_p)
	  gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NLOG2, stmt, hbb);
	else
	  gen_hsa_unaryop_builtin_call (stmt, hbb);

	break;
      }

    case IFN_SIN:
      {
	if (is_float_type_p)
	  gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NSIN, stmt, hbb);
	else
	  gen_hsa_unaryop_builtin_call (stmt, hbb);
	break;
      }

    case IFN_CLRSB:
      gen_hsa_clrsb (stmt, hbb);
      break;

    case IFN_CLZ:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_FIRSTBIT, stmt, hbb);
      break;

    case IFN_CTZ:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_LASTBIT, stmt, hbb);
      break;

    case IFN_FFS:
      gen_hsa_ffs (stmt, hbb);
      break;

    case IFN_PARITY:
      gen_hsa_parity (stmt, hbb);
      break;

    case IFN_POPCOUNT:
      gen_hsa_popcount (stmt, hbb);
      break;

    case IFN_ACOS:
    case IFN_ASIN:
    case IFN_ATAN:
    case IFN_EXP:
    case IFN_EXP10:
    case IFN_EXPM1:
    case IFN_LOG:
    case IFN_LOG10:
    case IFN_LOG1P:
    case IFN_LOGB:
    case IFN_SIGNIFICAND:
    case IFN_TAN:
    case IFN_NEARBYINT:
    case IFN_ROUND:
    case IFN_ATAN2:
    case IFN_COPYSIGN:
    case IFN_FMOD:
    case IFN_POW:
    case IFN_REMAINDER:
    case IFN_SCALB:
    case IFN_FMIN:
    case IFN_FMAX:
      gen_hsa_insns_for_call_of_internal_fn (stmt, hbb);

    default:
      HSA_SORRY_ATV (gimple_location (stmt),
		     "support for HSA does not implement internal function: %s",
		     internal_fn_name (fn));
      break;
    }
}

#define HSA_MEMORY_BUILTINS_LIMIT     128

/* Generate HSA instructions for the given call statement STMT.  Instructions
   will be appended to HBB.  */

static void
gen_hsa_insns_for_call (gimple *stmt, hsa_bb *hbb)
{
  gcall *call = as_a <gcall *> (stmt);
  tree lhs = gimple_call_lhs (stmt);
  hsa_op_reg *dest;

  if (gimple_call_internal_p (stmt))
    {
      gen_hsa_insn_for_internal_fn_call (call, hbb);
      return;
    }

  if (!gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      tree function_decl = gimple_call_fndecl (stmt);
      if (function_decl == NULL_TREE)
	{
	  HSA_SORRY_AT (gimple_location (stmt),
			"support for HSA does not implement indirect calls");
	  return;
	}

      if (hsa_callable_function_p (function_decl))
	gen_hsa_insns_for_direct_call (stmt, hbb);
      else if (!gen_hsa_insns_for_known_library_call (stmt, hbb))
	HSA_SORRY_AT (gimple_location (stmt),
		      "HSA supports only calls of functions marked with pragma "
		      "omp declare target");
      return;
    }

  tree fndecl = gimple_call_fndecl (stmt);
  enum built_in_function builtin = DECL_FUNCTION_CODE (fndecl);
  switch (builtin)
    {
    case BUILT_IN_FABS:
    case BUILT_IN_FABSF:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_ABS, stmt, hbb);
      break;

    case BUILT_IN_CEIL:
    case BUILT_IN_CEILF:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_CEIL, stmt, hbb);
      break;

    case BUILT_IN_FLOOR:
    case BUILT_IN_FLOORF:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_FLOOR, stmt, hbb);
      break;

    case BUILT_IN_RINT:
    case BUILT_IN_RINTF:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_RINT, stmt, hbb);
      break;

    case BUILT_IN_SQRT:
    case BUILT_IN_SQRTF:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_SQRT, stmt, hbb);
      break;

    case BUILT_IN_TRUNC:
    case BUILT_IN_TRUNCF:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_TRUNC, stmt, hbb);
      break;

    case BUILT_IN_COS:
    case BUILT_IN_SIN:
    case BUILT_IN_EXP2:
    case BUILT_IN_LOG2:
      /* HSAIL does not provide an instruction for double argument type.  */
      gen_hsa_unaryop_builtin_call (stmt, hbb);
      break;

    case BUILT_IN_COSF:
      gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NCOS, stmt, hbb);
      break;

    case BUILT_IN_EXP2F:
      gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NEXP2, stmt, hbb);
      break;

    case BUILT_IN_LOG2F:
      gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NLOG2, stmt, hbb);
      break;

    case BUILT_IN_SINF:
      gen_hsa_unaryop_or_call_for_builtin (BRIG_OPCODE_NSIN, stmt, hbb);
      break;

    case BUILT_IN_CLRSB:
    case BUILT_IN_CLRSBL:
    case BUILT_IN_CLRSBLL:
      gen_hsa_clrsb (call, hbb);
      break;

    case BUILT_IN_CLZ:
    case BUILT_IN_CLZL:
    case BUILT_IN_CLZLL:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_FIRSTBIT, stmt, hbb);
      break;

    case BUILT_IN_CTZ:
    case BUILT_IN_CTZL:
    case BUILT_IN_CTZLL:
      gen_hsa_unaryop_for_builtin (BRIG_OPCODE_LASTBIT, stmt, hbb);
      break;

    case BUILT_IN_FFS:
    case BUILT_IN_FFSL:
    case BUILT_IN_FFSLL:
      gen_hsa_ffs (call, hbb);
      break;

    case BUILT_IN_PARITY:
    case BUILT_IN_PARITYL:
    case BUILT_IN_PARITYLL:
      gen_hsa_parity (call, hbb);
      break;

    case BUILT_IN_POPCOUNT:
    case BUILT_IN_POPCOUNTL:
    case BUILT_IN_POPCOUNTLL:
      gen_hsa_popcount (call, hbb);
      break;

    case BUILT_IN_ATOMIC_LOAD_1:
    case BUILT_IN_ATOMIC_LOAD_2:
    case BUILT_IN_ATOMIC_LOAD_4:
    case BUILT_IN_ATOMIC_LOAD_8:
    case BUILT_IN_ATOMIC_LOAD_16:
      {
	BrigType16_t mtype;
	hsa_op_address *addr;
	addr = get_address_from_value (gimple_call_arg (stmt, 0), hbb);
	tree model = gimple_call_arg (stmt, 1);
	if (!tree_fits_uhwi_p (model))
	  {
	    HSA_SORRY_ATV (gimple_location (stmt),
			   "support for HSA does not implement "
			   "memory model: %E",
			   model);
	    return;
	  }

	unsigned HOST_WIDE_INT mmodel = tree_to_uhwi (model);
	BrigMemoryOrder memorder = get_memory_order (mmodel,
						     gimple_location (stmt));

	if (memorder == BRIG_MEMORY_ORDER_SC_ACQUIRE_RELEASE)
	  memorder = BRIG_MEMORY_ORDER_SC_ACQUIRE;

	if (memorder != BRIG_MEMORY_ORDER_RELAXED
	    && memorder != BRIG_MEMORY_ORDER_SC_ACQUIRE
	    && memorder != BRIG_MEMORY_ORDER_NONE)
	  {
	    HSA_SORRY_ATV (gimple_location (stmt),
			   "support for HSA does not implement "
			   "memory model for ATOMIC_LD: %s",
			   get_memory_order_name (mmodel));
	    return;
	  }

	if (lhs)
	  {
	    BrigType16_t t = hsa_type_for_scalar_tree_type (TREE_TYPE (lhs),
							    false);
	    mtype = mem_type_for_type (t);
	    mtype = hsa_bittype_for_type (mtype);
	    dest = hsa_cfun->reg_for_gimple_ssa (lhs);
	  }
	else
	  {
	    mtype = BRIG_TYPE_B64;
	    dest = new hsa_op_reg (mtype);
	  }

	hsa_insn_atomic *atominsn
	  = new hsa_insn_atomic (2, BRIG_OPCODE_ATOMIC, BRIG_ATOMIC_LD, mtype,
				 memorder, dest, addr);

	hbb->append_insn (atominsn);
	break;
      }

    case BUILT_IN_ATOMIC_EXCHANGE_1:
    case BUILT_IN_ATOMIC_EXCHANGE_2:
    case BUILT_IN_ATOMIC_EXCHANGE_4:
    case BUILT_IN_ATOMIC_EXCHANGE_8:
    case BUILT_IN_ATOMIC_EXCHANGE_16:
      gen_hsa_ternary_atomic_for_builtin (true, BRIG_ATOMIC_EXCH, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_FETCH_ADD_1:
    case BUILT_IN_ATOMIC_FETCH_ADD_2:
    case BUILT_IN_ATOMIC_FETCH_ADD_4:
    case BUILT_IN_ATOMIC_FETCH_ADD_8:
    case BUILT_IN_ATOMIC_FETCH_ADD_16:
      gen_hsa_ternary_atomic_for_builtin (true, BRIG_ATOMIC_ADD, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_FETCH_SUB_1:
    case BUILT_IN_ATOMIC_FETCH_SUB_2:
    case BUILT_IN_ATOMIC_FETCH_SUB_4:
    case BUILT_IN_ATOMIC_FETCH_SUB_8:
    case BUILT_IN_ATOMIC_FETCH_SUB_16:
      gen_hsa_ternary_atomic_for_builtin (true, BRIG_ATOMIC_SUB, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_FETCH_AND_1:
    case BUILT_IN_ATOMIC_FETCH_AND_2:
    case BUILT_IN_ATOMIC_FETCH_AND_4:
    case BUILT_IN_ATOMIC_FETCH_AND_8:
    case BUILT_IN_ATOMIC_FETCH_AND_16:
      gen_hsa_ternary_atomic_for_builtin (true, BRIG_ATOMIC_AND, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_FETCH_XOR_1:
    case BUILT_IN_ATOMIC_FETCH_XOR_2:
    case BUILT_IN_ATOMIC_FETCH_XOR_4:
    case BUILT_IN_ATOMIC_FETCH_XOR_8:
    case BUILT_IN_ATOMIC_FETCH_XOR_16:
      gen_hsa_ternary_atomic_for_builtin (true, BRIG_ATOMIC_XOR, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_FETCH_OR_1:
    case BUILT_IN_ATOMIC_FETCH_OR_2:
    case BUILT_IN_ATOMIC_FETCH_OR_4:
    case BUILT_IN_ATOMIC_FETCH_OR_8:
    case BUILT_IN_ATOMIC_FETCH_OR_16:
      gen_hsa_ternary_atomic_for_builtin (true, BRIG_ATOMIC_OR, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_STORE_1:
    case BUILT_IN_ATOMIC_STORE_2:
    case BUILT_IN_ATOMIC_STORE_4:
    case BUILT_IN_ATOMIC_STORE_8:
    case BUILT_IN_ATOMIC_STORE_16:
      /* Since there cannot be any LHS, the first parameter is meaningless.  */
      gen_hsa_ternary_atomic_for_builtin (true, BRIG_ATOMIC_ST, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_ADD_FETCH_1:
    case BUILT_IN_ATOMIC_ADD_FETCH_2:
    case BUILT_IN_ATOMIC_ADD_FETCH_4:
    case BUILT_IN_ATOMIC_ADD_FETCH_8:
    case BUILT_IN_ATOMIC_ADD_FETCH_16:
      gen_hsa_ternary_atomic_for_builtin (false, BRIG_ATOMIC_ADD, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_SUB_FETCH_1:
    case BUILT_IN_ATOMIC_SUB_FETCH_2:
    case BUILT_IN_ATOMIC_SUB_FETCH_4:
    case BUILT_IN_ATOMIC_SUB_FETCH_8:
    case BUILT_IN_ATOMIC_SUB_FETCH_16:
      gen_hsa_ternary_atomic_for_builtin (false, BRIG_ATOMIC_SUB, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_AND_FETCH_1:
    case BUILT_IN_ATOMIC_AND_FETCH_2:
    case BUILT_IN_ATOMIC_AND_FETCH_4:
    case BUILT_IN_ATOMIC_AND_FETCH_8:
    case BUILT_IN_ATOMIC_AND_FETCH_16:
      gen_hsa_ternary_atomic_for_builtin (false, BRIG_ATOMIC_AND, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_XOR_FETCH_1:
    case BUILT_IN_ATOMIC_XOR_FETCH_2:
    case BUILT_IN_ATOMIC_XOR_FETCH_4:
    case BUILT_IN_ATOMIC_XOR_FETCH_8:
    case BUILT_IN_ATOMIC_XOR_FETCH_16:
      gen_hsa_ternary_atomic_for_builtin (false, BRIG_ATOMIC_XOR, stmt, hbb);
      break;

    case BUILT_IN_ATOMIC_OR_FETCH_1:
    case BUILT_IN_ATOMIC_OR_FETCH_2:
    case BUILT_IN_ATOMIC_OR_FETCH_4:
    case BUILT_IN_ATOMIC_OR_FETCH_8:
    case BUILT_IN_ATOMIC_OR_FETCH_16:
      gen_hsa_ternary_atomic_for_builtin (false, BRIG_ATOMIC_OR, stmt, hbb);
      break;

    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_16:
      {
	/* TODO: Use the appropriate memory model for now.  */
	tree type = TREE_TYPE (gimple_call_arg (stmt, 1));

	BrigType16_t atype
	  = hsa_bittype_for_type (hsa_type_for_scalar_tree_type (type, false));

	hsa_insn_atomic *atominsn
	  = new hsa_insn_atomic (4, BRIG_OPCODE_ATOMIC, BRIG_ATOMIC_CAS, atype,
				 BRIG_MEMORY_ORDER_SC_ACQUIRE_RELEASE);
	hsa_op_address *addr;
	addr = get_address_from_value (gimple_call_arg (stmt, 0), hbb);

	if (lhs != NULL)
	  dest = hsa_cfun->reg_for_gimple_ssa (lhs);
	else
	  dest = new hsa_op_reg (atype);

	/* Should check what the memory scope is.  */
	atominsn->m_memoryscope = BRIG_MEMORY_SCOPE_WORKGROUP;
	atominsn->set_op (0, dest);
	atominsn->set_op (1, addr);

	hsa_op_with_type *op
	  = hsa_reg_or_immed_for_gimple_op (gimple_call_arg (stmt, 1), hbb);
	atominsn->set_op (2, op);
	op = hsa_reg_or_immed_for_gimple_op (gimple_call_arg (stmt, 2), hbb);
	atominsn->set_op (3, op);

	hbb->append_insn (atominsn);
	break;
      }
    case BUILT_IN_GOMP_PARALLEL:
      HSA_SORRY_AT (gimple_location (stmt),
		    "support for HSA does not implement non-gridified "
		    "OpenMP parallel constructs.");
      break;
    case BUILT_IN_OMP_GET_THREAD_NUM:
      {
	query_hsa_grid (stmt, BRIG_OPCODE_WORKITEMABSID, 0, hbb);
	break;
      }

    case BUILT_IN_OMP_GET_NUM_THREADS:
      {
	query_hsa_grid (stmt, BRIG_OPCODE_GRIDSIZE, 0, hbb);
	break;
      }
    case BUILT_IN_GOMP_TEAMS:
      {
	gen_set_num_threads (gimple_call_arg (stmt, 1), hbb);
	break;
      }
    case BUILT_IN_OMP_GET_NUM_TEAMS:
      {
	gen_get_num_teams (stmt, hbb);
	break;
      }
    case BUILT_IN_OMP_GET_TEAM_NUM:
      {
	gen_get_team_num (stmt, hbb);
	break;
      }
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMPCPY:
      {
	tree byte_size = gimple_call_arg (stmt, 2);

	if (!tree_fits_uhwi_p (byte_size))
	  {
	    gen_hsa_insns_for_direct_call (stmt, hbb);
	    return;
	  }

	unsigned n = tree_to_uhwi (byte_size);

	if (n > HSA_MEMORY_BUILTINS_LIMIT)
	  {
	    gen_hsa_insns_for_direct_call (stmt, hbb);
	    return;
	  }

	tree dst = gimple_call_arg (stmt, 0);
	tree src = gimple_call_arg (stmt, 1);

	hsa_op_address *dst_addr = get_address_from_value (dst, hbb);
	hsa_op_address *src_addr = get_address_from_value (src, hbb);

	gen_hsa_memory_copy (hbb, dst_addr, src_addr, n);

	tree lhs = gimple_call_lhs (stmt);
	if (lhs)
	  {
	    hsa_op_reg *lhs_reg = hsa_cfun->reg_for_gimple_ssa (lhs);
	    hsa_op_with_type *dst_reg = hsa_reg_or_immed_for_gimple_op (dst,
									hbb);
	    hsa_op_with_type *tmp;

	    if (builtin == BUILT_IN_MEMPCPY)
	      {
		tmp = new hsa_op_reg (dst_reg->m_type);
		hsa_insn_basic *add
		  = new hsa_insn_basic (3, BRIG_OPCODE_ADD, tmp->m_type,
					tmp, dst_reg,
					new hsa_op_immed (n, dst_reg->m_type));
		hbb->append_insn (add);
	      }
	    else
	      tmp = dst_reg;

	    hsa_build_append_simple_mov (lhs_reg, tmp, hbb);
	  }

	break;
      }
    case BUILT_IN_MEMSET:
      {
	tree dst = gimple_call_arg (stmt, 0);
	tree c = gimple_call_arg (stmt, 1);

	if (TREE_CODE (c) != INTEGER_CST)
	  {
	    gen_hsa_insns_for_direct_call (stmt, hbb);
	    return;
	  }

	tree byte_size = gimple_call_arg (stmt, 2);

	if (!tree_fits_uhwi_p (byte_size))
	  {
	    gen_hsa_insns_for_direct_call (stmt, hbb);
	    return;
	  }

	unsigned n = tree_to_uhwi (byte_size);

	if (n > HSA_MEMORY_BUILTINS_LIMIT)
	  {
	    gen_hsa_insns_for_direct_call (stmt, hbb);
	    return;
	  }

	hsa_op_address *dst_addr;
	dst_addr = get_address_from_value (dst, hbb);
	unsigned HOST_WIDE_INT constant
	  = tree_to_uhwi (fold_convert (unsigned_char_type_node, c));

	gen_hsa_memory_set (hbb, dst_addr, constant, n);

	tree lhs = gimple_call_lhs (stmt);
	if (lhs)
	  gen_hsa_insns_for_single_assignment (lhs, dst, hbb);

	break;
      }
    case BUILT_IN_BZERO:
      {
	tree dst = gimple_call_arg (stmt, 0);
	tree byte_size = gimple_call_arg (stmt, 1);

	if (!tree_fits_uhwi_p (byte_size))
	  {
	    gen_hsa_insns_for_direct_call (stmt, hbb);
	    return;
	  }

	unsigned n = tree_to_uhwi (byte_size);

	if (n > HSA_MEMORY_BUILTINS_LIMIT)
	  {
	    gen_hsa_insns_for_direct_call (stmt, hbb);
	    return;
	  }

	hsa_op_address *dst_addr;
	dst_addr = get_address_from_value (dst, hbb);

	gen_hsa_memory_set (hbb, dst_addr, 0, n);

	break;
      }
    case BUILT_IN_ALLOCA:
    case BUILT_IN_ALLOCA_WITH_ALIGN:
      {
	gen_hsa_alloca (call, hbb);
	break;
      }
    default:
      {
	gen_hsa_insns_for_direct_call (stmt, hbb);
	return;
      }
    }
}

/* Generate HSA instructions for a given gimple statement.  Instructions will be
   appended to HBB.  */

static void
gen_hsa_insns_for_gimple_stmt (gimple *stmt, hsa_bb *hbb)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      if (gimple_clobber_p (stmt))
	break;

      if (gimple_assign_single_p (stmt))
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  tree rhs = gimple_assign_rhs1 (stmt);
	  gen_hsa_insns_for_single_assignment (lhs, rhs, hbb);
	}
      else
	gen_hsa_insns_for_operation_assignment (stmt, hbb);
      break;
    case GIMPLE_RETURN:
      gen_hsa_insns_for_return (as_a <greturn *> (stmt), hbb);
      break;
    case GIMPLE_COND:
      gen_hsa_insns_for_cond_stmt (stmt, hbb);
      break;
    case GIMPLE_CALL:
      gen_hsa_insns_for_call (stmt, hbb);
      break;
    case GIMPLE_DEBUG:
      /* ??? HSA supports some debug facilities.  */
      break;
    case GIMPLE_LABEL:
    {
      tree label = gimple_label_label (as_a <glabel *> (stmt));
      if (FORCED_LABEL (label))
	HSA_SORRY_AT (gimple_location (stmt),
		      "support for HSA does not implement gimple label with "
		      "address taken");

      break;
    }
    case GIMPLE_NOP:
    {
      hbb->append_insn (new hsa_insn_basic (0, BRIG_OPCODE_NOP));
      break;
    }
    case GIMPLE_SWITCH:
    {
      gen_hsa_insns_for_switch_stmt (as_a <gswitch *> (stmt), hbb);
      break;
    }
    default:
      HSA_SORRY_ATV (gimple_location (stmt),
		     "support for HSA does not implement gimple statement %s",
		     gimple_code_name[(int) gimple_code (stmt)]);
    }
}

/* Generate a HSA PHI from a gimple PHI.  */

static void
gen_hsa_phi_from_gimple_phi (gimple *phi_stmt, hsa_bb *hbb)
{
  hsa_insn_phi *hphi;
  unsigned count = gimple_phi_num_args (phi_stmt);

  hsa_op_reg *dest
    = hsa_cfun->reg_for_gimple_ssa (gimple_phi_result (phi_stmt));
  hphi = new hsa_insn_phi (count, dest);
  hphi->m_bb = hbb->m_bb;

  tree lhs = gimple_phi_result (phi_stmt);

  for (unsigned i = 0; i < count; i++)
    {
      tree op = gimple_phi_arg_def (phi_stmt, i);

      if (TREE_CODE (op) == SSA_NAME)
	{
	  hsa_op_reg *hreg = hsa_cfun->reg_for_gimple_ssa (op);
	  hphi->set_op (i, hreg);
	}
      else
	{
	  gcc_assert (is_gimple_min_invariant (op));
	  tree t = TREE_TYPE (op);
	  if (!POINTER_TYPE_P (t)
	      || (TREE_CODE (op) == STRING_CST
		  && TREE_CODE (TREE_TYPE (t)) == INTEGER_TYPE))
	    hphi->set_op (i, new hsa_op_immed (op));
	  else if (POINTER_TYPE_P (TREE_TYPE (lhs))
		   && TREE_CODE (op) == INTEGER_CST)
	    {
	      /* Handle assignment of NULL value to a pointer type.  */
	      hphi->set_op (i, new hsa_op_immed (op));
	    }
	  else if (TREE_CODE (op) == ADDR_EXPR)
	    {
	      edge e = gimple_phi_arg_edge (as_a <gphi *> (phi_stmt), i);
	      hsa_bb *hbb_src = hsa_init_new_bb (split_edge (e));
	      hsa_op_address *addr = gen_hsa_addr (TREE_OPERAND (op, 0),
						   hbb_src);

	      hsa_op_reg *dest
		= new hsa_op_reg (hsa_get_segment_addr_type (BRIG_SEGMENT_FLAT));
	      hsa_insn_basic *insn
		= new hsa_insn_basic (2, BRIG_OPCODE_LDA, BRIG_TYPE_U64,
				      dest, addr);
	      hbb_src->append_insn (insn);

	      hphi->set_op (i, dest);
	    }
	  else
	    {
	      HSA_SORRY_AT (gimple_location (phi_stmt),
			    "support for HSA does not handle PHI nodes with "
			    "constant address operands");
	      return;
	    }
	}
    }

  hphi->m_prev = hbb->m_last_phi;
  hphi->m_next = NULL;
  if (hbb->m_last_phi)
    hbb->m_last_phi->m_next = hphi;
  hbb->m_last_phi = hphi;
  if (!hbb->m_first_phi)
    hbb->m_first_phi = hphi;
}

/* Constructor of class containing HSA-specific information about a basic
   block.  CFG_BB is the CFG BB this HSA BB is associated with.  IDX is the new
   index of this BB (so that the constructor does not attempt to use
   hsa_cfun during its construction).  */

hsa_bb::hsa_bb (basic_block cfg_bb, int idx)
  : m_bb (cfg_bb), m_first_insn (NULL), m_last_insn (NULL), m_first_phi (NULL),
    m_last_phi (NULL), m_index (idx), m_liveout (BITMAP_ALLOC (NULL)),
    m_livein (BITMAP_ALLOC (NULL))
{
  gcc_assert (!cfg_bb->aux);
  cfg_bb->aux = this;
}

/* Constructor of class containing HSA-specific information about a basic
   block.  CFG_BB is the CFG BB this HSA BB is associated with.  */

hsa_bb::hsa_bb (basic_block cfg_bb)
  : m_bb (cfg_bb), m_first_insn (NULL), m_last_insn (NULL), m_first_phi (NULL),
    m_last_phi (NULL), m_index (hsa_cfun->m_hbb_count++),
    m_liveout (BITMAP_ALLOC (NULL)), m_livein (BITMAP_ALLOC (NULL))
{
  gcc_assert (!cfg_bb->aux);
  cfg_bb->aux = this;
}

/* Destructor of class representing HSA BB.  */

hsa_bb::~hsa_bb ()
{
  BITMAP_FREE (m_livein);
  BITMAP_FREE (m_liveout);
}

/* Create and initialize and return a new hsa_bb structure for a given CFG
   basic block BB.  */

hsa_bb *
hsa_init_new_bb (basic_block bb)
{
  return new (*hsa_allocp_bb) hsa_bb (bb);
}

/* Initialize OMP in an HSA basic block PROLOGUE.  */

static void
init_prologue (void)
{
  if (!hsa_cfun->m_kern_p)
    return;

  hsa_bb *prologue = hsa_bb_for_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* Create a magic number that is going to be printed by libgomp.  */
  unsigned index = hsa_get_number_decl_kernel_mappings ();

  /* Emit store to debug argument.  */
  if (PARAM_VALUE (PARAM_HSA_GEN_DEBUG_STORES) > 0)
    set_debug_value (prologue, new hsa_op_immed (1000 + index, BRIG_TYPE_U64));
}

/* Initialize hsa_num_threads to a default value.  */

static void
init_hsa_num_threads (void)
{
  hsa_bb *prologue = hsa_bb_for_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* Save the default value to private variable hsa_num_threads.  */
  hsa_insn_basic *basic
    = new hsa_insn_mem (BRIG_OPCODE_ST, hsa_num_threads->m_type,
			new hsa_op_immed (0, hsa_num_threads->m_type),
			new hsa_op_address (hsa_num_threads));
  prologue->append_insn (basic);
}

/* Go over gimple representation and generate our internal HSA one.  */

static void
gen_body_from_gimple ()
{
  basic_block bb;

  /* Verify CFG for complex edges we are unable to handle.  */
  edge_iterator ei;
  edge e;

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  /* Verify all unsupported flags for edges that point
	     to the same basic block.  */
	  if (e->flags & EDGE_EH)
	    {
	      HSA_SORRY_AT (UNKNOWN_LOCATION,
			    "support for HSA does not implement exception "
			    "handling");
	      return;
	    }
	}
    }

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      hsa_bb *hbb = hsa_bb_for_bb (bb);
      if (hbb)
	continue;

      hbb = hsa_init_new_bb (bb);

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gen_hsa_insns_for_gimple_stmt (gsi_stmt (gsi), hbb);
	  if (hsa_seen_error ())
	    return;
	}
    }

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      hsa_bb *hbb = hsa_bb_for_bb (bb);
      gcc_assert (hbb != NULL);

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	if (!virtual_operand_p (gimple_phi_result (gsi_stmt (gsi))))
	  gen_hsa_phi_from_gimple_phi (gsi_stmt (gsi), hbb);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "------- Generated SSA form -------\n");
      dump_hsa_cfun (dump_file);
    }
}

static void
gen_function_decl_parameters (hsa_function_representation *f,
			      tree decl)
{
  tree parm;
  unsigned i;

  for (parm = TYPE_ARG_TYPES (TREE_TYPE (decl)), i = 0;
       parm;
       parm = TREE_CHAIN (parm), i++)
    {
      /* Result type if last in the tree list.  */
      if (TREE_CHAIN (parm) == NULL)
	break;

      tree v = TREE_VALUE (parm);

      hsa_symbol *arg = new hsa_symbol (BRIG_TYPE_NONE, BRIG_SEGMENT_ARG,
					BRIG_LINKAGE_NONE);
      arg->m_type = hsa_type_for_tree_type (v, &arg->m_dim);
      arg->m_name_number = i;

      f->m_input_args.safe_push (arg);
    }

  tree result_type = TREE_TYPE (TREE_TYPE (decl));
  if (!VOID_TYPE_P (result_type))
    {
      f->m_output_arg = new hsa_symbol (BRIG_TYPE_NONE, BRIG_SEGMENT_ARG,
					BRIG_LINKAGE_NONE);
      f->m_output_arg->m_type
	= hsa_type_for_tree_type (result_type, &f->m_output_arg->m_dim);
      f->m_output_arg->m_name = "res";
    }
}

/* Generate the vector of parameters of the HSA representation of the current
   function.  This also includes the output parameter representing the
   result.  */

static void
gen_function_def_parameters ()
{
  tree parm;

  hsa_bb *prologue = hsa_bb_for_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  for (parm = DECL_ARGUMENTS (cfun->decl); parm;
       parm = DECL_CHAIN (parm))
    {
      struct hsa_symbol **slot;

      hsa_symbol *arg
	= new hsa_symbol (BRIG_TYPE_NONE, hsa_cfun->m_kern_p
			  ? BRIG_SEGMENT_KERNARG : BRIG_SEGMENT_ARG,
			  BRIG_LINKAGE_FUNCTION);
      arg->fillup_for_decl (parm);

      hsa_cfun->m_input_args.safe_push (arg);

      if (hsa_seen_error ())
	return;

      arg->m_name = hsa_get_declaration_name (parm);

      /* Copy all input arguments and create corresponding private symbols
	 for them.  */
      hsa_symbol *private_arg;
      hsa_op_address *parm_addr = new hsa_op_address (arg);

      if (TREE_ADDRESSABLE (parm)
	  || (!is_gimple_reg (parm) && !TREE_READONLY (parm)))
	{
	  private_arg = hsa_cfun->create_hsa_temporary (arg->m_type);
	  private_arg->fillup_for_decl (parm);

	  hsa_op_address *private_arg_addr = new hsa_op_address (private_arg);
	  gen_hsa_memory_copy (prologue, private_arg_addr, parm_addr,
			       arg->total_byte_size ());
	}
      else
	private_arg = arg;

      slot = hsa_cfun->m_local_symbols->find_slot (private_arg, INSERT);
      gcc_assert (!*slot);
      *slot = private_arg;

      if (is_gimple_reg (parm))
	{
	  tree ddef = ssa_default_def (cfun, parm);
	  if (ddef && !has_zero_uses (ddef))
	    {
	      BrigType16_t t = hsa_type_for_scalar_tree_type (TREE_TYPE (ddef),
							      false);
	      BrigType16_t mtype = mem_type_for_type (t);
	      hsa_op_reg *dest = hsa_cfun->reg_for_gimple_ssa (ddef);
	      hsa_insn_mem *mem = new hsa_insn_mem (BRIG_OPCODE_LD, mtype,
						    dest, parm_addr);
	      gcc_assert (!parm_addr->m_reg);
	      prologue->append_insn (mem);
	    }
	}
    }

  if (!VOID_TYPE_P (TREE_TYPE (TREE_TYPE (cfun->decl))))
    {
      struct hsa_symbol **slot;

      hsa_cfun->m_output_arg = new hsa_symbol (BRIG_TYPE_NONE, BRIG_SEGMENT_ARG,
					       BRIG_LINKAGE_FUNCTION);
      hsa_cfun->m_output_arg->fillup_for_decl (DECL_RESULT (cfun->decl));

      if (hsa_seen_error ())
	return;

      hsa_cfun->m_output_arg->m_name = "res";
      slot = hsa_cfun->m_local_symbols->find_slot (hsa_cfun->m_output_arg,
						   INSERT);
      gcc_assert (!*slot);
      *slot = hsa_cfun->m_output_arg;
    }
}

/* Generate function representation that corresponds to
   a function declaration.  */

hsa_function_representation *
hsa_generate_function_declaration (tree decl)
{
  hsa_function_representation *fun
    = new hsa_function_representation (decl, false, 0);

  fun->m_declaration_p = true;
  fun->m_name = get_brig_function_name (decl);
  gen_function_decl_parameters (fun, decl);

  return fun;
}


/* Generate function representation that corresponds to
   an internal FN.  */

hsa_function_representation *
hsa_generate_internal_fn_decl (hsa_internal_fn *fn)
{
  hsa_function_representation *fun = new hsa_function_representation (fn);

  fun->m_name = fn->name ();

  for (unsigned i = 0; i < fn->get_arity (); i++)
    {
      hsa_symbol *arg
	= new hsa_symbol (fn->get_argument_type (i), BRIG_SEGMENT_ARG,
			  BRIG_LINKAGE_NONE);
      arg->m_name_number = i;
      fun->m_input_args.safe_push (arg);
    }

  fun->m_output_arg = new hsa_symbol (fn->get_argument_type (-1),
				      BRIG_SEGMENT_ARG, BRIG_LINKAGE_NONE);
  fun->m_output_arg->m_name = "res";

  return fun;
}

/* Return true if switch statement S can be transformed
   to a SBR instruction in HSAIL.  */

static bool
transformable_switch_to_sbr_p (gswitch *s)
{
  /* Identify if a switch statement can be transformed to
     SBR instruction, like:

     sbr_u32 $s1 [@label1, @label2, @label3];
  */

  tree size = get_switch_size (s);
  if (!tree_fits_uhwi_p (size))
    return false;

  if (tree_to_uhwi (size) > HSA_MAXIMUM_SBR_LABELS)
    return false;

  return true;
}

/* Structure hold connection between PHI nodes and immediate
   values hold by there nodes.  */

struct phi_definition
{
  phi_definition (unsigned phi_i, unsigned label_i, tree imm):
    phi_index (phi_i), label_index (label_i), phi_value (imm)
  {}

  unsigned phi_index;
  unsigned label_index;
  tree phi_value;
};

/* Sum slice of a vector V, starting from index START and ending
   at the index END - 1.  */

template <typename T>
static
T sum_slice (const auto_vec <T> &v, unsigned start, unsigned end)
{
  T s = 0;

  for (unsigned i = start; i < end; i++)
    s += v[i];

  return s;
}

/* Function transforms GIMPLE SWITCH statements to a series of IF statements.
   Let's assume following example:

L0:
   switch (index)
     case C1:
L1:    hard_work_1 ();
       break;
     case C2..C3:
L2:    hard_work_2 ();
       break;
     default:
LD:    hard_work_3 ();
       break;

  The transformation encompasses following steps:
    1) all immediate values used by edges coming from the switch basic block
       are saved
    2) all these edges are removed
    3) the switch statement (in L0) is replaced by:
	 if (index == C1)
	   goto L1;
	 else
	   goto L1';

    4) newly created basic block Lx' is used for generation of
       a next condition
    5) else branch of the last condition goes to LD
    6) fix all immediate values in PHI nodes that were propagated though
       edges that were removed in step 2

  Note: if a case is made by a range C1..C2, then process
	following transformation:

  switch_cond_op1 = C1 <= index;
  switch_cond_op2 = index <= C2;
  switch_cond_and = switch_cond_op1 & switch_cond_op2;
  if (switch_cond_and != 0)
    goto Lx;
  else
    goto Ly;

*/

static void
convert_switch_statements ()
{
  function *func = DECL_STRUCT_FUNCTION (current_function_decl);
  basic_block bb;

  bool need_update = false;

  FOR_EACH_BB_FN (bb, func)
  {
    gimple_stmt_iterator gsi = gsi_last_bb (bb);
    if (gsi_end_p (gsi))
      continue;

    gimple *stmt = gsi_stmt (gsi);

    if (gimple_code (stmt) == GIMPLE_SWITCH)
      {
	gswitch *s = as_a <gswitch *> (stmt);

	/* If the switch can utilize SBR insn, skip the statement.  */
	if (transformable_switch_to_sbr_p (s))
	  continue;

	need_update = true;

	unsigned labels = gimple_switch_num_labels (s);
	tree index = gimple_switch_index (s);
	tree index_type = TREE_TYPE (index);
	tree default_label = gimple_switch_default_label (s);
	basic_block default_label_bb
	  = label_to_block_fn (func, CASE_LABEL (default_label));
	basic_block cur_bb = bb;

	auto_vec <edge> new_edges;
	auto_vec <phi_definition *> phi_todo_list;
	auto_vec <gcov_type> edge_counts;
	auto_vec <int> edge_probabilities;

	/* Investigate all labels that and PHI nodes in these edges which
	   should be fixed after we add new collection of edges.  */
	for (unsigned i = 0; i < labels; i++)
	  {
	    tree label = gimple_switch_label (s, i);
	    basic_block label_bb = label_to_block_fn (func, CASE_LABEL (label));
	    edge e = find_edge (bb, label_bb);
	    edge_counts.safe_push (e->count);
	    edge_probabilities.safe_push (e->probability);
	    gphi_iterator phi_gsi;

	    /* Save PHI definitions that will be destroyed because of an edge
	       is going to be removed.  */
	    unsigned phi_index = 0;
	    for (phi_gsi = gsi_start_phis (e->dest);
		 !gsi_end_p (phi_gsi); gsi_next (&phi_gsi))
	      {
		gphi *phi = phi_gsi.phi ();
		for (unsigned j = 0; j < gimple_phi_num_args (phi); j++)
		  {
		    if (gimple_phi_arg_edge (phi, j) == e)
		      {
			tree imm = gimple_phi_arg_def (phi, j);
			phi_definition *p = new phi_definition (phi_index, i,
								imm);
			phi_todo_list.safe_push (p);
			break;
		      }
		  }
		phi_index++;
	      }
	  }

	/* Remove all edges for the current basic block.  */
	for (int i = EDGE_COUNT (bb->succs) - 1; i >= 0; i--)
 	  {
	    edge e = EDGE_SUCC (bb, i);
	    remove_edge (e);
	  }

	/* Iterate all non-default labels.  */
	for (unsigned i = 1; i < labels; i++)
	  {
	    tree label = gimple_switch_label (s, i);
	    tree low = CASE_LOW (label);
	    tree high = CASE_HIGH (label);

	    if (!useless_type_conversion_p (TREE_TYPE (low), index_type))
	      low = fold_convert (index_type, low);

	    gimple_stmt_iterator cond_gsi = gsi_last_bb (cur_bb);
	    gimple *c = NULL;
	    if (high)
	      {
		tree tmp1 = make_temp_ssa_name (boolean_type_node, NULL,
						"switch_cond_op1");

		gimple *assign1 = gimple_build_assign (tmp1, LE_EXPR, low,
						      index);

		tree tmp2 = make_temp_ssa_name (boolean_type_node, NULL,
						"switch_cond_op2");

		if (!useless_type_conversion_p (TREE_TYPE (high), index_type))
		  high = fold_convert (index_type, high);
		gimple *assign2 = gimple_build_assign (tmp2, LE_EXPR, index,
						      high);

		tree tmp3 = make_temp_ssa_name (boolean_type_node, NULL,
						"switch_cond_and");
		gimple *assign3 = gimple_build_assign (tmp3, BIT_AND_EXPR, tmp1,
						      tmp2);

		gsi_insert_before (&cond_gsi, assign1, GSI_SAME_STMT);
		gsi_insert_before (&cond_gsi, assign2, GSI_SAME_STMT);
		gsi_insert_before (&cond_gsi, assign3, GSI_SAME_STMT);

		tree b = constant_boolean_node (false, boolean_type_node);
		c = gimple_build_cond (NE_EXPR, tmp3, b, NULL, NULL);
	      }
	    else
	      c = gimple_build_cond (EQ_EXPR, index, low, NULL, NULL);

	    gimple_set_location (c, gimple_location (stmt));

	    gsi_insert_before (&cond_gsi, c, GSI_SAME_STMT);

	    basic_block label_bb
	      = label_to_block_fn (func, CASE_LABEL (label));
	    edge new_edge = make_edge (cur_bb, label_bb, EDGE_TRUE_VALUE);
	    int prob_sum = sum_slice <int> (edge_probabilities, i, labels) +
	       edge_probabilities[0];

	    if (prob_sum)
	      new_edge->probability
		= RDIV (REG_BR_PROB_BASE * edge_probabilities[i], prob_sum);

	    new_edge->count = edge_counts[i];
	    new_edges.safe_push (new_edge);

	    if (i < labels - 1)
	      {
		/* Prepare another basic block that will contain
		   next condition.  */
		basic_block next_bb = create_empty_bb (cur_bb);
		if (current_loops)
		  {
		    add_bb_to_loop (next_bb, cur_bb->loop_father);
		    loops_state_set (LOOPS_NEED_FIXUP);
		  }

		edge next_edge = make_edge (cur_bb, next_bb, EDGE_FALSE_VALUE);
		next_edge->probability
		  = inverse_probability (new_edge->probability);
		next_edge->count = edge_counts[0]
		  + sum_slice <gcov_type> (edge_counts, i, labels);
		next_bb->frequency = EDGE_FREQUENCY (next_edge);
		cur_bb = next_bb;
	      }
	    else /* Link last IF statement and default label
		    of the switch.  */
	      {
		edge e = make_edge (cur_bb, default_label_bb, EDGE_FALSE_VALUE);
		e->probability = inverse_probability (new_edge->probability);
		e->count = edge_counts[0];
		new_edges.safe_insert (0, e);
	      }
	  }

	  /* Restore original PHI immediate value.  */
	  for (unsigned i = 0; i < phi_todo_list.length (); i++)
	    {
	      phi_definition *phi_def = phi_todo_list[i];
	      edge new_edge = new_edges[phi_def->label_index];

	      gphi_iterator it = gsi_start_phis (new_edge->dest);
	      for (unsigned i = 0; i < phi_def->phi_index; i++)
		gsi_next (&it);

	      gphi *phi = it.phi ();
	      add_phi_arg (phi, phi_def->phi_value, new_edge, UNKNOWN_LOCATION);
	      delete phi_def;
	    }

	/* Remove the original GIMPLE switch statement.  */
	gsi_remove (&gsi, true);
      }
  }

  if (dump_file)
    dump_function_to_file (current_function_decl, dump_file, TDF_DETAILS);

  if (need_update)
    {
      free_dominance_info (CDI_DOMINATORS);
      calculate_dominance_info (CDI_DOMINATORS);
    }
}

/* Expand builtins that can't be handled by HSA back-end.  */

static void
expand_builtins ()
{
  function *func = DECL_STRUCT_FUNCTION (current_function_decl);
  basic_block bb;

  FOR_EACH_BB_FN (bb, func)
  {
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	 gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);

	if (gimple_code (stmt) != GIMPLE_CALL)
	  continue;

	gcall *call = as_a <gcall *> (stmt);

	if (!gimple_call_builtin_p (call, BUILT_IN_NORMAL))
	  continue;

	tree fndecl = gimple_call_fndecl (stmt);
	enum built_in_function fn = DECL_FUNCTION_CODE (fndecl);
	switch (fn)
	  {
	  case BUILT_IN_CEXPF:
	  case BUILT_IN_CEXPIF:
	  case BUILT_IN_CEXPI:
	    {
	      /* Similar to builtins.c (expand_builtin_cexpi), the builtin
		 can be transformed to: cexp(I * z) = ccos(z) + I * csin(z).  */
	      tree lhs = gimple_call_lhs (stmt);
	      tree rhs = gimple_call_arg (stmt, 0);
	      tree rhs_type = TREE_TYPE (rhs);
	      bool float_type_p = rhs_type == float_type_node;
	      tree real_part = make_temp_ssa_name (rhs_type, NULL,
						   "cexp_real_part");
	      tree imag_part = make_temp_ssa_name (rhs_type, NULL,
						   "cexp_imag_part");

	      tree cos_fndecl
		= mathfn_built_in (rhs_type, fn == float_type_p
				   ? BUILT_IN_COSF : BUILT_IN_COS);
	      gcall *cos = gimple_build_call (cos_fndecl, 1, rhs);
	      gimple_call_set_lhs (cos, real_part);
	      gsi_insert_before (&gsi, cos, GSI_SAME_STMT);

	      tree sin_fndecl
		= mathfn_built_in (rhs_type, fn == float_type_p
				   ? BUILT_IN_SINF : BUILT_IN_SIN);
	      gcall *sin = gimple_build_call (sin_fndecl, 1, rhs);
	      gimple_call_set_lhs (sin, imag_part);
	      gsi_insert_before (&gsi, sin, GSI_SAME_STMT);


	      gassign *assign = gimple_build_assign (lhs, COMPLEX_EXPR,
						     real_part, imag_part);
	      gsi_insert_before (&gsi, assign, GSI_SAME_STMT);
	      gsi_remove (&gsi, true);

	      break;
	    }
	  default:
	    break;
	  }
      }
  }
}

/* Emit HSA module variables that are global for the entire module.  */

static void
emit_hsa_module_variables (void)
{
  hsa_num_threads = new hsa_symbol (BRIG_TYPE_U32, BRIG_SEGMENT_PRIVATE,
				    BRIG_LINKAGE_MODULE, true);

  hsa_num_threads->m_name = "hsa_num_threads";

  hsa_brig_emit_omp_symbols ();
}

/* Generate HSAIL representation of the current function and write into a
   special section of the output file.  If KERNEL is set, the function will be
   considered an HSA kernel callable from the host, otherwise it will be
   compiled as an HSA function callable from other HSA code.  */

static void
generate_hsa (bool kernel)
{
  hsa_init_data_for_cfun ();

  if (hsa_num_threads == NULL)
    emit_hsa_module_variables ();

  /* Initialize hsa_cfun.  */
  hsa_cfun = new hsa_function_representation (cfun->decl, kernel,
					      SSANAMES (cfun)->length ());
  hsa_cfun->init_extra_bbs ();

  if (flag_tm)
    {
      HSA_SORRY_AT (UNKNOWN_LOCATION,
		    "support for HSA does not implement transactional memory");
      goto fail;
    }

  verify_function_arguments (cfun->decl);
  if (hsa_seen_error ())
    goto fail;

  hsa_cfun->m_name = get_brig_function_name (cfun->decl);

  gen_function_def_parameters ();
  if (hsa_seen_error ())
    goto fail;

  init_prologue ();

  gen_body_from_gimple ();
  if (hsa_seen_error ())
    goto fail;

  if (hsa_cfun->m_kernel_dispatch_count)
    init_hsa_num_threads ();

  if (hsa_cfun->m_kern_p)
    {
      hsa_function_summary *s
	= hsa_summaries->get (cgraph_node::get (hsa_cfun->m_decl));
      hsa_add_kern_decl_mapping (current_function_decl, hsa_cfun->m_name,
				 hsa_cfun->m_maximum_omp_data_size,
				 s->m_gridified_kernel_p);
    }

  if (flag_checking)
    {
      for (unsigned i = 0; i < hsa_cfun->m_ssa_map.length (); i++)
	if (hsa_cfun->m_ssa_map[i])
	  hsa_cfun->m_ssa_map[i]->verify_ssa ();

      basic_block bb;
      FOR_EACH_BB_FN (bb, cfun)
	{
	  hsa_bb *hbb = hsa_bb_for_bb (bb);

	  for (hsa_insn_basic *insn = hbb->m_first_insn; insn;
	       insn = insn->m_next)
	    insn->verify ();
	}
    }

  hsa_regalloc ();
  hsa_brig_emit_function ();

 fail:
  hsa_deinit_data_for_cfun ();
}

namespace {

const pass_data pass_data_gen_hsail =
{
  GIMPLE_PASS,
  "hsagen",	 			/* name */
  OPTGROUP_NONE,			/* optinfo_flags */
  TV_NONE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
};

class pass_gen_hsail : public gimple_opt_pass
{
public:
  pass_gen_hsail (gcc::context *ctxt)
    : gimple_opt_pass(pass_data_gen_hsail, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *);
  unsigned int execute (function *);

}; // class pass_gen_hsail

/* Determine whether or not to run generation of HSAIL.  */

bool
pass_gen_hsail::gate (function *f)
{
  return hsa_gen_requested_p ()
    && hsa_gpu_implementation_p (f->decl);
}

unsigned int
pass_gen_hsail::execute (function *)
{
  hsa_function_summary *s
    = hsa_summaries->get (cgraph_node::get_create (current_function_decl));

  convert_switch_statements ();
  expand_builtins ();
  generate_hsa (s->m_kind == HSA_KERNEL);
  TREE_ASM_WRITTEN (current_function_decl) = 1;
  return TODO_discard_function;
}

} // anon namespace

/* Create the instance of hsa gen pass.  */

gimple_opt_pass *
make_pass_gen_hsail (gcc::context *ctxt)
{
  return new pass_gen_hsail (ctxt);
}
