/* Define configuration-related items for UPC
   Copyright (C) 2004 Free Software Foundation, Inc.
   Original Implementation by Jesse M. Draper <jdraper@super.org>
   and William W. Carlson <wwc@super.org>.
   Extended, and ported to other architectures by
   Gary Funck <gary@intrepid.com> and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* helper macros for expanding the value of a macro as a string. */
#define __UPC_STR__(S) #S
#define __UPC_XSTR__(S) __UPC_STR__(S)

/* Enable parsing of UPC pragma */
#define HANDLE_PRAGMA_UPC 1
/* Enable parsing of PUPC pragma */
#define HANDLE_PRAGMA_PUPC 1

#ifndef UPC_PTS_VADDR_FIRST
#define UPC_PTS_VADDR_FIRST 1
#endif

#if defined(UPC_PTS_PACKED_REP)

/* Defaults for packed pointer representation */

#ifndef UPC_PTS_SIZE
#define UPC_PTS_SIZE            64
#define UPC_PTS_THREAD_SIZE     12
#define UPC_PTS_PHASE_SIZE      16
#define UPC_PTS_VADDR_SIZE      36
#if UPC_PTS_VADDR_FIRST
#define UPC_PTS_PHASE_SHIFT     0
#define UPC_PTS_THREAD_SHIFT    UPC_PTS_PHASE_SIZE
#define UPC_PTS_VADDR_SHIFT     UPC_PTS_PHASE_SIZE+UPC_PTS_THREAD_SIZE
#else
#define UPC_PTS_VADDR_SHIFT     0
#define UPC_PTS_THREAD_SHIFT    UPC_PTS_VADDR_SIZE
#define UPC_PTS_PHASE_SHIFT     UPC_PTS_VADDR_SIZE+UPC_PTS_THREAD_SIZE
#endif
#endif

#define UPC_MAX_BLOCK_SIZE_STRING __UPC_XSTR__(UPC_MAX_BLOCK_SIZE)
#define UPC_MAX_BLOCK_SIZE_CSTU build_int_cstu (long_unsigned_type_node, UPC_MAX_BLOCK_SIZE)

#elif defined(UPC_PTS_STRUCT_REP)

/* Define structure shared pointer representation. */
#define UPC_PTS_SIZE            (LONG_TYPE_SIZE + POINTER_SIZE)
#define UPC_PTS_THREAD_SIZE     (LONG_TYPE_SIZE/2)
#define UPC_PTS_PHASE_SIZE      (LONG_TYPE_SIZE/2)
#define UPC_PTS_VADDR_SIZE      POINTER_SIZE
#define UPC_PTS_VADDR_TYPE      "char *"
#define UPC_PTS_THREAD_TYPE     ((LONG_TYPE_SIZE == 64) ? "uint32_t" : "uint16_t")
#define UPC_PTS_PHASE_TYPE      ((LONG_TYPE_SIZE == 64) ? "uint32_t" : "uint16_t")
#define UPC_MAX_BLOCK_SIZE_CSTU build_int_cstu (long_unsigned_type_node, \
                                (LONG_TYPE_SIZE == 64) ? 0xffffffff : 0xffff)
#define UPC_MAX_BLOCK_SIZE_STRING ((LONG_TYPE_SIZE == 64) ? "4294967295" : "65535")
#define UPC_MAX_THREADS           ((LONG_TYPE_SIZE == 64) ? 2147483647 : 65536)

#else
/* UPC shared pointer representaton not specified. */
#endif

/* Define UPC linker and runtime names and constructs */

#ifdef TARGET_IRIX
/* Linker switches passed via the "upc" command. */
#define UPC_LINKER_SWITCHES "-Wl,-LD_MSG:off=84 -lm"
#endif

/* Name of section used to assign addresses to shared data items. */
#ifndef UPC_SHARED_SECTION_NAME
#define UPC_SHARED_SECTION_NAME "upc_shared"
#endif
/* Used by upc-crtbegin to define the beginning of the shared section */
#define UPC_SHARED_BEGIN_NAME __upc_shared_start
#define UPC_SHARED_BEGIN_NAME_STR "__upc_shared_start"
/* Used by upc-crtend to define the end of the shared section */
#define UPC_SHARED_END_NAME __upc_shared_end

/* Name of section used to hold info. describing how a UPC source file was compiled. */
#ifndef UPC_PGM_INFO_SECTION_NAME
#define UPC_PGM_INFO_SECTION_NAME "upc_pgm_info"
#endif
/* Used by upc-crtbegin to define the beginning of the shared section */
#define UPC_PGM_INFO_BEGIN_NAME __upc_pgm_info_start
/* Used by upc-crtend to define the end of the shared section */
#define UPC_PGM_INFO_END_NAME __upc_pgm_info_end

/* Name of section where UPC iniitialization routines are located.  */
#ifndef UPC_INIT_SECTION_NAME
#define UPC_INIT_SECTION_NAME "upc_init"
#endif
/* Used by upc-crtbegin to define the beginning of init. routines section */
#define UPC_INIT_BEGIN_NAME __upc_init_start
/* Used by upc-crtend to define the beginning of init. routines section */
#define UPC_INIT_END_NAME __upc_init_end

/* Name of section that holds an array of addresses that points to 
   the UPC initialization routiones */
#ifndef UPC_INIT_ARRAY_SECTION_NAME
#define UPC_INIT_ARRAY_SECTION_NAME "upc_init_array"
#endif
/* Used by upc-crtbegin to define the beginning of UPC init. array ection.  */
#define UPC_INIT_ARRAY_BEGIN_NAME __upc_init_array_start
/* Used by upc-crtend to define the beginning of UPC init. array section.  */
#define UPC_INIT_ARRAY_END_NAME __upc_init_array_end

/* Name of initialization routine that is called to initialize
   shared variables and calculate shared address.  Both of these
   operations must be performed at runtime before UPC's main
   program is called.  */
#define UPC_INIT_DECLS_FUNC "__upc_init_decls"

/* Name of runtime variable that is used by the code generated
   for the 'upc_forall' statement to implement nested upc_forall
   semantics.  Per the language specification, a nested upc_forall
   statement with an affinity clause will operate as if "continue"
   had been supplied for the affinity clause.  */
#define UPC_FORALL_DEPTH_NAME "__upc_forall_depth"

/* the name of the runtime variable holding the address of the beginning of
   the global shared region. */
#define UPC_GLOBAL_BASE_NAME "__upc_global_base"

/* the name of the runtime variable holding the calculated size of the 
   (per-thread) contribution to the global shared memory. */
#define UPC_LOCAL_SIZE_NAME "__upc_local_size"

/* Names of various UPC runtime library routines that implement various
   UPC statement constructs. */
#define UPC_BARRIER_LIBCALL "__upc_barrier"
#define UPC_GETADDR_LIBCALL "__getaddr"
#define UPC_NOTIFY_LIBCALL "__upc_notify"
#define UPC_WAIT_LIBCALL "__upc_wait"

/* Profiled runtime library routines  */
#define UPC_BARRIERG_LIBCALL "__upc_barrierg"
#define UPC_GETADDRG_LIBCALL "__getaddrg"
#define UPC_NOTIFYG_LIBCALL "__upc_notifyg"
#define UPC_WAITG_LIBCALL "__upc_waitg"

/* Runtime library function that records upc_forall begin/end
   when -fupc-instrument is asserted.  */
#define UPC_INSTRUMENT_FORALL "__upc_forallg"

/* Runtime library function that records function entry/exit 
   when -fupc-instrument-functions is asserted.  */
#define UPC_INSTRUMENT_FUNC "__upc_funcg"
