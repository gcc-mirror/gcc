/* brig-atomic-inst-handler.cc -- brig atomic instruction handling
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.
   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include <sstream>

#include "brig-code-entry-handler.h"
#include "brig-util.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "print-tree.h"
#include "convert.h"
#include "langhooks.h"
#include "gimple-expr.h"
#include "stringpool.h"
#include "brig-builtins.h"

brig_atomic_inst_handler::brig_atomic_inst_handler (brig_to_generic &parent)
  : brig_code_entry_handler (parent)
{
}

size_t
brig_atomic_inst_handler::generate_tree (const BrigInstBase &inst,
					 BrigAtomicOperation8_t atomic_opcode)
{
  tree_stl_vec operands = build_operands (inst);
  const int first_input
    = gccbrig_hsa_opcode_op_output_p (inst.opcode, 0) ? 1 : 0;

  tree instr_type = gccbrig_tree_type_for_hsa_type (inst.type);

  /* Utilize the atomic data types (from C++11 support) for implementing
     atomic operations.  */

  tree atomic_type = build_qualified_type (instr_type, TYPE_QUAL_ATOMIC);

  gcc_assert (atomic_type != NULL_TREE);

  tree signal_handle = operands[first_input];
  tree atomic_ptype = build_pointer_type (atomic_type);
  tree casted_to_ptr = convert_to_pointer (atomic_ptype, signal_handle);

  tree src0 = NULL_TREE;
  if (atomic_opcode != BRIG_ATOMIC_LD)
    src0 = operands[first_input + 1];

  tree instr_expr = NULL_TREE;

  tree ptype = build_pointer_type (instr_type);
  tree ptr = convert_to_pointer (ptype, operands[first_input]);

  if (atomic_opcode == BRIG_ATOMIC_ST)
    {
      tree mem_ref = build2 (MEM_REF, atomic_type, casted_to_ptr,
			     build_int_cst (atomic_ptype, 0));
      instr_expr = build2 (MODIFY_EXPR, atomic_type, mem_ref, src0);
    }
  else if (atomic_opcode == BRIG_ATOMIC_LD
	   || (atomic_opcode >= BRIG_ATOMIC_WAIT_EQ
	       && atomic_opcode <= BRIG_ATOMIC_WAITTIMEOUT_GTE))
    {
      tree mem_ref = build2 (MEM_REF, atomic_type, casted_to_ptr,
			     build_int_cst (atomic_ptype, 0));
      /* signal_wait* instructions can return spuriously before the
	 condition becomes true.  Therefore it's legal to return
	 right away.  TODO: builtin calls which can be
	 implemented with a power efficient sleep-wait.  */
      instr_expr = mem_ref;
    }
  else if (atomic_opcode == BRIG_ATOMIC_CAS)
    {
      /* Special case for CAS due to the two args.  */
      tree built_in = NULL_TREE;
      switch (gccbrig_hsa_type_bit_size (inst.type))
	{
	case 32:
	  built_in
	    = builtin_decl_explicit (BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_4);
	  break;
	case 64:
	  built_in
	    = builtin_decl_explicit (BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_8);
	  break;
	default:
	  gcc_unreachable ();
	}

      tree src1 = operands[first_input + 2];

      tree src0_type
	= TREE_VALUE (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (built_in))));

      tree src1_type = TREE_VALUE
	(TREE_CHAIN (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (built_in)))));

      instr_expr = call_builtin (built_in, 3, instr_type, ptype, ptr,
				 src0_type, src0, src1_type, src1);
    }
  else
    {
      tree built_in = NULL_TREE;
      /* The rest of the builtins have the same number of parameters.
	 Generate a big if..else that finds the correct builtin
	 automagically from the def file.  */
#undef DEF_HSAIL_SAT_BUILTIN
#undef DEF_HSAIL_BUILTIN
#undef DEF_HSAIL_ATOMIC_BUILTIN
#undef DEF_HSAIL_INTR_BUILTIN
#undef DEF_HSAIL_CVT_ZEROI_SAT_BUILTIN

#define DEF_HSAIL_ATOMIC_BUILTIN(ENUM, ATOMIC_OPCODE, HSAIL_TYPE,	\
				 NAME, TYPE, ATTRS)			\
      if (atomic_opcode == ATOMIC_OPCODE && inst.type == HSAIL_TYPE)	\
	built_in = builtin_decl_explicit (ENUM);			\
      else
#include "brig-builtins.def"
      switch (atomic_opcode)
	{
	case BRIG_ATOMIC_ADD:
	  switch (gccbrig_hsa_type_bit_size (inst.type))
	    {
	    case 32:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_ADD_4);
	      break;
	    case 64:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_ADD_8);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	case BRIG_ATOMIC_SUB:
	  switch (gccbrig_hsa_type_bit_size (inst.type))
	    {
	    case 32:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_SUB_4);
	      break;
	    case 64:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_SUB_8);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	case BRIG_ATOMIC_AND:
	  switch (gccbrig_hsa_type_bit_size (inst.type))
	    {
	    case 32:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_AND_4);
	      break;
	    case 64:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_AND_8);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	case BRIG_ATOMIC_XOR:
	  switch (gccbrig_hsa_type_bit_size (inst.type))
	    {
	    case 32:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_XOR_4);
	      break;
	    case 64:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_XOR_8);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	case BRIG_ATOMIC_OR:
	  switch (gccbrig_hsa_type_bit_size (inst.type))
	    {
	    case 32:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_OR_4);
	      break;
	    case 64:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_FETCH_AND_OR_8);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	case BRIG_ATOMIC_EXCH:
	  switch (gccbrig_hsa_type_bit_size (inst.type))
	    {
	    case 32:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_LOCK_TEST_AND_SET_4);
	      break;
	    case 64:
	      built_in
		= builtin_decl_explicit (BUILT_IN_SYNC_LOCK_TEST_AND_SET_8);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	default:
	  gcc_unreachable ();
	};

      gcc_assert (built_in != NULL_TREE);
      tree arg0_type
	= TREE_VALUE (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (built_in))));

      instr_expr = call_builtin (built_in, 2, instr_type, ptr_type_node,
				 ptr, arg0_type, src0);

      /* We need a temp variable for the result, because otherwise
	 the gimplifier drops a necessary (unsigned to signed) cast in
	 the output assignment and fails a check later.  */
      tree tmp_var = create_tmp_var (arg0_type, "builtin_out");
      tree tmp_assign
	= build2 (MODIFY_EXPR, TREE_TYPE (tmp_var), tmp_var, instr_expr);
      m_parent.m_cf->append_statement (tmp_assign);
      instr_expr = tmp_var;
    }

  if (first_input > 0)
    build_output_assignment (inst, operands[0], instr_expr);
  else
    m_parent.m_cf->append_statement (instr_expr);

  return inst.base.byteCount;
}

size_t
brig_atomic_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstAtomic *inst = (const BrigInstAtomic *) base;
  BrigAtomicOperation8_t atomic_opcode;
  atomic_opcode = inst->atomicOperation;

  return generate_tree (inst->base, atomic_opcode);
}
