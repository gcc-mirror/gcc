/* brig-copy-move-inst-handler.cc -- brig copy/move instruction handling
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

#include "brig-code-entry-handler.h"
#include "tree-pretty-print.h"
#include "print-tree.h"
#include "errors.h"
#include "brig-util.h"

size_t
brig_copy_move_inst_handler::handle_lda (const BrigInstBase *brig_inst)
{
  tree dest_type = gccbrig_tree_type_for_hsa_type (brig_inst->type);

  tree input = build_tree_operand_from_brig (brig_inst, NULL, 1);
  tree output = build_tree_operand_from_brig (brig_inst, dest_type, 0);

  build_output_assignment (*brig_inst, output, input);
  return brig_inst->base.byteCount;
}

size_t
brig_copy_move_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstBase *brig_inst
    = (const BrigInstBase *) &((const BrigInstBasic *) base)->base;

  if (brig_inst->opcode == BRIG_OPCODE_LDA)
    return handle_lda (brig_inst);

  const BrigInstSourceType *inst_src_type = (const BrigInstSourceType *) base;

  tree source_type = gccbrig_tree_type_for_hsa_type (inst_src_type->sourceType);
  tree dest_type = gccbrig_tree_type_for_hsa_type (brig_inst->type);

  tree input = build_tree_operand_from_brig (brig_inst, source_type, 1);
  tree output = build_tree_operand_from_brig (brig_inst, dest_type, 0);

  if (brig_inst->opcode == BRIG_OPCODE_COMBINE)
    {
      /* For combine, a simple reinterpret cast from the array constructor
	 works.  */
      tree casted = build_resize_convert_view (TREE_TYPE (output), input);
      tree assign = build2 (MODIFY_EXPR, TREE_TYPE (output), output, casted);
      m_parent.m_cf->append_statement (assign);
    }
  else if (brig_inst->opcode == BRIG_OPCODE_EXPAND)
    build_output_assignment (*brig_inst, output, input);
  else
    {
      brig_basic_inst_handler basic (m_parent);
      return basic (base);
    }
  return base->byteCount;
}
