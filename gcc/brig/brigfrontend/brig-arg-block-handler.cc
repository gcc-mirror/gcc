/* brig-arg-block-handler.cc -- brig arg block start/end directive handling
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
#include "tree-iterator.h"
#include "system.h"
#include "errors.h"

#include "tree-pretty-print.h"
#include "print-tree.h"

size_t
brig_directive_arg_block_handler::operator () (const BrigBase *base)
{
  if (base->kind == BRIG_KIND_DIRECTIVE_ARG_BLOCK_START)
    {
      /* Initiate a new code block for the call site.  */
      tree stmt_list = alloc_stmt_list ();
      tree bind_expr
	= build3 (BIND_EXPR, void_type_node, NULL, stmt_list, NULL);
      tree block = make_node (BLOCK);
      BIND_EXPR_BLOCK (bind_expr) = block;
      static int block_id = 0;
      BLOCK_NUMBER (block) = block_id++;
      TREE_USED (block) = 1;
      tree m_parentblock = DECL_INITIAL (m_parent.m_cf->m_func_decl);
      BLOCK_SUPERCONTEXT (block) = m_parentblock;

      chainon (BLOCK_SUBBLOCKS (m_parentblock), block);

      m_parent.m_cf->m_current_bind_expr = bind_expr;
      m_parent.m_cf->m_generating_arg_block = true;
    }
  else if (base->kind == BRIG_KIND_DIRECTIVE_ARG_BLOCK_END)
    {
      /* Restore the used bind expression back to the function
	 scope.  */
      tree new_bind_expr = m_parent.m_cf->m_current_bind_expr;
      m_parent.m_cf->m_current_bind_expr
	= DECL_SAVED_TREE (m_parent.m_cf->m_func_decl);
      m_parent.m_cf->append_statement (new_bind_expr);
      m_parent.m_cf->m_generating_arg_block = false;
    }
  else
    gcc_unreachable ();

  return base->byteCount;
}
