/* brig-label-handler.cc -- brig label directive handling
   Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

size_t
brig_directive_label_handler::operator () (const BrigBase *base)
{
  const BrigDirectiveLabel *brig_label = (const BrigDirectiveLabel *) base;

  const BrigData *label_name = m_parent.get_brig_data_entry (brig_label->name);

  std::string label_str ((const char *) (label_name->bytes),
			 label_name->byteCount);

  m_parent.m_cf->start_new_bb ();

  tree stmt = build_stmt (LABEL_EXPR, m_parent.m_cf->label (label_str));
  m_parent.m_cf->append_statement (stmt);

  return base->byteCount;
}
