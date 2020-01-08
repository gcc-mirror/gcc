/* brig-inst-mod-handler.cc -- brig rounding moded instruction handling
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

#include "gimple-expr.h"
#include "errors.h"

size_t
brig_inst_mod_handler::generate (const BrigBase *base)
{
  brig_basic_inst_handler basic_handler (m_parent);
  return basic_handler (base);
}

const BrigAluModifier8_t *
brig_inst_mod_handler::modifier (const BrigBase *base) const
{
  const BrigInstMod *inst = (const BrigInstMod *) base;
  return &inst->modifier;
}

const BrigRound8_t *
brig_inst_mod_handler::round (const BrigBase *base) const
{
  const BrigInstMod *inst = (const BrigInstMod *) base;
  return &inst->round;
}

/* This used to inject fesetround () calls to control the rounding mode of the
   actual executed floating point operation.  It turned out that supporting
   conversions using fesetround calls won't work in gcc due to it not being
   able to restrict code motions across calls at the moment.  This
   functionality is therefore disabled for now until a better solution is
   found or if fesetround () is fixed in gcc.  */
size_t
brig_inst_mod_handler::operator () (const BrigBase *base)
{
  return generate (base);
}
