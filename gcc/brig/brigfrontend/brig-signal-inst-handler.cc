/* brig-signal-inst-handler.cc -- brig signal instruction handling
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

size_t
brig_signal_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstSignal *inst = (const BrigInstSignal *) base;
  BrigAtomicOperation8_t atomic_opcode;
  atomic_opcode = inst->signalOperation;

  return generate_tree (inst->base, atomic_opcode);
}
