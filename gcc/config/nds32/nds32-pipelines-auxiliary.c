/* Auxiliary functions for pipeline descriptions pattern of Andes
   NDS32 cpu for GNU compiler
   Copyright (C) 2012-2015 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* ------------------------------------------------------------------------ */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"	/* Required by recog.h.  */
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"		/* For DFA state_t.  */
#include "insn-codes.h"		/* For CODE_FOR_xxx.  */
#include "reload.h"		/* For push_reload().  */
#include "flags.h"
#include "function.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "emit-rtl.h"
#include "stmt.h"
#include "expr.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "dominance.h"
#include "cfg.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "predict.h"
#include "basic-block.h"
#include "df.h"
#include "tm_p.h"
#include "tm-constrs.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "target.h"
#include "langhooks.h"		/* For add_builtin_function().  */
#include "builtins.h"

/* ------------------------------------------------------------------------ */

/* This file is prepared for future implementation of precise
   pipeline description for nds32 target.  */

/* ------------------------------------------------------------------------ */
