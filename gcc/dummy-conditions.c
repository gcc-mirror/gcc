/* Support for calculating constant conditions.
   Copyright (C) 2002 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "gensupport.h"

/* MD generators that are run before insn-conditions.c exists should
   link against this file instead.  Currently that is genconditions
   and genconstants.  */

/* Empty conditions table to prevent link errors.  */
const struct c_test insn_conditions[1] = { { 0, 0 } };
const size_t n_insn_conditions = 0;

/* Disable insn elision, since it is currently impossible.  */
const int insn_elision_unavailable = 1;
