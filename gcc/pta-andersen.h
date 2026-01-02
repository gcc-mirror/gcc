/* Andersen-style solver for tree based points-to analysis
   Copyright (C) 2005-2026 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef PTA_ANDERSEN_H
#define PTA_ANDERSEN_H

namespace pointer_analysis {

/* Solve the constraint set.  */
void solve_constraints (void);

} // namespace pointer_analysis

#endif /* PTA_ANDERSEN_H  */
