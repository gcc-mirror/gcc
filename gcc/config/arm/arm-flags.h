/* Flags used to identify the presence of processor capabilities.

   Copyright (C) 2016-2021 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#ifndef GCC_ARM_FLAGS_H
#define GCC_ARM_FLAGS_H

/* Flags used to identify a few tuning properties.  These are for legacy
   purposes only.  Do not add any more of these: use the main tuning tables.  */
#define TF_LDSCHED	(1U << 0)
#define TF_WBUF		(1U << 1)
#define TF_CO_PROC	(1U << 2)
#define TF_SMALLMUL	(1U << 3)
#define TF_STRONG	(1U << 4)
#define TF_XSCALE	(1U << 5)
#define TF_NO_MODE32	(1U << 6)

#endif /* GCC_ARM_FLAGS_H */
