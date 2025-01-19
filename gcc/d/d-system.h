/* d-system.h -- DMD frontend inclusion of gcc header files.
 * Copyright (C) 2018-2025 Free Software Foundation, Inc.
 *
 * GCC is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GCC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GCC; see the file COPYING3.  If not see
 * <http://www.gnu.org/licenses/>.
 */

#ifndef GCC_D_SYSTEM_H
#define GCC_D_SYSTEM_H

#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif
#include "system.h"

/* Forward assert invariants to gcc_assert.  */
#undef assert
#define assert(EXPR) gcc_assert(EXPR)

#endif  /* GCC_D_SYSTEM_H  */
