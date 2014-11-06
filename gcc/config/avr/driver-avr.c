/* Subroutines for the gcc driver.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
   Contributed by Anatoly Sokolov <aesok@post.ru>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

/* Current architecture.  */
const avr_arch_t *avr_current_arch = NULL;

/* Current device.  */
const avr_mcu_t *avr_current_device = NULL;
