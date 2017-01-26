/* brig-module-handler.cc -- brig module directive handling
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

#include "brig-code-entry-handler.h"
#include "diagnostic-core.h"

size_t
brig_directive_module_handler::operator () (const BrigBase *base)
{
  const BrigDirectiveModule* mod = (const BrigDirectiveModule*)base;
  m_parent.m_module_name = m_parent.get_string (mod->name).substr (1);
  if (mod->hsailMajor != 1 || mod->hsailMinor != 0)
    fatal_error (UNKNOWN_LOCATION, PHSA_ERROR_PREFIX_INCOMPATIBLE_MODULE " "
		 "HSAIL version not supported. HSAIL 1.0 required.");
  if (mod->machineModel != BRIG_MACHINE_LARGE)
    fatal_error (UNKNOWN_LOCATION, PHSA_ERROR_PREFIX_INCOMPATIBLE_MODULE " "
		 "Only HSA 'large' machine model supported.");
  /* Do not check for the profile as the runtime conformance suite tests
     with 'full' profile BRIGs even though they don't use any full profile
     features.  This allows us to run the conformance suite with the
     BRIG FE.  */
  return base->byteCount;
}
