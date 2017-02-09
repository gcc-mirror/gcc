/* Subroutines for the gcc driver.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by Claudiu Zissulescu <claziss@synopsys.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

/* Returns command line parameters to pass to as.  */

const char*
arc_cpu_to_as (int argc, const char **argv)
{
  const char *name = NULL;
  const arc_cpu_t *arc_selected_cpu;

  /* No argument, check what is the default cpu.  */
  if (argc == 0)
    {
      arc_selected_cpu = &arc_cpu_types[(int) TARGET_CPU_DEFAULT];
    }
  else
    {
      name = argv[0];
      for (arc_selected_cpu = arc_cpu_types; arc_selected_cpu->name;
	   arc_selected_cpu++)
	{
	  if (strcmp (arc_selected_cpu->name, name) == 0)
	    break;
	}
    }

  switch (arc_selected_cpu->arch_info->arch_id)
    {
    case BASE_ARCH_em:
      if (arc_selected_cpu->flags & FL_CD)
	name = "-mcode-density";
      else
	name = "";
      if (arc_selected_cpu->flags & FL_FPUDA)
	name = concat ("-mfpuda ", name, NULL);
      if (arc_selected_cpu->flags & FL_SPFP)
	name = concat ("-mspfp ", name, NULL);
      if (arc_selected_cpu->flags & FL_DPFP)
	name = concat ("-mdpfp ", name, NULL);
      return concat ("-mcpu=arcem ", name, NULL);
    case BASE_ARCH_hs:
      return "-mcpu=archs";
    case BASE_ARCH_700:
      if (arc_selected_cpu->processor == PROCESSOR_nps400)
	return "-mcpu=nps400 -mEA";
      else
	return "-mcpu=arc700 -mEA";
    case BASE_ARCH_6xx:
      if (arc_selected_cpu->flags & FL_MUL64)
	return "-mcpu=arc600 -mmul64 -mnorm";
      if (arc_selected_cpu->flags & FL_MUL32x16)
	return "-mcpu=arc600 -mdsp-packa -mnorm";
      return "-mcpu=arc600 -mnorm";
    default:
      gcc_unreachable ();
    }
  return NULL;
}
