/* Command line option handling.  Code involving global state that
   should not be shared with the driver.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "opts.h"
#include "flags.h"
#include "ggc.h"
#include "tm.h" /* Required by rtl.h.  */
#include "rtl.h"
#include "output.h"
#include "plugin.h"
#include "tree-pass.h"

void
handle_common_deferred_options (void)
{
  unsigned int i;
  cl_deferred_option *opt;
  VEC(cl_deferred_option,heap) *vec
    = (VEC(cl_deferred_option,heap) *) common_deferred_options;

  FOR_EACH_VEC_ELT (cl_deferred_option, vec, i, opt)
    {
      switch (opt->opt_index)
	{
	case OPT_fcall_used_:
	  fix_register (opt->arg, 0, 1);
	  break;

	case OPT_fcall_saved_:
	  fix_register (opt->arg, 0, 0);
	  break;

	case OPT_fdump_:
	  if (!dump_switch_p (opt->arg))
	    error ("unrecognized command line option %<-fdump-%s%>", opt->arg);
	  break;

	case OPT_ffixed_:
	  /* Deferred.  */
	  fix_register (opt->arg, 1, 1);
	  break;

	case OPT_fplugin_:
#ifdef ENABLE_PLUGIN
	  add_new_plugin (opt->arg);
#else
	  error ("plugin support is disabled; configure with --enable-plugin");
#endif
	  break;

	case OPT_fplugin_arg_:
#ifdef ENABLE_PLUGIN
	  parse_plugin_arg_opt (opt->arg);
#else
	  error ("plugin support is disabled; configure with --enable-plugin");
#endif
	  break;

	case OPT_fstack_limit:
	  /* The real switch is -fno-stack-limit.  */
	  gcc_assert (!opt->value);
	  stack_limit_rtx = NULL_RTX;
	  break;

	case OPT_fstack_limit_register_:
	  {
	    int reg = decode_reg_name (opt->arg);
	    if (reg < 0)
	      error ("unrecognized register name %qs", opt->arg);
	    else
	      stack_limit_rtx = gen_rtx_REG (Pmode, reg);
	  }
	  break;

	case OPT_fstack_limit_symbol_:
	  stack_limit_rtx = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (opt->arg));
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}
