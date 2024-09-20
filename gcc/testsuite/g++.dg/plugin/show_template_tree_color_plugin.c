/* We want to verify the colorized output of cxx_format_postprocessor,
   but turning on colorization for everything confuses "dg-error" etc.
   The color codes in the generated messages would also need escaping
   for use within dg-error.

   Hence the simplest approach is to provide a custom
   diagnostic_text_starter_fn, which does nothing.

   The resulting messages lack the "FILENAME:LINE:COL: error: " prefix
   and can thus be tested using dg-begin/end-multiline-output.  */

/* { dg-options "-O" } */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "plugin-version.h"
#include "diagnostic.h"

int plugin_is_GPL_compatible;

void
noop_text_starter_fn (diagnostic_text_output_format &, const diagnostic_info *)
{
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  diagnostic_text_starter (global_dc) = noop_text_starter_fn;

  return 0;
}
