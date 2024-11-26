/* Not worth spending time optimizing this.  */
/* { dg-options "-O0" } */

#include "config.h"
#include "gcc-plugin.h"
#include "system.h"
#include "coretypes.h"
#include "poly-int-tests.h"

int plugin_is_GPL_compatible;

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  test_num_coeffs_extra<1> ();
  return 0;
}
