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
  test_signed_2<int> ();
  test_signed_2<HOST_WIDE_INT> ();
  test_signed_2<offset_int> ();
  test_signed_2<widest_int> ();

  test_ordered_2<unsigned short> ();
  test_ordered_2<unsigned int> ();
  test_ordered_2<unsigned HOST_WIDE_INT> ();

  return 0;
}
