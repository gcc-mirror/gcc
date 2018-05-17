/* This plugin tests the GGC related plugin events.  */
/* { dg-options "-O" } */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gcc-plugin.h"
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "basic-block.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"

int plugin_is_GPL_compatible;

/* our callback is the same for all PLUGIN_GGC_START,
   PLUGIN_GGC_MARKING, PLUGIN_GGC_END events; it just increments the
   user_data which is an int */
static void increment_callback (void *gcc_data, void *user_data);

/* our counters are user_data */
static int our_ggc_start_counter;
static int our_ggc_end_counter;
static int our_ggc_marking_counter;

/* our empty GGC extra root table */
static const struct ggc_root_tab our_xtratab[] = {
  LAST_GGC_ROOT_TAB
};


/* The initialization routine exposed to and called by GCC. The spec of this
   function is defined in gcc/gcc-plugin.h.

   Note that this function needs to be named exactly "plugin_init".  */
int
plugin_init (struct plugin_name_args *plugin_info,
	      struct plugin_gcc_version *version)
{
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  int i = 0;
  struct plugin_argument *argv = plugin_info->argv;
  if (!plugin_default_version_check (version, &gcc_version))
    return 1;
  /* Process the plugin arguments. This plugin takes the following arguments:
     count-ggc-start count-ggc-end count-ggc-mark */
  for (i = 0; i < argc; ++i)
    {
      if (!strcmp (argv[i].key, "count-ggc-start"))
	{
	  if (argv[i].value)
	    warning (0, G_("option '-fplugin-arg-%s-count-ggc-start=%s'"
			   " ignored (superfluous '=%s')"),
		     plugin_name, argv[i].value, argv[i].value);
	  else
	    register_callback ("ggcplug",
			       PLUGIN_GGC_START,
			       increment_callback,
			       (void *) &our_ggc_start_counter);
	}
      else if (!strcmp (argv[i].key, "count-ggc-end"))
	{
	  if (argv[i].value)
	    warning (0, G_("option '-fplugin-arg-%s-count-ggc-end=%s'"
			   " ignored (superfluous '=%s')"),
		     plugin_name, argv[i].value, argv[i].value);
	  else
	    register_callback ("ggcplug",
			       PLUGIN_GGC_END,
			       increment_callback,
			       (void *) &our_ggc_end_counter);
	}
      else if (!strcmp (argv[i].key, "count-ggc-mark"))
	{
	  if (argv[i].value)
	    warning (0, G_("option '-fplugin-arg-%s-count-ggc-mark=%s'"
			   " ignored (superfluous '=%s')"),
		     plugin_name, argv[i].value, argv[i].value);
	  else
	    register_callback ("ggcplug",
			       PLUGIN_GGC_MARKING,
			       increment_callback,
			       (void *) &our_ggc_marking_counter);
	}
      else if (!strcmp (argv[i].key, "test-extra-root"))
	{
	  if (argv[i].value)
	    warning (0, G_("option '-fplugin-arg-%s-test-extra-root=%s'"
			   " ignored (superfluous '=%s')"),
		     plugin_name, argv[i].value, argv[i].value);
	  else
	    register_callback ("ggcplug",
			       PLUGIN_REGISTER_GGC_ROOTS,
			       NULL,
			       (void *) our_xtratab);
	}
    }
  /* plugin initialization succeeded */
  return 0;
 }

static void
increment_callback (void *gcc_data, void *user_data)
{
  int *usercountptr = (int *) user_data;
  gcc_assert (!gcc_data);
  gcc_assert (user_data);
  (*usercountptr)++;
}
