#include "gcc-plugin.h"
#include <stdlib.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"

/* reqs */
#include "tm.h"

/* gcc/ headers. */
#include "cp/cp-tree.h"
#include "diagnostic.h"
#include "c-family/c-common.h"
#include "c-family/c-pretty-print.h"
#include "tree-iterator.h"
#include "plugin.h"
#include "tree-flow.h"
#include "langhooks.h"
#include "cp/cxx-pretty-print.h"
#include "cp/name-lookup.h"

int plugin_is_GPL_compatible;

int
plugin_init (struct plugin_name_args *plugin_info,
             struct plugin_gcc_version *version)
{
  return 0;
}
