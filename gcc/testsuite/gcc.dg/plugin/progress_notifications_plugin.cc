/* Toy implementation of progress notifications about the compiler,
   using gcc::topics::pass_events.  */

#define INCLUDE_LIST
#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree-pass.h"
#include "diagnostic-core.h"
#include "context.h"
#include "channels.h"
#include "topics/pass-events.h"

int plugin_is_GPL_compatible;

namespace pass_events = gcc::topics::pass_events;

namespace {

class notification_event_subscriber : public pass_events::subscriber
{
public:
  void on_message (const pass_events::before_pass &m) final override
  {
    if (m.fun)
      inform (m.fun->function_start_locus, "starting pass %qs on %qD",
	      m.pass->name, m.fun->decl);
    else
      inform (UNKNOWN_LOCATION, "starting pass %qs", m.pass->name);
  }
  void on_message (const pass_events::after_pass &m) final override
  {
    if (m.fun)
      inform (m.fun->function_end_locus, "finished pass %qs on %qD",
	      m.pass->name, m.fun->decl);
    else
      inform (UNKNOWN_LOCATION, "finished pass %qs", m.pass->name);
  }
} my_event_subscriber;

} // anonymous namespace

int
plugin_init (struct plugin_name_args *,
	     struct plugin_gcc_version *)
{
  g->get_channels ().pass_events_channel.add_subscriber (my_event_subscriber);

  return 0;
}
