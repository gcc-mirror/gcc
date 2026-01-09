/* channels.h - Publish/Subscribe channels on compiler-specific topics.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_CHANNELS_H
#define GCC_CHANNELS_H

#include "pub-sub.h"

namespace gcc {

/* Forward decls of subscribers for the various topics we have
   publish/subscribe channels for.  */
namespace topics {
  namespace analyzer_events { struct subscriber; }
  namespace pass_events { struct subscriber; }
} // namespace gcc::topics

/* Publish/subscribe channels on various compiler-specific topics.  */

struct compiler_channels
{
  pub_sub::channel<topics::analyzer_events::subscriber> analyzer_events_channel;
  pub_sub::channel<topics::pass_events::subscriber> pass_events_channel;
};

} // namespace gcc

#endif /* ! GCC_CHANNELS_H */
