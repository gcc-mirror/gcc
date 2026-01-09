/* pass-events.h - pub/sub messages about GCC optimization passes.
   Copyright (C) 2025 Free Software Foundation, Inc.

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

#ifndef GCC_TOPICS_PASS_EVENTS_H
#define GCC_TOPICS_PASS_EVENTS_H

#include "pub-sub.h"

namespace gcc {
namespace topics {

/* A topic for messages relating to GCC optimization passes.  */

namespace pass_events {

struct before_pass
{
  opt_pass *pass;
  function *fun;
};

struct after_pass
{
  opt_pass *pass;
  function *fun;
};

/* Abstract base class for a subscriber to messages about
   GCC optimization passes.  */

struct subscriber
{
  virtual ~subscriber () = default;

  virtual void on_message (const before_pass &) = 0;
  virtual void on_message (const after_pass &) = 0;
};

} // namespace gcc::topics::pass_events
} // namespace gcc::topics
} // namespace gcc

#endif /* ! GCC_TOPICS_PASS_EVENTS_H */
