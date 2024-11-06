/* Paths through the code associated with a diagnostic.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_DIAGNOSTIC_PATH_H
#define GCC_DIAGNOSTIC_PATH_H

#include "diagnostic.h" /* for ATTRIBUTE_GCC_DIAG.  */
#include "diagnostic-event-id.h"

class sarif_object;

/* A diagnostic_path is an optional additional piece of metadata associated
   with a diagnostic (via its rich_location).

   It describes a sequence of events predicted by the compiler that
   lead to the problem occurring, with their locations in the user's source,
   and text descriptions.

   For example, the following error has a 3-event path:

     test.c: In function 'demo':
     test.c:29:5: error: passing NULL as argument 1 to 'PyList_Append' which
       requires a non-NULL parameter
        29 |     PyList_Append(list, item);
           |     ^~~~~~~~~~~~~~~~~~~~~~~~~
       'demo': events 1-3
        25 |   list = PyList_New(0);
           |          ^~~~~~~~~~~~~
           |          |
           |          (1) when 'PyList_New' fails, returning NULL
        26 |
        27 |   for (i = 0; i < count; i++) {
           |   ~~~
           |   |
           |   (2) when 'i < count'
        28 |     item = PyLong_FromLong(random());
        29 |     PyList_Append(list, item);
           |     ~~~~~~~~~~~~~~~~~~~~~~~~~
           |     |
           |     (3) when calling 'PyList_Append', passing NULL from (1) as argument 1

    The diagnostic-printing code has consolidated the path into a single
    run of events, since all the events are near each other and within the same
    function; more complicated examples (such as interprocedural paths)
    might be printed as multiple runs of events.  */

/* Abstract base classes, describing events within a path, and the paths
   themselves.  */

/* One event within a diagnostic_path.  */

class diagnostic_event
{
 public:
  /* Enums for giving a sense of what this event means.
     Roughly corresponds to SARIF v2.1.0 section 3.38.8.  */
  enum verb
  {
    VERB_unknown,

    VERB_acquire,
    VERB_release,
    VERB_enter,
    VERB_exit,
    VERB_call,
    VERB_return,
    VERB_branch,

    VERB_danger
  };
  enum noun
  {
    NOUN_unknown,

    NOUN_taint,
    NOUN_sensitive, // this one isn't in SARIF v2.1.0; filed as https://github.com/oasis-tcs/sarif-spec/issues/530
    NOUN_function,
    NOUN_lock,
    NOUN_memory,
    NOUN_resource
  };
  enum property
  {
    PROPERTY_unknown,

    PROPERTY_true,
    PROPERTY_false
  };
  /* A bundle of such enums, allowing for descriptions of the meaning of
     an event, such as
     - "acquire memory": meaning (VERB_acquire, NOUN_memory)
     - "take true branch"": meaning (VERB_branch, PROPERTY_true)
     - "return from function": meaning (VERB_return, NOUN_function)
     etc, as per SARIF's threadFlowLocation "kinds" property
     (SARIF v2.1.0 section 3.38.8).  */
  struct meaning
  {
    meaning ()
    : m_verb (VERB_unknown),
      m_noun (NOUN_unknown),
      m_property (PROPERTY_unknown)
    {
    }
    meaning (enum verb verb, enum noun noun)
    : m_verb (verb), m_noun (noun), m_property (PROPERTY_unknown)
    {
    }
    meaning (enum verb verb, enum property property)
    : m_verb (verb), m_noun (NOUN_unknown), m_property (property)
    {
    }

    void dump_to_pp (pretty_printer *pp) const;

    static const char *maybe_get_verb_str (enum verb);
    static const char *maybe_get_noun_str (enum noun);
    static const char *maybe_get_property_str (enum property);

    enum verb m_verb;
    enum noun m_noun;
    enum property m_property;
  };

  virtual ~diagnostic_event () {}

  virtual location_t get_location () const = 0;

  /* Stack depth, so that consumers can visualize the interprocedural
     calls, returns, and frame nesting.  */
  virtual int get_stack_depth () const = 0;

  /* Print a localized (and possibly colorized) description of this event.  */
  virtual void print_desc (pretty_printer &pp) const = 0;

  /* Get a logical_location for this event, or nullptr if there is none.  */
  virtual const logical_location *get_logical_location () const = 0;

  virtual meaning get_meaning () const = 0;

  /* True iff we should draw a line connecting this event to the
     next event (e.g. to highlight control flow).  */
  virtual bool connect_to_next_event_p () const = 0;

  virtual diagnostic_thread_id_t get_thread_id () const = 0;

  /* Hook for SARIF output to allow for adding diagnostic-specific
     properties to the threadFlowLocation object's property bag.  */
  virtual void
  maybe_add_sarif_properties (sarif_object &/*thread_flow_loc_obj*/) const
  {
  }

  label_text get_desc () const;
};

/* Abstract base class representing a thread of execution within
   a diagnostic_path.
   Each diagnostic_event is associated with one thread.
   Typically there is just one thread per diagnostic_path. */

class diagnostic_thread
{
public:
  virtual ~diagnostic_thread () {}
  virtual label_text get_name (bool can_colorize) const = 0;
};

/* Abstract base class for getting at a sequence of events.  */

class diagnostic_path
{
 public:
  virtual ~diagnostic_path () {}
  virtual unsigned num_events () const = 0;
  virtual const diagnostic_event & get_event (int idx) const = 0;
  virtual unsigned num_threads () const = 0;
  virtual const diagnostic_thread &
  get_thread (diagnostic_thread_id_t) const = 0;

  /* Return true iff the two events are both within the same function,
     or both outside of any function.  */
  virtual bool
  same_function_p (int event_idx_a,
		   int event_idx_b) const = 0;

  bool interprocedural_p () const;
  bool multithreaded_p () const;

private:
  bool get_first_event_in_a_function (unsigned *out_idx) const;
};

/* Concrete subclasses of the above can be found in
   simple-diagnostic-path.h.  */

extern void debug (diagnostic_path *path);

#endif /* ! GCC_DIAGNOSTIC_PATH_H */
