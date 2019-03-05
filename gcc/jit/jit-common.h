/* Core of implementation of libgccjit.so
   Copyright (C) 2013-2019 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "libgccjit.h"

#include "vec.h"
#include "tree.h"
#include "inchash.h"
#include "tree-iterator.h"

#ifdef GCC_VERSION
#if GCC_VERSION >= 4001
#define GNU_PRINTF(M, N) __attribute__ ((format (gnu_printf, (M), (N))))
#else
#define GNU_PRINTF(M, N)
#endif
#endif

const int NUM_GCC_JIT_TYPES = GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE + 1;

/* This comment is included by the docs.

   In order to allow jit objects to be usable outside of a compile
   whilst working with the existing structure of GCC's code the
   C API is implemented in terms of a gcc::jit::recording::context,
   which records the calls made to it.

   When a gcc_jit_context is compiled, the recording context creates a
   playback context.  The playback context invokes the bulk of the GCC
   code, and within the "frontend" parsing hook, plays back the recorded
   API calls, creating GCC tree objects.

   So there are two parallel families of classes: those relating to
   recording, and those relating to playback:

   * Visibility: recording objects are exposed back to client code,
     whereas playback objects are internal to the library.

   * Lifetime: recording objects have a lifetime equal to that of the
     recording context that created them, whereas playback objects only
     exist within the frontend hook.

   * Memory allocation: recording objects are allocated by the recording
     context, and automatically freed by it when the context is released,
     whereas playback objects are allocated within the GC heap, and
     garbage-collected; they can own GC-references.

   * Integration with rest of GCC: recording objects are unrelated to the
     rest of GCC, whereas playback objects are wrappers around "tree"
     instances.  Hence you can't ask a recording rvalue or lvalue what its
     type is, whereas you can for a playback rvalue of lvalue (since it
     can work with the underlying GCC tree nodes).

   * Instancing: There can be multiple recording contexts "alive" at once
     (albeit it only one compiling at once), whereas there can only be one
     playback context alive at one time (since it interacts with the GC).

   Ultimately if GCC could support multiple GC heaps and contexts, and
   finer-grained initialization, then this recording vs playback
   distinction could be eliminated.

   During a playback, we associate objects from the recording with
   their counterparts during this playback.  For simplicity, we store this
   within the recording objects, as ``void *m_playback_obj``, casting it to
   the appropriate playback object subclass.  For these casts to make
   sense, the two class hierarchies need to have the same structure.

   Note that the playback objects that ``m_playback_obj`` points to are
   GC-allocated, but the recording objects don't own references:
   these associations only exist within a part of the code where
   the GC doesn't collect, and are set back to NULL before the GC can
   run.

   End of comment for inclusion in the docs.  */

namespace gcc {

namespace jit {

class result;
class dump;
class logger;
class builtins_manager; // declared within jit-builtins.h
class tempdir;

namespace recording {

  /* Recording types.  */

  /* Indentation indicates inheritance: */
  class context;
  class memento;
    class string;
    class location;
    class type;
      class function_type;
      class compound_type;
        class struct_;
	class union_;
      class vector_type;
    class field;
    class fields;
    class function;
    class block;
    class rvalue;
      class lvalue;
        class local;
	class global;
        class param;
      class base_call;
      class function_pointer;
    class statement;
    class case_;

  /* End of recording types. */
}

namespace playback {
  /* Playback types.  */

  /* Indentation indicates inheritance: */
  class context;
  class wrapper;
    class type;
      class compound_type;
    class field;
    class function;
    class block;
    class rvalue;
      class lvalue;
        class param;
    class source_file;
    class source_line;
    class location;
    class case_;

  /* End of playback types. */
}

typedef playback::context replayer;

class dump
{
public:
  dump (recording::context &ctxt,
	const char *filename,
	bool update_locations);
  ~dump ();

  recording::context &get_context () { return m_ctxt; }

  void write (const char *fmt, ...)
    GNU_PRINTF(2, 3);

  bool update_locations () const { return m_update_locations; }

  recording::location *
  make_location () const;

  FILE *get_file () const { return m_file; }

private:
  recording::context &m_ctxt;
  const char *m_filename;
  bool m_update_locations;
  int m_line;
  int m_column;
  FILE *m_file;
};

/* A hidden enum of boolean options that are only exposed via API
   entrypoints, rather than via gcc_jit_context_set_bool_option.  */

enum inner_bool_option
{
  INNER_BOOL_OPTION_ALLOW_UNREACHABLE_BLOCKS,
  INNER_BOOL_OPTION_USE_EXTERNAL_DRIVER,

  NUM_INNER_BOOL_OPTIONS
};

} // namespace gcc::jit

} // namespace gcc

#endif /* JIT_COMMON_H */
