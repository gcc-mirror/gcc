/* Internals of libgccjit: logging
   Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

#ifndef JIT_LOGGING_H
#define JIT_LOGGING_H

#include "jit-common.h"

namespace gcc {

namespace jit {

/* A gcc::jit::logger encapsulates a logging stream: a way to send
   lines of pertinent information to a FILE *.  */

class logger
{
 public:
  logger (FILE *f_out, int flags, int verbosity);
  ~logger ();

  void incref (const char *reason);
  void decref (const char *reason);

  void log (const char *fmt, ...)
    GNU_PRINTF(2, 3);
  void log_va (const char *fmt, va_list ap)
    GNU_PRINTF(2, 0);

  void enter_scope (const char *scope_name);
  void exit_scope (const char *scope_name);

private:
  int m_refcount;
  FILE *m_f_out;
  int m_indent_level;
  bool m_log_refcount_changes;
};

/* The class gcc::jit::log_scope is an RAII-style class intended to make
   it easy to notify a logger about entering and exiting the body of a
   given function.  */

class log_scope
{
public:
  log_scope (logger *logger, const char *name);
  ~log_scope ();

 private:
  logger *m_logger;
  const char *m_name;
};

/* The constructor for gcc::jit::log_scope.

   The normal case is that the logger is NULL, in which case this should
   be largely a no-op.

   If we do have a logger, notify it that we're entering the given scope.
   We also need to hold a reference on it, to avoid a use-after-free
   when logging the cleanup of the owner of the logger.  */

inline
log_scope::log_scope (logger *logger, const char *name) :
 m_logger (logger),
 m_name (name)
{
  if (m_logger)
    {
      m_logger->incref ("log_scope ctor");
      m_logger->enter_scope (m_name);
    }
}

/* The destructor for gcc::jit::log_scope; essentially the opposite of
   the constructor.  */

inline
log_scope::~log_scope ()
{
  if (m_logger)
    {
      m_logger->exit_scope (m_name);
      m_logger->decref ("log_scope dtor");
    }
}

/* A gcc::jit::log_user is something that potentially uses a
   gcc::jit::logger (which could be NULL).

   It is the base class for each of:

      - class gcc::jit::recording::context

      - class gcc::jit::playback::context

      - class gcc::jit::tempdir

      - class gcc::jit::result

   The log_user class keeps the reference-count of a logger up-to-date.  */

class log_user
{
 public:
  log_user (logger *logger);
  ~log_user ();

  logger * get_logger () const { return m_logger; }
  void set_logger (logger * logger);

  void log (const char *fmt, ...) const
    GNU_PRINTF(2, 3);

  void enter_scope (const char *scope_name);
  void exit_scope (const char *scope_name);

 private:
  logger *m_logger;
};

/* A shortcut for calling log from a context/result, handling the common
   case where the underlying logger is NULL via a no-op.  */

inline void
log_user::log (const char *fmt, ...) const
{
  if (m_logger)
    {
      va_list ap;
      va_start (ap, fmt);
      m_logger->log_va (fmt, ap);
      va_end (ap);
    }
}

/* A shortcut for recording entry into a scope from a context/result,
   handling the common case where the underlying logger is NULL via
   a no-op.  */

inline void
log_user::enter_scope (const char *scope_name)
{
  if (m_logger)
    m_logger->enter_scope (scope_name);
}

/* A shortcut for recording exit from a scope from a context/result,
   handling the common case where the underlying logger is NULL via
   a no-op.  */

inline void
log_user::exit_scope (const char *scope_name)
{
  if (m_logger)
    m_logger->exit_scope (scope_name);
}

} // namespace gcc::jit

} // namespace gcc

/* If the given logger is non-NULL, log entry/exit of this scope to
   it, identifying it using __PRETTY_FUNCTION__.  */

#define JIT_LOG_SCOPE(LOGGER) \
  gcc::jit::log_scope s (LOGGER, __PRETTY_FUNCTION__)

/* If the given logger is non-NULL, log entry/exit of this scope to
   it, identifying it using __func__.  */

#define JIT_LOG_FUNC(LOGGER) \
  gcc::jit::log_scope s (LOGGER, __func__)

#endif /* JIT_LOGGING_H */
