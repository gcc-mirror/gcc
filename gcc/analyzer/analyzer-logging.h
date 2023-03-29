/* Hierarchical log messages for the analyzer.
   Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

/* Adapted from jit-logging.h.  */

#ifndef ANALYZER_LOGGING_H
#define ANALYZER_LOGGING_H

#include "diagnostic-core.h"

namespace ana {

/* A logger encapsulates a logging stream: a way to send
   lines of pertinent information to a FILE *.  */

class logger
{
 public:
  logger (FILE *f_out, int flags, int verbosity, const pretty_printer &reference_pp);
  ~logger ();

  void incref (const char *reason);
  void decref (const char *reason);

  void log (const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(2, 3);
  void log_va (const char *fmt, va_list *ap)
    ATTRIBUTE_GCC_DIAG(2, 0);
  void start_log_line ();
  void log_partial (const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(2, 3);
  void log_va_partial (const char *fmt, va_list *ap)
    ATTRIBUTE_GCC_DIAG(2, 0);
  void end_log_line ();

  void enter_scope (const char *scope_name);
  void enter_scope (const char *scope_name, const char *fmt, va_list *ap)
    ATTRIBUTE_GCC_DIAG(3, 0);
  void exit_scope (const char *scope_name);
  void inc_indent () { m_indent_level++; }
  void dec_indent () { m_indent_level--; }

  pretty_printer *get_printer () const { return m_pp; }
  FILE *get_file () const { return m_f_out; }

private:
  DISABLE_COPY_AND_ASSIGN (logger);

  int m_refcount;
  FILE *m_f_out;
  int m_indent_level;
  bool m_log_refcount_changes;
  pretty_printer *m_pp;
};

/* The class log_scope is an RAII-style class intended to make
   it easy to notify a logger about entering and exiting the body of a
   given function.  */

class log_scope
{
public:
  log_scope (logger *logger, const char *name);
  log_scope (logger *logger, const char *name, const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(4, 5);
  ~log_scope ();

 private:
  DISABLE_COPY_AND_ASSIGN (log_scope);

  logger *m_logger;
  const char *m_name;
};

/* The constructor for log_scope.

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

inline
log_scope::log_scope (logger *logger, const char *name, const char *fmt, ...):
 m_logger (logger),
 m_name (name)
{
  if (m_logger)
    {
      m_logger->incref ("log_scope ctor");
      va_list ap;
      va_start (ap, fmt);
      m_logger->enter_scope (m_name, fmt, &ap);
      va_end (ap);
    }
}


/* The destructor for log_scope; essentially the opposite of
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

/* A log_user is something that potentially uses a logger (which could be NULL).

   The log_user class keeps the reference-count of a logger up-to-date.  */

class log_user
{
 public:
  log_user (logger *logger);
  ~log_user ();

  logger * get_logger () const { return m_logger; }
  void set_logger (logger * logger);

  void log (const char *fmt, ...) const
    ATTRIBUTE_GCC_DIAG(2, 3);

  void start_log_line () const;
  void end_log_line () const;

  void enter_scope (const char *scope_name);
  void exit_scope (const char *scope_name);

  pretty_printer *get_logger_pp () const
  {
    gcc_assert (m_logger);
    return m_logger->get_printer ();
  }

  FILE *get_logger_file () const
  {
    if (m_logger == NULL)
      return NULL;
    return m_logger->get_file ();
  }

 private:
  DISABLE_COPY_AND_ASSIGN (log_user);

  logger *m_logger;
};

/* A shortcut for calling log from a log_user, handling the common
   case where the underlying logger is NULL via a no-op.  */

inline void
log_user::log (const char *fmt, ...) const
{
  if (m_logger)
    {
      va_list ap;
      va_start (ap, fmt);
      m_logger->log_va (fmt, &ap);
      va_end (ap);
    }
}

/* A shortcut for starting a log line from a log_user,
   handling the common case where the underlying logger is NULL via
   a no-op.  */

inline void
log_user::start_log_line () const
{
  if (m_logger)
    m_logger->start_log_line ();
}

/* A shortcut for ending a log line from a log_user,
   handling the common case where the underlying logger is NULL via
   a no-op.  */

inline void
log_user::end_log_line () const
{
  if (m_logger)
    m_logger->end_log_line ();
}

/* A shortcut for recording entry into a scope from a log_user,
   handling the common case where the underlying logger is NULL via
   a no-op.  */

inline void
log_user::enter_scope (const char *scope_name)
{
  if (m_logger)
    m_logger->enter_scope (scope_name);
}

/* A shortcut for recording exit from a scope from a log_user,
   handling the common case where the underlying logger is NULL via
   a no-op.  */

inline void
log_user::exit_scope (const char *scope_name)
{
  if (m_logger)
    m_logger->exit_scope (scope_name);
}

/* If the given logger is non-NULL, log entry/exit of this scope to
   it, identifying it using __PRETTY_FUNCTION__.  */

#define LOG_SCOPE(LOGGER)		\
  log_scope s (LOGGER, __PRETTY_FUNCTION__)

/* If the given logger is non-NULL, log entry/exit of this scope to
   it, identifying it using __func__.  */

#define LOG_FUNC(LOGGER) \
  log_scope s (LOGGER, __func__)

#define LOG_FUNC_1(LOGGER, FMT, A0)	\
  log_scope s (LOGGER, __func__, FMT, A0)

#define LOG_FUNC_2(LOGGER, FMT, A0, A1)		\
  log_scope s (LOGGER, __func__, FMT, A0, A1)

#define LOG_FUNC_3(LOGGER, FMT, A0, A1, A2)	\
  log_scope s (LOGGER, __func__, FMT, A0, A1, A2)

#define LOG_FUNC_4(LOGGER, FMT, A0, A1, A2, A3) \
  log_scope s (LOGGER, __func__, FMT, A0, A1, A2, A3)

} // namespace ana

#endif /* ANALYZER_LOGGING_H */
