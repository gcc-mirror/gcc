/* Hierarchical log messages for the analyzer.
   Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "toplev.h" /* for print_version */
#include "pretty-print.h" /* for print_version */
#include "diagnostic.h"
#include "tree-diagnostic.h"

#include "analyzer/analyzer-logging.h"

#if ENABLE_ANALYZER

namespace ana {

/* Implementation of class logger.  */

/* ctor for logger.  */

logger::logger (FILE *f_out,
		int, /* flags */
		int /* verbosity */,
		const pretty_printer &reference_pp) :
  m_refcount (0),
  m_f_out (f_out),
  m_indent_level (0),
  m_log_refcount_changes (false),
  m_pp (reference_pp.clone ())
{
  pp_show_color (m_pp) = 0;
  pp_buffer (m_pp)->stream = f_out;

  /* %qE in logs for SSA_NAMEs should show the ssa names, rather than
     trying to prettify things by showing the underlying var.  */
  pp_format_decoder (m_pp) = default_tree_printer;

  /* Begin the log by writing the GCC version.  */
  print_version (f_out, "", false);
}

/* The destructor for logger, invoked via
   the decref method when the refcount hits zero.
   Note that we do not close the underlying FILE * (m_f_out).  */

logger::~logger ()
{
  /* This should be the last message emitted.  */
  log ("%s", __PRETTY_FUNCTION__);
  gcc_assert (m_refcount == 0);
  delete m_pp;
}

/* Increment the reference count of the logger.  */

void
logger::incref (const char *reason)
{
  m_refcount++;
  if (m_log_refcount_changes)
    log ("%s: reason: %s refcount now %i ",
	 __PRETTY_FUNCTION__, reason, m_refcount);
}

/* Decrement the reference count of the logger,
   deleting it if nothing is referring to it.  */

void
logger::decref (const char *reason)
{
  gcc_assert (m_refcount > 0);
  --m_refcount;
  if (m_log_refcount_changes)
    log ("%s: reason: %s refcount now %i",
	 __PRETTY_FUNCTION__, reason, m_refcount);
  if (m_refcount == 0)
    delete this;
}

/* Write a formatted message to the log, by calling the log_va method.  */

void
logger::log (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  log_va (fmt, &ap);
  va_end (ap);
}

/* Write an indented line to the log file.

   We explicitly flush after each line: if something crashes the process,
   we want the logfile/stream to contain the most up-to-date hint about the
   last thing that was happening, without it being hidden in an in-process
   buffer.  */

void
logger::log_va (const char *fmt, va_list *ap)
{
  start_log_line ();
  log_va_partial (fmt, ap);
  end_log_line ();
}

void
logger::start_log_line ()
{
  for (int i = 0; i < m_indent_level; i++)
    fputc (' ', m_f_out);
}

void
logger::log_partial (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  log_va_partial (fmt, &ap);
  va_end (ap);
}

void
logger::log_va_partial (const char *fmt, va_list *ap)
{
  text_info text;
  text.format_spec = fmt;
  text.args_ptr = ap;
  text.err_no = 0;
  pp_format (m_pp, &text);
  pp_output_formatted_text (m_pp);
}

void
logger::end_log_line ()
{
  pp_flush (m_pp);
  pp_clear_output_area (m_pp);
  fprintf (m_f_out, "\n");
  fflush (m_f_out);
}

/* Record the entry within a particular scope, indenting subsequent
   log lines accordingly.  */

void
logger::enter_scope (const char *scope_name)
{
  log ("entering: %s", scope_name);
  m_indent_level += 1;
}

void
logger::enter_scope (const char *scope_name, const char *fmt, va_list *ap)
{
  start_log_line ();
  log_partial ("entering: %s: ", scope_name);
  log_va_partial (fmt, ap);
  end_log_line ();

  m_indent_level += 1;
}


/* Record the exit from a particular scope, restoring the indent level to
   before the scope was entered.  */

void
logger::exit_scope (const char *scope_name)
{
  if (m_indent_level)
    m_indent_level -= 1;
  else
    log ("(mismatching indentation)");
  log ("exiting: %s", scope_name);
}

/* Implementation of class log_user.  */

/* The constructor for log_user.  */

log_user::log_user (logger *logger) : m_logger (logger)
{
  if (m_logger)
    m_logger->incref("log_user ctor");
}

/* The destructor for log_user.  */

log_user::~log_user ()
{
  if (m_logger)
    m_logger->decref("log_user dtor");
}

/* Set the logger for a log_user, managing the reference counts
   of the old and new logger (either of which might be NULL).  */

void
log_user::set_logger (logger *logger)
{
  if (logger)
    logger->incref ("log_user::set_logger");
  if (m_logger)
    m_logger->decref ("log_user::set_logger");
  m_logger = logger;
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
