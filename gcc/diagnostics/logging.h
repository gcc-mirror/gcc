/* Debugging code for logging what the diagnostics subsystem is doing.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_LOGGING_H
#define GCC_DIAGNOSTICS_LOGGING_H

#include "diagnostics/output-file.h"
#include "diagnostics/option-id.h"
#include "diagnostics/kinds.h"

namespace diagnostics {

namespace logging {

/* A class for emitting a temporal log of what the diagnostics subsystem
   is doing, for debugging.
   We can't use pretty_printer here as we could potentially be debugging
   pretty-printing itself.  */

class logger
{
public:
  logger (output_file outfile);

  /* High-level functions that emit a line of text.  */
  void log_printf (const char *fmt, ...)
    __attribute__ ((__format__ (printf, 2, 3)));
  void log_bool_return (const char *function_name, bool retval);

  /* Lower-level functions for building up a line of text.  */
  void emit_indent () const;
  void emit_newline () const;

  FILE *get_stream () const
  {
    return m_outfile.get_open_file ();
  }

  int get_indent () const { return m_log_depth * 2; }

  void inc_depth () { m_log_depth++; }
  void dec_depth () { m_log_depth--; }

private:
  output_file m_outfile;
  int m_log_depth;
};

/* RAII class for pushing/popping depth within a logger.  */

class auto_inc_depth
{
public:
  auto_inc_depth (logger *log)
  : m_logger (log)
  {
    if (m_logger)
      m_logger->inc_depth ();
  }
  ~auto_inc_depth ()
  {
    if (m_logger)
      m_logger->dec_depth ();
  }

private:
  logger *m_logger;
};

/* Class for debugging function call parameters.  */

class log_function_params
{
public:
  log_function_params (logger *logger_, const char *name)
  : m_logger (logger_),
    m_first_param (true)
  {
    if (m_logger)
      {
	m_logger->emit_indent ();
	fprintf (m_logger->get_stream (), "%s (", name);
      }
  }
  ~log_function_params ()
  {
    if (m_logger)
      {
	fprintf (m_logger->get_stream (), ")");
	m_logger->emit_newline ();
      }
  }

  log_function_params &
  log_param_string (const char *name, const char *value)
  {
    if (m_logger)
      {
	add_any_comma ();
	fprintf (m_logger->get_stream (), "%s: \"%s\"", name, value);
      }
    return *this;
  }

  log_function_params &
  log_param_location_t (const char *name, location_t value)
  {
    if (m_logger)
      {
	add_any_comma ();
	fprintf (m_logger->get_stream (),
		 "%s: " HOST_SIZE_T_PRINT_HEX,
		 name, (fmt_size_t)value);
      }
    return *this;
  }

  log_function_params &
  log_param_rich_location (const char *name, const rich_location *richloc)
  {
    if (m_logger)
      {
	add_any_comma ();
	fprintf (m_logger->get_stream (),
		 "%s: %p",
		 name, const_cast<void *> ((const void *)richloc));
      }
    return *this;
  }

  log_function_params &
  log_param_option_id (const char *name, diagnostics::option_id value)
  {
    if (m_logger)
      {
	add_any_comma ();
	fprintf (m_logger->get_stream (), "%s: %i", name, value.m_idx);
      }
    return *this;
  }

  log_function_params &
  log_param_kind (const char *name, enum diagnostics::kind value)
  {
    if (m_logger)
      {
	add_any_comma ();
	fprintf (m_logger->get_stream (), "%s: %s",
		 name, get_debug_string_for_kind (value));
      }
    return *this;
  }

  log_function_params &
  log_param_uhwi (const char *name, unsigned HOST_WIDE_INT value)
  {
    if (m_logger)
      {
	add_any_comma ();
	fprintf (m_logger->get_stream (),
		 "%s: " HOST_WIDE_INT_PRINT_DEC,
		 name, value);
      }
    return *this;
  }

  log_function_params &
  log_params_n_gmsgids (unsigned HOST_WIDE_INT n,
			const char *singular_gmsgid,
			const char *plural_gmsgid)
  {
    return log_param_uhwi ("n", n)
      .log_param_string ("singular_gmsgid", singular_gmsgid)
      .log_param_string ("plural_gmsgid", plural_gmsgid);
  }

private:
  void
  add_any_comma ()
  {
    gcc_assert (m_logger);
    if (m_first_param)
      m_first_param = false;
    else
      fprintf (m_logger->get_stream (), ", ");
  }

  logger *m_logger;
  bool m_first_param;
};

} // namespace logging
} // namespace diagnostics

/* Various macros for logging a formatted line, and indenting
   further log messages within a scope.  */

#define DIAGNOSTICS_LOG_SCOPE_PRINTF0(LOGGER, FMT) \
  if (LOGGER)							\
    (LOGGER)->log_printf ((FMT));				\
  diagnostics::logging::auto_inc_depth depth_sentinel (LOGGER);

#define DIAGNOSTICS_LOG_SCOPE_PRINTF1(LOGGER, FMT, ARG0)	\
  if (LOGGER)							\
    (LOGGER)->log_printf ((FMT), (ARG0));			\
  diagnostics::logging::auto_inc_depth depth_sentinel (LOGGER);

#define DIAGNOSTICS_LOG_SCOPE_PRINTF2(LOGGER, FMT, ARG0, ARG1) \
  if (LOGGER)							\
    (LOGGER)->log_printf ((FMT), (ARG0), (ARG1));		\
  diagnostics::logging::auto_inc_depth depth_sentinel (LOGGER);

#endif /* ! GCC_DIAGNOSTICS_LOGGING_H */
