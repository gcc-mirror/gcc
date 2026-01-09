/* Command line option handling.  Interactions with diagnostics code.
   Copyright (C) 2010-2026 Free Software Foundation, Inc.

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

#ifndef GCC_OPTS_DIAGNOSTIC_H
#define GCC_OPTS_DIAGNOSTIC_H

#include "diagnostics/sink.h"

/* Abstract subclass of diagnostics::option_id_manager for gcc options.  */

class gcc_diagnostic_option_id_manager : public diagnostics::option_id_manager
{
public:
  char *make_option_url (diagnostics::option_id option_id) const final override;

protected:
  gcc_diagnostic_option_id_manager (unsigned lang_mask)
  : m_lang_mask (lang_mask)
  {}

  unsigned m_lang_mask;
};

/* Concrete implementation of diagnostics::option_id_manager for compiler.  */

class compiler_diagnostic_option_id_manager
  : public gcc_diagnostic_option_id_manager
{
public:
  compiler_diagnostic_option_id_manager (const diagnostics::context &context,
					 unsigned lang_mask,
					 void *opts)
  : gcc_diagnostic_option_id_manager (lang_mask),
    m_context (context),
    m_opts (opts)
  {
  }

  int option_enabled_p (diagnostics::option_id option_id) const final override;
  char *
  make_option_name (diagnostics::option_id option_id,
		    enum diagnostics::kind orig_diag_kind,
		    enum diagnostics::kind diag_kind) const final override;

private:
  const diagnostics::context &m_context;
  void *m_opts;
};

class gcc_extension_factory
{
public:
  virtual ~gcc_extension_factory () {}

  virtual std::unique_ptr<diagnostics::sink::extension>
  make_cfg_extension (diagnostics::sink &sink) const = 0;

  static const gcc_extension_factory *singleton;
};


extern void
handle_OPT_fdiagnostics_add_output_ (const gcc_options &opts,
				     diagnostics::context &dc,
				     const char *arg,
				     location_t loc);

extern void
handle_OPT_fdiagnostics_set_output_ (const gcc_options &opts,
				     diagnostics::context &dc,
				     const char *arg,
				     location_t loc);
#endif
