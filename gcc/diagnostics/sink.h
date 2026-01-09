/* Declarations for managing different output formats for diagnostics.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_SINK_H
#define GCC_DIAGNOSTICS_SINK_H

#include "diagnostic.h"
#include "diagnostics/logical-locations.h"

namespace diagnostics {

class per_sink_buffer;

/* Abstract base class for a particular output format for diagnostics;
   each value of -fdiagnostics-output-format= will have its own
   implementation.  */

class sink
{
public:
  /* Abstract base class for adding additional functionality to a sink
     (e.g. via a plugin).  */
  class extension
  {
  public:
    virtual ~extension () {}
    virtual void dump (FILE *out, int indent) const = 0;
    virtual void finalize () {}

    sink &get_sink () const { return m_sink; }

  protected:
    extension (sink &sink_)
    : m_sink (sink_)
    {
    }

  private:
    sink &m_sink;
  };

  virtual ~sink () {}

  virtual text_sink *dyn_cast_text_sink () { return nullptr; }

  virtual void dump_kind (FILE *out) const = 0;
  virtual void dump (FILE *out, int indent) const;

  /* Vfunc for notifying this format what the primary input file is,
     e.g. for titles of HTML, for SARIF's artifact metadata.  */
  virtual void set_main_input_filename (const char *) {}

  /* Vfunc for making an appropriate per_sink_buffer
     subclass for this format.  */
  virtual std::unique_ptr<per_sink_buffer>
  make_per_sink_buffer () = 0;

  /* Vfunc to be called when call a diagnostics::buffer is set on
     a diagnostics::context, to update this format.  The per_sink_buffer
     will be one created by make_per_sink_buffer above and thus be
     of the correct subclass.  */
  virtual void set_buffer (per_sink_buffer *) = 0;

  virtual void on_begin_group () = 0;
  virtual void on_end_group () = 0;

  /* Vfunc with responsibility for phase 3 of formatting the message
     and "printing" the result.  */
  virtual void on_report_diagnostic (const diagnostic_info &,
				     enum kind orig_diag_kind) = 0;

  virtual void on_report_verbatim (text_info &);

  virtual void on_diagram (const diagram &diag) = 0;
  virtual void after_diagnostic (const diagnostic_info &) = 0;
  virtual bool machine_readable_stderr_p () const = 0;
  virtual bool follows_reference_printer_p () const = 0;

  /* Vfunc called when the diagnostics::context changes its
     reference printer (either to a new subclass of pretty_printer
     or when color/url options change).
     Subclasses should update their m_printer accordingly.  */
  virtual void update_printer () = 0;

  virtual void
  report_global_digraph (const lazily_created<digraphs::digraph> &) = 0;

  virtual void
  report_digraph_for_logical_location (const lazily_created<digraphs::digraph> &,
				       logical_locations::key) = 0;

  context &get_context () const { return m_context; }
  pretty_printer *get_printer () const { return m_printer.get (); }

  text_art::theme *get_diagram_theme () const
  {
    return m_context.get_diagram_theme ();
  }

  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  logging::logger *get_logger () { return m_context.get_logger (); }

  void
  add_extension (std::unique_ptr<extension> sink_ext)
  {
    m_extensions.push_back (std::move (sink_ext));
  }

  void
  finalize_extensions ();

protected:
  sink (context &dc)
  : m_context (dc),
    m_printer (dc.clone_printer ())
  {}

protected:
  context &m_context;
  std::unique_ptr<pretty_printer> m_printer;

private:
  std::vector<std::unique_ptr<extension>> m_extensions;
};

extern void
output_format_init (context &,
		    const char *main_input_filename_,
		    const char *base_file_name,
		    enum diagnostics_output_format,
		    bool json_formatting);

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_SINK_H */
