/* Analyzer-specific extensions to diagnostic-handling.
   Copyright (C) 2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "diagnostics/digraphs.h"
#include "diagnostics/dumping.h"
#include "context.h"
#include "channels.h"
#include "tree-diagnostic-sink-extensions.h"

#include "analyzer/exploded-graph.h"

namespace { namespace analyzer_events = gcc::topics::analyzer_events; }

#if ENABLE_ANALYZER

namespace ana {

class lazy_supergraph_diagnostic_digraph
  : public lazily_created<diagnostics::digraphs::digraph>
{
public:
  lazy_supergraph_diagnostic_digraph (const supergraph &sg)
  : m_sg (sg)
  {
  }

  std::unique_ptr<diagnostics::digraphs::digraph>
  create_object () const final override
  {
    auto g = m_sg.make_diagnostics_digraph ();
    g->set_description ("\"supergraph\" used by '-fanalyzer'"
			" internally to represent the user's code");
    return g;
  }

private:
  const supergraph &m_sg;
};

class lazy_exploded_graph_diagnostic_digraph
  : public lazily_created<diagnostics::digraphs::digraph>
{
public:
  lazy_exploded_graph_diagnostic_digraph (const exploded_graph &eg)
  : m_eg (eg)
  {
  }

  std::unique_ptr<diagnostics::digraphs::digraph>
  create_object () const final override
  {
    auto g = m_eg.make_diagnostics_digraph ();
    g->set_description ("\"exploded graph\" used by '-fanalyzer'"
			" internally to represent interprocedural control"
			" flow in the user's code along with possible states"
			" at each program point");
    return g;
  }

private:
  const exploded_graph &m_eg;
};

static void
report_analyzer_info (diagnostics::sink &sink,
		      const analyzer_events::on_ana_done &m)
{
  sink.report_global_digraph
    (lazy_supergraph_diagnostic_digraph (m.m_eg.get_supergraph ()));

  sink.report_global_digraph
    (lazy_exploded_graph_diagnostic_digraph (m.m_eg));

  // TODO: capture other information???
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */

/* A diagnostics::sink::extension which subscribes to the analyzer
   being done and responds to it by adding internal details about the
   analysis to the diagnostic sink.  */

class analyzer_sink_extension : public diagnostics::sink::extension
{
public:
  analyzer_sink_extension (diagnostics::sink &sink)
  : extension (sink),
    m_event_subscriber (sink)
  {
    auto &channel = g->get_channels ().analyzer_events_channel;
    channel.add_subscriber (m_event_subscriber);
  }

  void
  dump (FILE *out, int indent) const
  {
    diagnostics::dumping::emit_heading (out, indent, "analyzer_sink_extension");
  }

private:
  class event_subscriber : public analyzer_events::subscriber
  {
  public:
    event_subscriber (diagnostics::sink &sink) : m_sink (sink) {}

    void
    on_message (const analyzer_events::on_ana_done &m) final override
    {
#if ENABLE_ANALYZER
      report_analyzer_info (m_sink, m);
#endif
    }

  private:
    diagnostics::sink &m_sink;
  } m_event_subscriber;
};

std::unique_ptr<diagnostics::sink::extension>
compiler_extension_factory::make_analyzer_graphs_extension (diagnostics::sink &sink) const
{
  return std::make_unique<analyzer_sink_extension> (sink);
}
