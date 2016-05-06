// escape.cc -- Go escape analysis (based on Go compiler algorithm).

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "gogo.h"
#include "escape.h"

// Analyze the program flow for escape information.

void
Gogo::analyze_escape()
{
  // Discover strongly connected groups of functions to analyze for escape
  // information in this package.
  this->discover_analysis_sets();

  for (std::vector<Analysis_set>::iterator p = this->analysis_sets_.begin();
       p != this->analysis_sets_.end();
       ++p)
    {
      std::vector<Named_object*> stack = p->first;
      Escape_context* context = new Escape_context(p->second);

      // Analyze the flow of each function; build the connection graph.
      // This is the assign phase.
      for (std::vector<Named_object*>::reverse_iterator fn = stack.rbegin();
           fn != stack.rend();
           ++fn)
	{
	  context->set_current_function(*fn);
	  this->assign_connectivity(context, *fn);
	}

      // TODO(cmang): Introduce escape node.
      // Propagate levels across each dst.  This is the flood phase.
      // std::vector<Node*> dsts = context->dsts();
      // for (std::vector<Node*>::iterator n = dsts.begin();
      //      n != dsts.end();
      //      ++n)
      //   this->propagate_escape(context, *n);

      // Tag each exported function's parameters with escape information.
      for (std::vector<Named_object*>::iterator fn = stack.begin();
           fn != stack.end();
           ++fn)
        this->tag_function(context, *fn);

      delete context;
    }
}

// Discover strongly connected groups of functions to analyze.

void
Gogo::discover_analysis_sets()
{
  // TODO(cmang): Implement Analysis_set discovery traversal.
  // Escape_analysis_discover(this);
  // this->traverse(&ead);
}

// Build a connectivity graph between nodes in the function being analyzed.

void
Gogo::assign_connectivity(Escape_context*, Named_object*)
{
  // TODO(cmang): Model the flow analysis of input parameters and results for a
  // function.
  // TODO(cmang): Analyze the current function's body.
}

// Propagate escape information across the nodes modeled in this Analysis_set,
// TODO(cmang): Introduce escape analysis node.

void
Gogo::propagate_escape(Escape_context*)
{
  // TODO(cmang): Do a breadth-first traversal of a node's upstream, adjusting
  // the Level appropriately.
}


// Tag each top-level function with escape information that will be used to
// retain analysis results across imports.

void
Gogo::tag_function(Escape_context*, Named_object*)
{
  // TODO(cmang): Create escape information notes for each input and output
  // parameter in a given function.
  // Escape_analysis_tag eat(context, fn);
  // this->traverse(&eat);
}
