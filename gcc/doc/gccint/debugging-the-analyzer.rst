..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: analyzer, debugging, static analyzer, debugging

.. _debugging-the-analyzer:

Debugging the Analyzer
**********************

Special Functions for Debugging the Analyzer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The analyzer recognizes various special functions by name, for use
in debugging the analyzer.  Declarations can be seen in the testsuite
in :samp:`analyzer-decls.h`.  None of these functions are actually
implemented.

Add:

.. code-block:: c++

    __analyzer_break ();

to the source being analyzed to trigger a breakpoint in the analyzer when
that source is reached.  By putting a series of these in the source, it's
much easier to effectively step through the program state as it's analyzed.

The analyzer handles:

.. code-block:: c++

  __analyzer_describe (0, expr);

by emitting a warning describing the 2nd argument (which can be of any
type), at a verbosity level given by the 1st argument.  This is for use when
debugging, and may be of use in DejaGnu tests.

.. code-block:: c++

  __analyzer_dump ();

will dump the copious information about the analyzer's state each time it
reaches the call in its traversal of the source.

.. code-block:: c++

  extern void __analyzer_dump_capacity (const void *ptr);

will emit a warning describing the capacity of the base region of
the region pointed to by the 1st argument.

.. code-block:: c++

  extern void __analyzer_dump_escaped (void);

will emit a warning giving the number of decls that have escaped on this
analysis path, followed by a comma-separated list of their names,
in alphabetical order.

.. code-block:: c++

  __analyzer_dump_path ();

will emit a placeholder 'note' diagnostic with a path to that call site,
if the analyzer finds a feasible path to it.

The builtin ``__analyzer_dump_exploded_nodes`` will emit a warning
after analysis containing information on all of the exploded nodes at that
program point:

.. code-block:: c++

    __analyzer_dump_exploded_nodes (0);

will output the number of 'processed' nodes, and the IDs of
both 'processed' and 'merger' nodes, such as:

.. code-block:: c++

  warning: 2 processed enodes: [EN: 56, EN: 58] merger(s): [EN: 54-55, EN: 57, EN: 59]

With a non-zero argument

.. code-block:: c++

    __analyzer_dump_exploded_nodes (1);

it will also dump all of the states within the 'processed' nodes.

.. code-block:: c++

     __analyzer_dump_region_model ();

will dump the region_model's state to stderr.

.. code-block:: c++

  __analyzer_dump_state ("malloc", ptr);

will emit a warning describing the state of the 2nd argument
(which can be of any type) with respect to the state machine with
a name matching the 1st argument (which must be a string literal).
This is for use when debugging, and may be of use in DejaGnu tests.

.. code-block:: c++

  __analyzer_eval (expr);

will emit a warning with text "TRUE", FALSE" or "UNKNOWN" based on the
truthfulness of the argument.  This is useful for writing DejaGnu tests.

.. code-block:: c++

  __analyzer_get_unknown_ptr ();

will obtain an unknown ``void *``.

Other Debugging Techniques
^^^^^^^^^^^^^^^^^^^^^^^^^^

The option :option:`-fdump-analyzer-json` will dump both the supergraph
and the exploded graph in compressed JSON form.

One approach when tracking down where a particular bogus state is
introduced into the ``exploded_graph`` is to add custom code to
``program_state::validate``.

The debug function ``region::is_named_decl_p`` can be used when debugging,
such as for assertions and conditional breakpoints.  For example, when
tracking down a bug in handling a decl called ``yy_buffer_stack``, I
temporarily added a:

.. code-block:: c++

    gcc_assert (!m_base_region->is_named_decl_p ("yy_buffer_stack"));

to ``binding_cluster::mark_as_escaped`` to trap a point where
``yy_buffer_stack`` was mistakenly being treated as having escaped.
