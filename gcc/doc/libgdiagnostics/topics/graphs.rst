.. Copyright (C) 2025 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <https://www.gnu.org/licenses/>.

.. default-domain:: c

Graphs
======

.. type:: diagnostic_graph

SARIF has support for capturing directed graphs (such as callgraphs
and control flow graphs), both at the level of the run as a whole,
and at the level of individual results.

libgdiagnostics supports this with the following entrypoints, allowing
directed graphs to be

* created (with :func:`diagnostic_manager_new_graph`)

* reported "globally" (with :func:`diagnostic_manager_take_global_graph`)

* reported as part of a :type:`diagnostic` (with :func:`diagnostic_take_graph`), or

* discarded (with :func:`diagnostic_graph_release`).

.. function:: diagnostic_graph * diagnostic_manager_new_graph (diagnostic_manager *manager)

   Create a new directed graph.

   The resulting graph is owned by the caller and must have one of
   :func:`diagnostic_manager_take_global_graph`,
   :func:`diagnostic_take_graph`,
   or :func:`diagnostic_graph_release` called on it to avoid leaks.

   The parameter ``manager`` must be non-null.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: void diagnostic_manager_take_global_graph (diagnostic_manager *manager, \
				      diagnostic_graph *graph)

   Report this graph "globally", taking ownership of it.
   This won't appear in text sinks, but in SARIF sinks the graph will be
   added to theRun.graphs (SARIF v2.1.0 3.14.20).

   Parameters ``manager`` and ``graph`` must both be non-null.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: void diagnostic_take_graph (diagnostic *diag, \
		      diagnostic_graph *graph)

   Add this graph to ``diag``, transferring ownership of it to ``diag``.
   This won't appear in text sinks, but in SARIF sinks the graph will be
   added to theResult.graphs (SARIF v2.1.0 3.27.19).

   Parameters ``diag`` and ``graph`` must both be non-null.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: void diagnostic_graph_release (diagnostic_graph *graph)

   Release ``graph`` which must still be owned by the caller
   i.e. it must *not* have had
   :func:`diagnostic_manager_take_global_graph` or
   :func:`diagnostic_take_graph` called on it.

   Parameters ``graph`` can be null.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.


.. function:: void diagnostic_graph_set_description (diagnostic_graph *graph, \
				 const char *description)

   Set the description of ``graph`` for use in the value of the
   SARIF ``description`` property (SARIF v2.1.0 section 3.39.2).

   The parameter ``graph`` must be non-null.
   The parameter ``description`` can be null, for clearing any existing
   description.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: diagnostic_node * diagnostic_graph_add_node (diagnostic_graph *graph, \
			   const char *node_id, \
			   diagnostic_node *parent_node)

   Create and add a new node within ``graph`` with the given `id``.
   The id must be unique within nodes in ``graph``.

   The parameters ``graph`` and ``id`` must be non-null.

   ``parent_node`` can be NULL (for a top-level node in the graph),
   or non-null for a child node, allowing for arbitrary nesting of
   nodes.

   The new node is owned by ``graph``.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: diagnostic_edge * diagnostic_graph_add_edge (diagnostic_graph *graph, \
			   const char *edge_id, \
			   diagnostic_node *src_node, \
			   diagnostic_node *dst_node, \
			   const char *label)

   Create and add a new edge within ``graph``.

   The parameters ``graph``, ``src_node`` and ``dest_node``
   must be non-null.

   If non-null, then ``edge_id`` must be unique within ``graph``;
   if ``edge_id`` is null then a unique id of the form "edge0", "edge1",
   etc will be used automatically.

   If non-null, then ``label`` will be used for the value of the
   SARIF ``label`` property (SARIF v2.1.0 section 3.41.3).

   The new edge is owned by ``graph``.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: diagnostic_node *diagnostic_graph_get_node_by_id (diagnostic_graph *graph, \
				 const char *node_id)

   Get the node in ``graph`` with the given id, or null.

   The parameters ``graph`` and ``node_id`` must be non-null.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: diagnostic_edge *diagnostic_graph_get_edge_by_id (diagnostic_graph *graph, \
				 const char *edge_id)

   Get the edge in ``graph`` with the given id, or null.

   The parameters ``graph`` and ``edge_id`` must be non-null.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.


.. type:: diagnostic_node

.. function:: void diagnostic_node_set_label (diagnostic_node *node, \
			   const char *label)

   Set the label of ``node`` for use in the value of the
   SARIF ``label`` property (SARIF v2.1.0 section 3.40.3).

   The parameter ``node`` must be non-null.
   The parameter ``label`` can be null, for clearing any existing
   label.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: void diagnostic_node_set_location (diagnostic_node *node, \
			      const diagnostic_physical_location *loc)

   Set the physical location of ``node``, if any.

   The parameter ``node`` must be non-null.
   The parameter ``loc`` can be null, for clearing any existing
   location.

   If set, the value will be used by SARIF sinks within the
   ``location`` property  (SARIF v2.1.0 section 3.40.4).

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.

.. function:: void diagnostic_node_set_logical_location (diagnostic_node *node, \
				      const diagnostic_logical_location *logical_loc)

   Set the logical location of ``node``, if any.

   The parameter ``node`` must be non-null.
   The parameter ``logical_loc`` _can be null, for clearing any existing
   location.

   If set, the value will be used by SARIF sinks within the
   ``location`` property  (SARIF v2.1.0 section 3.40.4).

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_3`.
