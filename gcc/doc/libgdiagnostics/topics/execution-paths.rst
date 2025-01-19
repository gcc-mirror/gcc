.. Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

Execution paths
===============

.. type:: diagnostic_execution_path

A :type:`diagnostic` can optionally contain a :type:`diagnostic_execution_path`
describing a path of execution through code.

.. function:: diagnostic_execution_path * diagnostic_add_execution_path (diagnostic *diag)

   Create and borrow a pointer to an execution path for ``diag``.

   The path is automatically cleaned up when ``diag`` is finished.

   ``diag`` must be non-NULL.

.. function:: diagnostic_execution_path * diagnostic_manager_new_execution_path (diagnostic_manager *diag_mgr)

   Create a new execution path. This is owned by the caller and must have either
   :func:`diagnostic_take_execution_path` or
   :func:`diagnostic_execution_path_release` called on it.

   ``diag_mgr`` must be non-NULL.

.. function:: void diagnostic_take_execution_path (diagnostic *diag, diagnostic_execution_path *path)

   Set ``diag`` to use ``path`` as its execution path, taking ownership of ``path``.

   Both parameters must be non-NULL.

.. function:: void diagnostic_execution_path_release (diagnostic_execution_path *path)

   Release ownership of ``path``, which must not have been taken by a diagnostic.

.. type:: diagnostic_event_id

A :type:`diagnostic_event_id` identifies a particular event within a
:type:`diagnostic_execution_path` and can be used for expressing
cross-references between events.  In particular FIXME

.. function:: diagnostic_event_id diagnostic_execution_path_add_event (diagnostic_execution_path *path, \
                                                                       const diagnostic_physical_location *physical_loc, \
                                                                       const diagnostic_logical_location *logical_loc, \
                                                                       unsigned stack_depth, \
                                                                       const char *fmt, ...)

   Append an event to the end of ``path``, which must be non-NULL.

   ``physical_loc`` can be NULL, or non-NULL to associate the event
   with a :type:`diagnostic_physical_location`.

   ``logical_loc`` can be NULL, or non-NULL to associate the event
   with a :type:`diagnostic_logical_location`.

   ``stack_depth`` is for use in interprocedural paths and identifies the
   depth of the stack at the event.  Purely intraprocedural paths should
   use a stack depth of 1 for their events

   ``fmt`` must be non-NULL.  See :doc:`message-formatting` for details of
   how to use it.

.. function:: diagnostic_event_id diagnostic_execution_path_add_event_va (diagnostic_execution_path *path, \
                                                                          const diagnostic_physical_location *physical_loc, \
                                                                          const diagnostic_logical_location *logical_loc, \
                                                                          unsigned stack_depth, \
                                                                          const char *fmt, \
                                                                          va_list *args)

   Equivalent to :func:`diagnostic_execution_path_add_event`, but using a
   :type:`va_list` rather than directly taking variadic arguments.

Paths are printed to text sinks, and for SARIF sinks each path is added as
a ``codeFlow`` object (see SARIF 2.1.0
`§3.36 codeFlow object <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790990>`_).
