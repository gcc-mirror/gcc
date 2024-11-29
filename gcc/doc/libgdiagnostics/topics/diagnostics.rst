.. Copyright (C) 2024 Free Software Foundation, Inc.
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

Diagnostics
===========

.. type:: diagnostic

A :type:`diagnostic` is an opaque bundle of state for a particular
diagnostic that is being constructed in memory.


Lifecycle of a diagnostic
*************************

Diagnostics are

* *created* from a :type:`diagnostic_manager` by using
  :func:`diagnostic_begin`, then

* *populated* with data, such as physical locations, logical locations,
  metadata, execution paths, or fix-it hints, then

* *finished*, in which a formatting string and arguments are given,
  via a call to :func:`diagnostic_finish` or :func:`diagnostic_finish_va`.
  The :type:`diagnostic_manager` will emit the diagnostic to all of the
  manager's output sinks (either immediately, or at some later time,
  depending on the sink).

  Once a :type:`diagnostic` has had one of these "finish" functions called
  on it, it is freed, and is no longer valid for use.

  The formatting strings use their own syntax; see :doc:`message-formatting`.

.. function::  diagnostic *diagnostic_begin (diagnostic_manager *diag_mgr, \
                                             enum diagnostic_level level)

   Create a new :type:`diagnostic` associated with the given
   :type:`diagnostic_manager`.

   The parameter ``diag_mgr`` must be non-NULL.

   The parameter ``level`` describes the severity of the diagnostic.

.. enum:: diagnostic_level

   This enum describes the severity of a particular diagnostic.

   .. macro:: DIAGNOSTIC_LEVEL_ERROR

      A problem sufficiently severe that the program cannot successfully
      complete, or where the input being analyzed is definitely wrong
      (e.g. malformed).

   .. macro:: DIAGNOSTIC_LEVEL_WARNING

      A problem where the input is technically correct, but is likely
      not what the user intended, such as common mistakes, or other
      unusual conditions that *may* indicate trouble, such as use of
      obsolete features.

   .. macro:: DIAGNOSTIC_LEVEL_NOTE

      A supplementary message added to another :type:`diagnostic`, giving
      extra information that may help the user understand it.

   .. macro:: DIAGNOSTIC_LEVEL_SORRY

      A problem where the input is valid, but the tool isn't
      able to handle it.

.. function:: void diagnostic_finish (diagnostic *diag, const char *fmt, ...)

   Emit ``diag`` to all sinks of its manager, and release ``diag``.  It is not
   valid to use ``diag`` after this call.

   Use parameter ``fmt`` for the message.
   Note that this uses gcc's pretty-print format, which is *not* printf.
   See :doc:`message-formatting`.

   Both ``diag`` and ``fmt`` must be non-NULL.

   TODO: who is responsible for putting FMT through gettext?

.. function:: void diagnostic_finish_va (diagnostic *diag, const char *fmt, va_list *args)

   This is equivalent to :func:`diagnostic_finish`, but using a
   :type:`va_list` rather than directly taking variadic arguments.

   All three parameters must be non-NULL.


Diagnostic groups
*****************

See :doc:`the "adding notes" section of the tutorial <../tutorial/04-notes>`
for an example of a diagnostic group.

.. function:: void diagnostic_manager_begin_group (diagnostic_manager *diag_mgr)

  Begin a diagnostic group.  All diagnostics emitted within
  ``diag_mgr`` after the first one will be treated as additional information
  relating to the initial diagnostic.

  The parameter ``diag_mgr`` must be non-NULL.

.. function:: void diagnostic_manager_end_group (diagnostic_manager *diag_mgr)

   Finish a diagnostic group.

   The parameter ``diag_mgr`` must be non-NULL.
