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

Diagnostic Managers
===================

.. type:: diagnostic_manager;

A :type:`diagnostic_manager` is an opaque bundle of state for a client of
libgdiagnostics.

It has zero of more "output sinks" to which diagnostics are emitted.

Responsibilities include:

* location-management

* caching of source file content

* patch generation

.. function:: diagnostic_manager *diagnostic_manager_new (void)

   Create a new diagnostic_manager.
   The caller will need to call :func:`diagnostic_release_manager`
   on it at some point.

   .. note:: No output sinks are created by default; so you will want
      to create one with something like:

      .. code-block::

	diagnostic_manager_add_text_sink (diag_mgr, stderr,
                                          DIAGNOSTIC_COLORIZE_IF_TTY);

.. function::  void diagnostic_manager_release (diagnostic_manager *diag_mgr)

   Release a diagnostic_manager.

   This will flush output to all of the output sinks, and clean up.

   The parameter must be non-NULL.
