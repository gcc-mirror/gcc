.. Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

ABI and API compatibility
=========================

The libgdiagnostics developers strive for ABI and API backward-compatibility:
programs built against libgdiagnostics.so stand a good chance of running
without recompilation against newer versions of libgdiagnostics.so, and
ought to recompile without modification against newer versions of
libgdiagnostics.h.

.. note:: The libgdiagnostics++.h C++ API is more experimental, and less
          locked-down at this time.

API compatibility is achieved by extending the API rather than changing
it.  For ABI compatiblity, we avoid bumping the SONAME, and instead use
symbol versioning to tag each symbol, so that a binary linked against
libgdiagnostics.so is tagged according to the symbols that it uses.

For example, :func:`diagnostic_logical_location_get_kind` was added in
``LIBGDIAGNOSTICS_ABI_1``.  If a client program uses it, this can be detected
from metadata by using ``objdump``:

.. code-block:: bash

   $ objdump -p testsuite/libgdiagnostics/test-logical-location.c.exe | tail -n 7

   Version References:
     required from libc.so.6:
       0x09691a75 0x00 04 GLIBC_2.2.5
     required from libgdiagnostics.so.0:
       0x0ec567d1 0x00 03 LIBGDIAGNOSTICS_ABI_1
       0x0ec567d0 0x00 02 LIBGDIAGNOSTICS_ABI_0

You can see the symbol tags provided by libgdiagnostics.so using ``objdump``:

.. code-block:: bash

   $ objdump -p libgdiagnostics.so | less
   [...snip...]
   Version definitions:
   1 0x01 0x099ea4b0 libgdiagnostics.so.0
   2 0x00 0x0ec567d0 LIBGDIAGNOSTICS_ABI_0
   3 0x00 0x0ec567d1 LIBGDIAGNOSTICS_ABI_1
           LIBGDIAGNOSTICS_ABI_0
   [...snip...]

ABI symbol tags
***************

.. _LIBGDIAGNOSTICS_ABI_0:

``LIBGDIAGNOSTICS_ABI_0``
-------------------------

All entrypoints in the initial release of libgdiagnostics (in GCC 15) are
tagged with ``LIBGDIAGNOSTICS_ABI_0``; these entrypoints are:

  * :func:`diagnostic_manager_new`

  * :func:`diagnostic_manager_release`

  * :func:`diagnostic_manager_set_tool_name`

  * :func:`diagnostic_manager_set_full_name`

  * :func:`diagnostic_manager_set_version_string`

  * :func:`diagnostic_manager_set_version_url`

  * :func:`diagnostic_manager_add_text_sink`

  * :func:`diagnostic_text_sink_set_source_printing_enabled`

  * :func:`diagnostic_text_sink_set_colorize`

  * :func:`diagnostic_text_sink_set_labelled_source_colorization_enabled`

  * :func:`diagnostic_manager_add_sarif_sink`

  * :func:`diagnostic_manager_write_patch`

  * :func:`diagnostic_manager_new_file`

  * :func:`diagnostic_file_set_buffered_content`

  * :func:`diagnostic_manager_debug_dump_file`

  * :func:`diagnostic_manager_new_location_from_file_and_line`

  * :func:`diagnostic_manager_new_location_from_file_line_column`

  * :func:`diagnostic_manager_new_location_from_range`

  * :func:`diagnostic_manager_debug_dump_location`

  * :func:`diagnostic_manager_new_logical_location`

  * :func:`diagnostic_manager_debug_dump_logical_location`

  * :func:`diagnostic_manager_begin_group`

  * :func:`diagnostic_manager_end_group`

  * :func:`diagnostic_begin`

  * :func:`diagnostic_set_cwe`

  * :func:`diagnostic_add_rule`

  * :func:`diagnostic_set_location`

  * :func:`diagnostic_set_location_with_label`

  * :func:`diagnostic_add_location`

  * :func:`diagnostic_add_location_with_label`

  * :func:`diagnostic_set_logical_location`

  * :func:`diagnostic_add_fix_it_hint_insert_before`

  * :func:`diagnostic_add_fix_it_hint_insert_after`

  * :func:`diagnostic_add_fix_it_hint_replace`

  * :func:`diagnostic_add_fix_it_hint_delete`

  * :func:`diagnostic_add_execution_path`

  * :func:`diagnostic_manager_new_execution_path`

  * :func:`diagnostic_take_execution_path`

  * :func:`diagnostic_execution_path_release`

  * :func:`diagnostic_execution_path_add_event`

  * :func:`diagnostic_execution_path_add_event_va`

  * :func:`diagnostic_finish`

  * :func:`diagnostic_finish_va`

  * :func:`diagnostic_physical_location_get_file`

.. _LIBGDIAGNOSTICS_ABI_1:

``LIBGDIAGNOSTICS_ABI_1``
-------------------------
``LIBGDIAGNOSTICS_ABI_1`` covers the addition of these functions for
acccessing values within a :type:`diagnostic_logical_location`:

  * :func:`diagnostic_logical_location_get_kind`

  * :func:`diagnostic_logical_location_get_parent`

  * :func:`diagnostic_logical_location_get_short_name`

  * :func:`diagnostic_logical_location_get_fully_qualified_name`

  * :func:`diagnostic_logical_location_get_decorated_name`
