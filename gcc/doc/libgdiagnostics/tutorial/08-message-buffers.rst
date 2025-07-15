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

Tutorial part 8: message buffers
================================

In previous examples, we finished a diagnostic with a call to
:func:`diagnostic_finish`, which takes a format string and arguments
to determine the text message of the diagnostic.

Sometimes this approach is inconvenient, such as where you might want to
build up a message programatically from a series of components.
Additionally, you might have existing code that uses ``fprintf``, whereas
:func:`diagnostic_finish` has its
:doc:`own formatting conventions <../topics/message-formatting>` which are
:strong:`not` the same as printf.

For this reason libgdiagnostics (from ``LIBGDIAGNOSTICS_ABI_3`` onwards)
supports :type:`diagnostic_message_buffer`, which can be used to accumulate a
message before using it.

You create a :type:`diagnostic_message_buffer` using
:func:`diagnostic_message_buffer_new`.

There are various API entrypoints for accumulating text into the buffer.

For example:

.. literalinclude:: ../../../testsuite/libgdiagnostics.dg/test-message-buffer.c
   :language: c
   :start-after: /* begin quoted source */
   :end-before:  /* end quoted source */

Running this will produce this text output::

.. code-block:: console

  $ ./test-message-buffer.c.exe
  ./test-message-buffer.c.exe: error: this is a string; foo; int: 42 str: mostly harmless; this is a link  'this is quoted' highlight A highlight B (1).

where in a suitably-capable terminal if a text sink is directly
connected to a tty:

* the ``this is a link`` will be a clickable hyperlink
  (and the URL will be captured in SARIF output).

* the quoted text will be in bold

* the ``highlight A`` and ``highlight B`` text will be colorized

* the event ID will be colorized (and will be a URL in SARIF output
  if used within a :type:`diagnostic_execution_path`).


Moving on
*********

That's the end of the tutorial.  For more information on libgdiagnostics, see
the :doc:`topic guide <../topics/index>`.
