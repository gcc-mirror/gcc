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

Tutorial part 1: "Hello world"
==============================

Before we look at the details of the API, let's look at building and
running programs that use the library.

Here's a toy program that uses libdiagnostics to emit an error message
to stderr.

  .. literalinclude:: ../../../testsuite/libdiagnostics.dg/test-example-1.c
     :language: c
     :start-after: /* begin quoted source */
     :end-before:  /* end quoted source */

Copy the above to `tut01-hello-world.c`.

Assuming you have libdiagnostics installed, build the test program
using:

.. code-block:: console

  $ gcc \
      tut01-hello-world.c \
      -o tut01-hello-world \
      -ldiagnostics

You should then be able to run the built program:

.. code-block:: console

  $ ./tut01-hello-world
  progname: error: I'm sorry Dave, I'm afraid I can't do that

If stderr is connected to a terminal, you should get colorized output
(using `SGR control codes <https://en.wikipedia.org/wiki/ANSI_escape_code>`_).

.. image:: example-1.png

Otherwise, the output will be plain text.

Obviously a trivial example like the above could be done using ``fprintf``
on stderr, and it's fairly easy to colorize text at the terminal.

In :doc:`the next part of the tutorial <02-physical-locations>` we'll add
file/location information to our error messages, and libdiagnostics will
quote the pertinent parts of the file, underlining them, which is less trivial
to reimplement.  libdiagnostics gives us many other such abilities, such as
fix-it hints and execution paths, which we'll cover in the following
tutorials.  Also, once a program's diagnostics are using libdiagnostics,
it is trivial to add support for outputting them in
machine-readable form as :doc:`SARIF <../topics/sarif>`.


Structure
*********

The above example shows the typical structure of a program using
libdiagnostics:

* **initialization**: create a :type:`diagnostic_manager` instance,
  and create an output sink for it, and other one-time initialization

* **emission**: create various :type:`diagnostic` instances, populating
  them with data, and calling "finish" once they're ready to be emitted.
  :doc:`Text sinks <../topics/text-output>` emit their diagnostics as soon
  as "finish" is called on them.

* **cleanup**: call :func:`diagnostic_manager_release` on the
  :type:`diagnostic_manager` to finish and free up resources.
  :doc:`SARIF sinks <../topics/sarif>` write their output when
  :func:`diagnostic_manager_release` is called on the manager.

For non-trivial examples we'll also want to create location information,
which could happen during initialization, or during a parsing phase of
the program using libdiagnostics.  See :doc:`02-physical-locations` for
more information.


Formatted messages
******************

The above example uses :func:`diagnostic_finish`, which takes a format
string and arguments.  libdiagnostics has its own style of format
string arguments used for :func:`diagnostic_finish` and some other
entrypoints.

.. note:: The format syntax is *not* the same as ``printf``; see
   :doc:`supported formatting options <../topics/message-formatting>`.

You can use the ``q`` modifier on arguments
to quote them, so, for example ``%qs`` is a quoted string, consuming a
``const char *`` argument::

   diagnostic_finish (d, "can't find %qs", "foo");

This gives output like this:

.. code-block:: console

  progname: error: can't find ‘foo’

where the quoted string will appear in bold in a suitably-capable
terminal, and the quotes will be internationalized, so that e.g. with
``LANG=fr_FR.UTF8`` we might get:

.. code-block:: console

  progname: erreur: can't find « free »

Note that:

* the string ``error`` has been localized by libdiagnostics to
  ``erreur``,

* locale-specific quoting has been used (``«`` and ``»`` rather than
  ``‘`` and ``’``),

* ``foo`` hasn't been localized - you would typically use quoted strings
  for referring to identifiers in the input language (such as function names
  in code, property names in JSON, etc),

* the message itself hasn't been localized: you are responsible for
  passing a translated format string to :func:`diagnostic_finish` if you
  want to internationalize the output.

There are many :doc:`supported formatting options <../topics/message-formatting>`.


Naming the program
******************

In the above output the message was preceded with ``progname``.  This
appears for diagnostics that don't have any location information associated
with them.  We'll look at setting up location information in the
:doc:`next tutorial <02-physical-locations>`, but we can override this
default name via :func:`diagnostic_manager_set_tool_name`::

   diagnostic_manager_set_tool_name (diag_mgr, "my-awesome-checker");

leading to output like this::

   my-awesome-checker: error: can't find ‘foo’

There are various other functions for
:doc:`supplying metadata to libdiagnostics <../../topics/metadata>`.


Moving beyond trivial examples
******************************

Obviously it's not very useful if we can't refer to specific files and
specific locations in those files in our diagnostics, so read
:doc:`part 2 of the tutorial <02-physical-locations>` for information on
how to do this.
