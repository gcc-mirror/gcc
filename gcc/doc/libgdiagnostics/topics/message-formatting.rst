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

Message formatting
==================

Various libgdiagnostics entrypoints take a format string and
variadic arguments.

The format strings take codes prefixed by ``%``, or ``%q`` to put
the result in quotes.  For example::

   "hello %s", "world"

would print::

   hello world

whereas::

   "hello %qs", "world"

would print::

   hello `world'

where ```world'`` would be displayed in bold if colorization were enabled
in the terminal.

The following format specifiers are accepted:


Numbers
*******

``d`` and ``i`` (``signed int``), ``u`` (``unsigned int``)
   ``%d``, ``%i``, and ``%u`` print integers in base ten.  For example::

     "the answer is %i", 42

   would print::

     the answer is 42

``o`` (``unsigned int``)
   Print the integer in base eight

``x`` (``unsigned int``)
   Print the integer in base sixteen

The above can be prefixed with ``l`` and ``ll`` prefixes to take
``long`` and ``long long`` values of the appropriate signedness.

For example::

   "address: %lx", (unsigned long)0x108b516

would print::

   address: 108b516

Similarly, the prefix ``z`` can be used for ``size_t``::

  "size: %zd", sizeof(struct foo)
  size: 32

and ``t`` for ptrdiff_t.
  
``f`` (``double``)
   ``%f`` prints a floating-point value.  For example::

     "value: %f", 1.0

   might print::

     value: 1.000000


Strings
*******

``c`` (``char``)
   ``%c`` prints a single character.

``s`` (``const char *``)
   ``%s`` prints a string.

   Note that if the string refers to something that might
   appear in the input file (such as the name of a function), it's better
   to quote the value; for example::

     "unrecognized identifier: %qs", "foo"

   might print::

     unrecognized identifier: `foo'

``m`` (no argument)
   Prints ``strerror(errno)``, for example::

     "can't open %qs: %m"

   might print::

     can't open `foo.txt': No such file or directory

``%`` (no argument)
   ``%%`` prints a `%` character, for example::

     "8%% of 75 is 75%% of 8, and is thus 6"

   prints::

     8% of 75 is 75% of 8, and is thus 6

``'`` (no argument)
  ``%'`` prints an apostrophe.  This should only be used in untranslated messages;
  translations should use appropriate punctuation directly.


Other format specifiers
***********************

``p`` (pointer)
   ``%p`` prints a pointer, although the precise format is
   implementation-defined.

``r`` (``const char *``)
   ``%r`` starts colorization on suitable text sinks, where the argument
   specifies the name of the kind of entity to be colored, such as ``error``.

``R`` (no argument)
   ``%R`` stops colorization

``<`` and ``>`` (no arguments)
   ``%<`` adds an opening quote and ``%>`` a closing quote, such as::

     "missing element %<%s:%s%>", ns, name

   which might be printed as::

     missing element `xhtml:head'

   If the thing to be quoted can be handled with another format specifier,
   then it's simpler to use ``q`` with it.  For example, it's much
   simpler to print a ``const char *`` in quotes via::

      "%qs", str

   rather than the error-prone::
     
      "%<%s%>", str

``{`` (``const char *``)
   ``%{`` starts a link; the argument is the URL.  This will be displayed
   in a suitably-capable terminal if a text sink is directly connected to
   a tty, and will be captured in SARIF output.

``}`` (no argument)
   ``%}`` stops a link started with ``%{``.

   For example::

      "for more information see %{the documentation%}", "https://example.com"

   would be printed as::

      for more information see the documentation

   with the URL emitted in suitable output sinks.

``@`` (``diagnostic_event_id *``)
   ``%@`` prints a reference to an event in a
   :type:`diagnostic_execution_path`, where the :type:`diagnostic_event_id`
   is passed by pointer.

   For example, if ``event_id`` refers to the first event in a path, then::

      "double-%qs of %qs; first %qs was at %@",
      function, ptr, function, &event_id

   might print::

     double-`free' of `p'; first `free` was at (1)

.. :

   TODO:

   %.*s: a substring the length of which is specified by an argument
	 integer.
   %Ns: likewise, but length specified as constant in the format string.
   %Z: Requires two arguments - array of int, and len. Prints elements
   of the array.

   %e: Consumes a pp_element * argument.

   Arguments can be used sequentially, or through %N$ resp. *N$
   notation Nth argument after the format string.  If %N$ / *N$
   notation is used, it must be used for all arguments, except %m, %%,
   %<, %>, %} and %', which may not have a number, as they do not consume
   an argument.  When %M$.*N$s is used, M must be N + 1.  (This may
   also be written %M$.*s, provided N is not otherwise used.)  The
   format string must have conversion specifiers with argument numbers
   1 up to highest argument; each argument may only be used once.
   A format string can have at most 30 arguments.  */


