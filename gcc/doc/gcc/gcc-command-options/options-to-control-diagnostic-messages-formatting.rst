..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: options to control diagnostics formatting, diagnostic messages, message formatting

.. _diagnostic-message-formatting-options:

Options to Control Diagnostic Messages Formatting
*************************************************

Traditionally, diagnostic messages have been formatted irrespective of
the output device's aspect (e.g. its width, ...).  You can use the
options described below
to control the formatting algorithm for diagnostic messages,
e.g. how many characters per line, how often source location
information should be reported.  Note that some language front ends may not
honor these options.

.. option:: -fmessage-length={n}

  Try to format error messages so that they fit on lines of about
  :samp:`{n}` characters.  If :samp:`{n}` is zero, then no line-wrapping is
  done; each error message appears on a single line.  This is the
  default for all front ends.

  Note - this option also affects the display of the :samp:`#error` and
  :samp:`#warning` pre-processor directives, and the :samp:`deprecated`
  function/type/variable attribute.  It does not however affect the
  :samp:`pragma GCC warning` and :samp:`pragma GCC error` pragmas.

.. option:: -fdiagnostics-plain-output

  This option requests that diagnostic output look as plain as possible, which
  may be useful when running :command:`dejagnu` or other utilities that need to
  parse diagnostics output and prefer that it remain more stable over time.
  :option:`-fdiagnostics-plain-output` is currently equivalent to the following
  options:

  :option:`-fno-diagnostics-show-caret` |gol|
  :option:`-fno-diagnostics-show-line-numbers` |gol|
  :option:`-fdiagnostics-color=never` |gol|
  :option:`-fdiagnostics-urls=never` |gol|
  :option:`-fdiagnostics-path-format=separate-events`
  In the future, if GCC changes the default appearance of its diagnostics, the
  corresponding option to disable the new behavior will be added to this list.

.. option:: -fdiagnostics-show-location=once

  Only meaningful in line-wrapping mode.  Instructs the diagnostic messages
  reporter to emit source location information *once*; that is, in
  case the message is too long to fit on a single physical line and has to
  be wrapped, the source location won't be emitted (as prefix) again,
  over and over, in subsequent continuation lines.  This is the default
  behavior.

.. option:: -fdiagnostics-show-location=every-line

  Only meaningful in line-wrapping mode.  Instructs the diagnostic
  messages reporter to emit the same source location information (as
  prefix) for physical lines that result from the process of breaking
  a message which is too long to fit on a single line.

.. index:: highlight, color, GCC_COLORS environment variable

.. option:: -fdiagnostics-color[={WHEN}]

  Use color in diagnostics.  :samp:`{WHEN}` is :samp:`never`, :samp:`always`,
  or :samp:`auto`.  The default depends on how the compiler has been configured,
  it can be any of the above :samp:`{WHEN}` options or also :samp:`never`
  if :envvar:`GCC_COLORS` environment variable isn't present in the environment,
  and :samp:`auto` otherwise.
  :samp:`auto` makes GCC use color only when the standard error is a terminal,
  and when not executing in an emacs shell.
  The forms :option:`-fdiagnostics-color` and :option:`-fno-diagnostics-color` are
  aliases for :option:`-fdiagnostics-color=always` and
  :option:`-fdiagnostics-color=never`, respectively.

  The colors are defined by the environment variable :envvar:`GCC_COLORS`.
  Its value is a colon-separated list of capabilities and Select Graphic
  Rendition (SGR) substrings. SGR commands are interpreted by the
  terminal or terminal emulator.  (See the section in the documentation
  of your text terminal for permitted values and their meanings as
  character attributes.)  These substring values are integers in decimal
  representation and can be concatenated with semicolons.
  Common values to concatenate include
  :samp:`1` for bold,
  :samp:`4` for underline,
  :samp:`5` for blink,
  :samp:`7` for inverse,
  :samp:`39` for default foreground color,
  :samp:`30` to :samp:`37` for foreground colors,
  :samp:`90` to :samp:`97` for 16-color mode foreground colors,
  :samp:`38;5;0` to :samp:`38;5;255`
  for 88-color and 256-color modes foreground colors,
  :samp:`49` for default background color,
  :samp:`40` to :samp:`47` for background colors,
  :samp:`100` to :samp:`107` for 16-color mode background colors,
  and :samp:`48;5;0` to :samp:`48;5;255`
  for 88-color and 256-color modes background colors.

  The default :envvar:`GCC_COLORS` is

  .. code-block::

    error=01;31:warning=01;35:note=01;36:range1=32:range2=34:locus=01:\
    quote=01:path=01;36:fixit-insert=32:fixit-delete=31:\
    diff-filename=01:diff-hunk=32:diff-delete=31:diff-insert=32:\
    type-diff=01;32:fnname=01;32:targs=35

  where :samp:`01;31` is bold red, :samp:`01;35` is bold magenta,
  :samp:`01;36` is bold cyan, :samp:`32` is green, :samp:`34` is blue,
  :samp:`01` is bold, and :samp:`31` is red.
  Setting :envvar:`GCC_COLORS` to the empty string disables colors.
  Supported capabilities are as follows.

  ``error=``

    .. index:: error GCC_COLORS capability

    SGR substring for error: markers.

  ``warning=``

    .. index:: warning GCC_COLORS capability

    SGR substring for warning: markers.

  ``note=``

    .. index:: note GCC_COLORS capability

    SGR substring for note: markers.

  ``path=``

    .. index:: path GCC_COLORS capability

    SGR substring for colorizing paths of control-flow events as printed
    via :option:`-fdiagnostics-path-format=`, such as the identifiers of
    individual events and lines indicating interprocedural calls and returns.

  ``range1=``

    .. index:: range1 GCC_COLORS capability

    SGR substring for first additional range.

  ``range2=``

    .. index:: range2 GCC_COLORS capability

    SGR substring for second additional range.

  ``locus=``

    .. index:: locus GCC_COLORS capability

    SGR substring for location information, :samp:`file:line` or
    :samp:`file:line:column` etc.

  ``quote=``

    .. index:: quote GCC_COLORS capability

    SGR substring for information printed within quotes.

  ``fnname=``

    .. index:: fnname GCC_COLORS capability

    SGR substring for names of C++ functions.

  ``targs=``

    .. index:: targs GCC_COLORS capability

    SGR substring for C++ function template parameter bindings.

  ``fixit-insert=``

    .. index:: fixit-insert GCC_COLORS capability

    SGR substring for fix-it hints suggesting text to
    be inserted or replaced.

  ``fixit-delete=``

    .. index:: fixit-delete GCC_COLORS capability

    SGR substring for fix-it hints suggesting text to
    be deleted.

  ``diff-filename=``

    .. index:: diff-filename GCC_COLORS capability

    SGR substring for filename headers within generated patches.

  ``diff-hunk=``

    .. index:: diff-hunk GCC_COLORS capability

    SGR substring for the starts of hunks within generated patches.

  ``diff-delete=``

    .. index:: diff-delete GCC_COLORS capability

    SGR substring for deleted lines within generated patches.

  ``diff-insert=``

    .. index:: diff-insert GCC_COLORS capability

    SGR substring for inserted lines within generated patches.

  ``type-diff=``

    .. index:: type-diff GCC_COLORS capability

    SGR substring for highlighting mismatching types within template
    arguments in the C++ frontend.

.. option:: -fdiagnostics-color

  Default setting; overrides :option:`-fno-diagnostics-color`.

.. index:: urls, GCC_URLS environment variable, TERM_URLS environment variable

.. option:: -fdiagnostics-urls[={WHEN}]

  Use escape sequences to embed URLs in diagnostics.  For example, when
  :option:`-fdiagnostics-show-option` emits text showing the command-line
  option controlling a diagnostic, embed a URL for documentation of that
  option.

  :samp:`{WHEN}` is :samp:`never`, :samp:`always`, or :samp:`auto`.
  :samp:`auto` makes GCC use URL escape sequences only when the standard error
  is a terminal, and when not executing in an emacs shell or any graphical
  terminal which is known to be incompatible with this feature, see below.

  The default depends on how the compiler has been configured.
  It can be any of the above :samp:`{WHEN}` options.

  GCC can also be configured (via the
  :option:`--with-diagnostics-urls=auto-if-env` configure-time option)
  so that the default is affected by environment variables.
  Under such a configuration, GCC defaults to using :samp:`auto`
  if either :envvar:`GCC_URLS` or :envvar:`TERM_URLS` environment variables are
  present and non-empty in the environment of the compiler, or :samp:`never`
  if neither are.

  However, even with :option:`-fdiagnostics-urls=always` the behavior is
  dependent on those environment variables:
  If :envvar:`GCC_URLS` is set to empty or :samp:`no`, do not embed URLs in
  diagnostics.  If set to :samp:`st`, URLs use ST escape sequences.
  If set to :samp:`bel`, the default, URLs use BEL escape sequences.
  Any other non-empty value enables the feature.
  If :envvar:`GCC_URLS` is not set, use :envvar:`TERM_URLS` as a fallback.
  Note: ST is an ANSI escape sequence, string terminator :samp:`ESC \\`,
  BEL is an ASCII character, CTRL-G that usually sounds like a beep.

  At this time GCC tries to detect also a few terminals that are known to
  not implement the URL feature, and have bugs or at least had bugs in
  some versions that are still in use, where the URL escapes are likely
  to misbehave, i.e. print garbage on the screen.
  That list is currently xfce4-terminal, certain known to be buggy
  gnome-terminal versions, the linux console, and mingw.
  This check can be skipped with the :option:`-fdiagnostics-urls=always`.

.. option:: -fno-diagnostics-show-option

  By default, each diagnostic emitted includes text indicating the
  command-line option that directly controls the diagnostic (if such an
  option is known to the diagnostic machinery).  Specifying the
  :option:`-fno-diagnostics-show-option` flag suppresses that behavior.

.. option:: -fdiagnostics-show-option

  Default setting; overrides :option:`-fno-diagnostics-show-option`.

.. option:: -fno-diagnostics-show-caret

  By default, each diagnostic emitted includes the original source line
  and a caret :samp:`^` indicating the column.  This option suppresses this
  information.  The source line is truncated to :samp:`{n}` characters, if
  the :option:`-fmessage-length=n` option is given.  When the output is done
  to the terminal, the width is limited to the width given by the
  :envvar:`COLUMNS` environment variable or, if not set, to the terminal width.

.. option:: -fdiagnostics-show-caret

  Default setting; overrides :option:`-fno-diagnostics-show-caret`.

.. option:: -fno-diagnostics-show-labels

  By default, when printing source code (via :option:`-fdiagnostics-show-caret`),
  diagnostics can label ranges of source code with pertinent information, such
  as the types of expressions:

  .. code-block::

        printf ("foo %s bar", long_i + long_j);
                     ~^       ~~~~~~~~~~~~~~~
                      |              |
                      char *         long int

  This option suppresses the printing of these labels (in the example above,
  the vertical bars and the 'char \*' and 'long int' text).

.. option:: -fdiagnostics-show-labels

  Default setting; overrides :option:`-fno-diagnostics-show-labels`.

.. option:: -fno-diagnostics-show-cwe

  Diagnostic messages can optionally have an associated
  `CWE <https://cwe.mitre.org/index.html>`_ identifier.
  GCC itself only provides such metadata for some of the :option:`-fanalyzer`
  diagnostics.  GCC plugins may also provide diagnostics with such metadata.
  By default, if this information is present, it will be printed with
  the diagnostic.  This option suppresses the printing of this metadata.

.. option:: -fdiagnostics-show-cwe

  Default setting; overrides :option:`-fno-diagnostics-show-cwe`.

.. option:: -fno-diagnostics-show-rules

  Diagnostic messages can optionally have rules associated with them, such
  as from a coding standard, or a specification.
  GCC itself does not do this for any of its diagnostics, but plugins may do so.
  By default, if this information is present, it will be printed with
  the diagnostic.  This option suppresses the printing of this metadata.

.. option:: -fdiagnostics-show-rules

  Default setting; overrides :option:`-fno-diagnostics-show-rules`.

.. option:: -fno-diagnostics-show-line-numbers

  By default, when printing source code (via :option:`-fdiagnostics-show-caret`),
  a left margin is printed, showing line numbers.  This option suppresses this
  left margin.

.. option:: -fdiagnostics-show-line-numbers

  Default setting; overrides :option:`-fno-diagnostics-show-line-numbers`.

.. option:: -fdiagnostics-minimum-margin-width={width}

  This option controls the minimum width of the left margin printed by
  :option:`-fdiagnostics-show-line-numbers`.  It defaults to 6.

.. option:: -fdiagnostics-parseable-fixits

  Emit fix-it hints in a machine-parseable format, suitable for consumption
  by IDEs.  For each fix-it, a line will be printed after the relevant
  diagnostic, starting with the string 'fix-it:'.  For example:

  .. code-block::

    fix-it:"test.c":{45:3-45:21}:"gtk_widget_show_all"

  The location is expressed as a half-open range, expressed as a count of
  bytes, starting at byte 1 for the initial column.  In the above example,
  bytes 3 through 20 of line 45 of 'test.c' are to be replaced with the
  given string:

  .. code-block::

    00000000011111111112222222222
    12345678901234567890123456789
      gtk_widget_showall (dlg);
      ^^^^^^^^^^^^^^^^^^
      gtk_widget_show_all

  The filename and replacement string escape backslash as '\\", tab as '\t',
  newline as '\n', double quotes as '\"', non-printable characters as octal
  (e.g. vertical tab as '\013').

  An empty replacement string indicates that the given range is to be removed.
  An empty range (e.g. '45:3-45:3') indicates that the string is to
  be inserted at the given position.

.. option:: -fdiagnostics-generate-patch

  Print fix-it hints to stderr in unified diff format, after any diagnostics
  are printed.  For example:

  .. code-block:: diff

    --- test.c
    +++ test.c
    @ -42,5 +42,5 @

     void show_cb(GtkDialog *dlg)
     {
    -  gtk_widget_showall(dlg);
    +  gtk_widget_show_all(dlg);
     }

  The diff may or may not be colorized, following the same rules
  as for diagnostics (see :option:`-fdiagnostics-color`).

.. option:: -fdiagnostics-show-template-tree

  In the C++ frontend, when printing diagnostics showing mismatching
  template types, such as:

  .. code-block::

      could not convert 'std::map<int, std::vector<double> >()'
        from 'map<[...],vector<double>>' to 'map<[...],vector<float>>

  the :option:`-fdiagnostics-show-template-tree` flag enables printing a
  tree-like structure showing the common and differing parts of the types,
  such as:

  .. code-block::

      map<
        [...],
        vector<
          [double != float]>>

  The parts that differ are highlighted with color ('double' and
  'float' in this case).

.. option:: -fno-elide-type

  By default when the C++ frontend prints diagnostics showing mismatching
  template types, common parts of the types are printed as '[...]' to
  simplify the error message.  For example:

  .. code-block::

      could not convert 'std::map<int, std::vector<double> >()'
        from 'map<[...],vector<double>>' to 'map<[...],vector<float>>

  Specifying the :option:`-fno-elide-type` flag suppresses that behavior.
  This flag also affects the output of the
  :option:`-fdiagnostics-show-template-tree` flag.

.. option:: -felide-type

  Default setting; overrides :option:`-fno-elide-type`.

.. option:: -fdiagnostics-path-format={KIND}

  Specify how to print paths of control-flow events for diagnostics that
  have such a path associated with them.

  :samp:`{KIND}` is :samp:`none`, :samp:`separate-events`, or :samp:`inline-events`,
  the default.

  :samp:`none` means to not print diagnostic paths.

  :samp:`separate-events` means to print a separate 'note' diagnostic for
  each event within the diagnostic.  For example:

  .. code-block::

    test.c:29:5: error: passing NULL as argument 1 to 'PyList_Append' which requires a non-NULL parameter
    test.c:25:10: note: (1) when 'PyList_New' fails, returning NULL
    test.c:27:3: note: (2) when 'i < count'
    test.c:29:5: note: (3) when calling 'PyList_Append', passing NULL from (1) as argument 1

  :samp:`inline-events` means to print the events 'inline' within the source
  code.  This view attempts to consolidate the events into runs of
  sufficiently-close events, printing them as labelled ranges within the source.

  For example, the same events as above might be printed as:

  .. code-block::

      'test': events 1-3
        |
        |   25 |   list = PyList_New(0);
        |      |          ^~~~~~~~~~~~~
        |      |          |
        |      |          (1) when 'PyList_New' fails, returning NULL
        |   26 |
        |   27 |   for (i = 0; i < count; i++) {
        |      |   ~~~
        |      |   |
        |      |   (2) when 'i < count'
        |   28 |     item = PyLong_FromLong(random());
        |   29 |     PyList_Append(list, item);
        |      |     ~~~~~~~~~~~~~~~~~~~~~~~~~
        |      |     |
        |      |     (3) when calling 'PyList_Append', passing NULL from (1) as argument 1
        |

  Interprocedural control flow is shown by grouping the events by stack frame,
  and using indentation to show how stack frames are nested, pushed, and popped.

  For example:

  .. code-block::

      'test': events 1-2
        |
        |  133 | {
        |      | ^
        |      | |
        |      | (1) entering 'test'
        |  134 |   boxed_int *obj = make_boxed_int (i);
        |      |                    ~~~~~~~~~~~~~~~~~~
        |      |                    |
        |      |                    (2) calling 'make_boxed_int'
        |
        +--> 'make_boxed_int': events 3-4
               |
               |  120 | {
               |      | ^
               |      | |
               |      | (3) entering 'make_boxed_int'
               |  121 |   boxed_int *result = (boxed_int *)wrapped_malloc (sizeof (boxed_int));
               |      |                                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               |      |                                    |
               |      |                                    (4) calling 'wrapped_malloc'
               |
               +--> 'wrapped_malloc': events 5-6
                      |
                      |    7 | {
                      |      | ^
                      |      | |
                      |      | (5) entering 'wrapped_malloc'
                      |    8 |   return malloc (size);
                      |      |          ~~~~~~~~~~~~~
                      |      |          |
                      |      |          (6) calling 'malloc'
                      |
        <-------------+
        |
     'test': event 7
        |
        |  138 |   free_boxed_int (obj);
        |      |   ^~~~~~~~~~~~~~~~~~~~
        |      |   |
        |      |   (7) calling 'free_boxed_int'
        |
    (etc)

.. option:: -fdiagnostics-show-path-depths

  This option provides additional information when printing control-flow paths
  associated with a diagnostic.

  If this is option is provided then the stack depth will be printed for
  each run of events within :option:`-fdiagnostics-path-format=inline-events`.
  If provided with :option:`-fdiagnostics-path-format=separate-events`, then
  the stack depth and function declaration will be appended when printing
  each event.

  This is intended for use by GCC developers and plugin developers when
  debugging diagnostics that report interprocedural control flow.

.. option:: -fno-show-column

  Do not print column numbers in diagnostics.  This may be necessary if
  diagnostics are being scanned by a program that does not understand the
  column numbers, such as :command:`dejagnu`.

.. option:: -fshow-column

  Default setting; overrides :option:`-fno-show-column`.

.. option:: -fdiagnostics-column-unit={UNIT}

  Select the units for the column number.  This affects traditional diagnostics
  (in the absence of :option:`-fno-show-column`), as well as JSON format
  diagnostics if requested.

  The default :samp:`{UNIT}`, :samp:`display`, considers the number of display
  columns occupied by each character.  This may be larger than the number
  of bytes required to encode the character, in the case of tab
  characters, or it may be smaller, in the case of multibyte characters.
  For example, the character 'GREEK SMALL LETTER PI (U+03C0)' occupies one
  display column, and its UTF-8 encoding requires two bytes; the character
  'SLIGHTLY SMILING FACE (U+1F642)' occupies two display columns, and
  its UTF-8 encoding requires four bytes.

  Setting :samp:`{UNIT}` to :samp:`byte` changes the column number to the raw byte
  count in all cases, as was traditionally output by GCC prior to version 11.1.0.

.. option:: -fdiagnostics-column-origin={ORIGIN}

  Select the origin for column numbers, i.e. the column number assigned to the
  first column.  The default value of 1 corresponds to traditional GCC
  behavior and to the GNU style guide.  Some utilities may perform better with an
  origin of 0; any non-negative value may be specified.

.. option:: -fdiagnostics-escape-format={FORMAT}

  When GCC prints pertinent source lines for a diagnostic it normally attempts
  to print the source bytes directly.  However, some diagnostics relate to encoding
  issues in the source file, such as malformed UTF-8, or issues with Unicode
  normalization.  These diagnostics are flagged so that GCC will escape bytes
  that are not printable ASCII when printing their pertinent source lines.

  This option controls how such bytes should be escaped.

  The default :samp:`{FORMAT}`, :samp:`unicode` displays Unicode characters that
  are not printable ASCII in the form :samp:`<U+XXXX>`, and bytes that do not
  correspond to a Unicode character validly-encoded in UTF-8-encoded will be
  displayed as hexadecimal in the form :samp:`<XX>`.

  For example, a source line containing the string :samp:`before` followed by the
  Unicode character U+03C0 ('GREEK SMALL LETTER PI', with UTF-8 encoding
  0xCF 0x80) followed by the byte 0xBF (a stray UTF-8 trailing byte), followed by
  the string :samp:`after` will be printed for such a diagnostic as:

  .. code-block:: c++

     before<U+03C0><BF>after

  Setting :samp:`{FORMAT}` to :samp:`bytes` will display all non-printable-ASCII bytes
  in the form :samp:`<XX>`, thus showing the underlying encoding of non-ASCII
  Unicode characters.  For the example above, the following will be printed:

  .. code-block:: c++

     before<CF><80><BF>after

.. option:: -fdiagnostics-format={FORMAT}

  Select a different format for printing diagnostics.
  :samp:`{FORMAT}` is :samp:`text`, :samp:`sarif-stderr`, :samp:`sarif-file`,
  :samp:`json`, :samp:`json-stderr`, or :samp:`json-file`.

  The default is :samp:`text`.

  The :samp:`sarif-stderr` and :samp:`sarif-file` formats both emit
  diagnostics in SARIF Version 2.1.0 format, either to stderr, or to a file
  named :samp:`{source}.sarif`, respectively.

  The :samp:`json` format is a synonym for :samp:`json-stderr`.
  The :samp:`json-stderr` and :samp:`json-file` formats are identical, apart from
  where the JSON is emitted to - with the former, the JSON is emitted to stderr,
  whereas with :samp:`json-file` it is written to :samp:`{source}.gcc.json`.

  The emitted JSON consists of a top-level JSON array containing JSON objects
  representing the diagnostics.  The JSON is emitted as one line, without
  formatting; the examples below have been formatted for clarity.

  Diagnostics can have child diagnostics.  For example, this error and note:

  .. code-block::

    misleading-indentation.c:15:3: warning: this 'if' clause does not
      guard... [-Wmisleading-indentation]
       15 |   if (flag)
          |   ^~
    misleading-indentation.c:17:5: note: ...this statement, but the latter
      is misleadingly indented as if it were guarded by the 'if'
       17 |     y = 2;
          |     ^

  might be printed in JSON form (after formatting) like this:

  .. code-block:: json

    [
        {
            "kind": "warning",
            "locations": [
                {
                    "caret": {
    		    "display-column": 3,
    		    "byte-column": 3,
                        "column": 3,
                        "file": "misleading-indentation.c",
                        "line": 15
                    },
                    "finish": {
    		    "display-column": 4,
    		    "byte-column": 4,
                        "column": 4,
                        "file": "misleading-indentation.c",
                        "line": 15
                    }
                }
            ],
            "message": "this ‘if’ clause does not guard...",
            "option": "-Wmisleading-indentation",
            "option_url": "https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Wmisleading-indentation",
            "children": [
                {
                    "kind": "note",
                    "locations": [
                        {
                            "caret": {
    			    "display-column": 5,
    			    "byte-column": 5,
                                "column": 5,
                                "file": "misleading-indentation.c",
                                "line": 17
                            }
                        }
                    ],
                    "escape-source": false,
                    "message": "...this statement, but the latter is ..."
                }
            ]
    	"escape-source": false,
    	"column-origin": 1,
        }
    ]

  where the ``note`` is a child of the ``warning``.

  A diagnostic has a ``kind``.  If this is ``warning``, then there is
  an ``option`` key describing the command-line option controlling the
  warning.

  A diagnostic can contain zero or more locations.  Each location has an
  optional ``label`` string and up to three positions within it: a
  ``caret`` position and optional ``start`` and ``finish`` positions.
  A position is described by a ``file`` name, a ``line`` number, and
  three numbers indicating a column position:

  * ``display-column`` counts display columns, accounting for tabs and
    multibyte characters.

  * ``byte-column`` counts raw bytes.

  * ``column`` is equal to one of
    the previous two, as dictated by the :option:`-fdiagnostics-column-unit`
    option.

  All three columns are relative to the origin specified by
  :option:`-fdiagnostics-column-origin`, which is typically equal to 1 but may
  be set, for instance, to 0 for compatibility with other utilities that
  number columns from 0.  The column origin is recorded in the JSON output in
  the ``column-origin`` tag.  In the remaining examples below, the extra
  column number outputs have been omitted for brevity.

  For example, this error:

  .. code-block::

    bad-binary-ops.c:64:23: error: invalid operands to binary + (have 'S' {aka
       'struct s'} and 'T' {aka 'struct t'})
       64 |   return callee_4a () + callee_4b ();
          |          ~~~~~~~~~~~~ ^ ~~~~~~~~~~~~
          |          |              |
          |          |              T {aka struct t}
          |          S {aka struct s}

  has three locations.  Its primary location is at the '+' token at column
  23.  It has two secondary locations, describing the left and right-hand sides
  of the expression, which have labels.  It might be printed in JSON form as:

  .. code-block:: json

        {
            "children": [],
            "kind": "error",
            "locations": [
                {
                    "caret": {
                        "column": 23, "file": "bad-binary-ops.c", "line": 64
                    }
                },
                {
                    "caret": {
                        "column": 10, "file": "bad-binary-ops.c", "line": 64
                    },
                    "finish": {
                        "column": 21, "file": "bad-binary-ops.c", "line": 64
                    },
                    "label": "S {aka struct s}"
                },
                {
                    "caret": {
                        "column": 25, "file": "bad-binary-ops.c", "line": 64
                    },
                    "finish": {
                        "column": 36, "file": "bad-binary-ops.c", "line": 64
                    },
                    "label": "T {aka struct t}"
                }
            ],
            "escape-source": false,
            "message": "invalid operands to binary + ..."
        }

  If a diagnostic contains fix-it hints, it has a ``fixits`` array,
  consisting of half-open intervals, similar to the output of
  :option:`-fdiagnostics-parseable-fixits`.  For example, this diagnostic
  with a replacement fix-it hint:

  .. code-block::

    demo.c:8:15: error: 'struct s' has no member named 'colour'; did you
      mean 'color'?
        8 |   return ptr->colour;
          |               ^~~~~~
          |               color

  might be printed in JSON form as:

  .. code-block:: json

        {
            "children": [],
            "fixits": [
                {
                    "next": {
                        "column": 21,
                        "file": "demo.c",
                        "line": 8
                    },
                    "start": {
                        "column": 15,
                        "file": "demo.c",
                        "line": 8
                    },
                    "string": "color"
                }
            ],
            "kind": "error",
            "locations": [
                {
                    "caret": {
                        "column": 15,
                        "file": "demo.c",
                        "line": 8
                    },
                    "finish": {
                        "column": 20,
                        "file": "demo.c",
                        "line": 8
                    }
                }
            ],
            "escape-source": false,
            "message": "‘struct s’ has no member named ..."
        }

  where the fix-it hint suggests replacing the text from ``start`` up
  to but not including ``next`` with ``string`` 's value.  Deletions
  are expressed via an empty value for ``string``, insertions by
  having ``start`` equal ``next``.

  If the diagnostic has a path of control-flow events associated with it,
  it has a ``path`` array of objects representing the events.  Each
  event object has a ``description`` string, a ``location`` object,
  along with a ``function`` string and a ``depth`` number for
  representing interprocedural paths.  The ``function`` represents the
  current function at that event, and the ``depth`` represents the
  stack depth relative to some baseline: the higher, the more frames are
  within the stack.

  For example, the intraprocedural example shown for
  :option:`-fdiagnostics-path-format=` might have this JSON for its path:

  .. code-block:: json

        "path": [
            {
                "depth": 0,
                "description": "when 'PyList_New' fails, returning NULL",
                "function": "test",
                "location": {
                    "column": 10,
                    "file": "test.c",
                    "line": 25
                }
            },
            {
                "depth": 0,
                "description": "when 'i < count'",
                "function": "test",
                "location": {
                    "column": 3,
                    "file": "test.c",
                    "line": 27
                }
            },
            {
                "depth": 0,
                "description": "when calling 'PyList_Append', passing NULL from (1) as argument 1",
                "function": "test",
                "location": {
                    "column": 5,
                    "file": "test.c",
                    "line": 29
                }
            }
        ]

  Diagnostics have a boolean attribute ``escape-source``, hinting whether
  non-ASCII bytes should be escaped when printing the pertinent lines of
  source code (``true`` for diagnostics involving source encoding issues).