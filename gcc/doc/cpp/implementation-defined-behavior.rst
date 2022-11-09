..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _implementation-defined-behavior:

.. _identifier-characters:

Implementation-defined behavior
*******************************

This is how CPP behaves in all the cases which the C standard
describes as :dfn:`implementation-defined`.  This term means that the
implementation is free to do what it likes, but must document its choice
and stick to it.

.. todo:: Check the C++ standard for more implementation-defined stuff.

* The mapping of physical source file multi-byte characters to the
  execution character set.

  The input character set can be specified using the
  :option:`-finput-charset` option, while the execution character set may
  be controlled using the :option:`-fexec-charset` and
  :option:`-fwide-exec-charset` options.

* Identifier characters.

  The C and C++ standards allow identifiers to be composed of :samp:`_`
  and the alphanumeric characters.  C++ also allows universal character
  names.  C99 and later C standards permit both universal character
  names and implementation-defined characters.  In both C and C++ modes,
  GCC accepts in identifiers exactly those extended characters that
  correspond to universal character names permitted by the chosen
  standard.

  GCC allows the :samp:`$` character in identifiers as an extension for
  most targets.  This is true regardless of the std= switch,
  since this extension cannot conflict with standards-conforming
  programs.  When preprocessing assembler, however, dollars are not
  identifier characters by default.

  Currently the targets that by default do not permit :samp:`$` are AVR,
  IP2K, MMIX, MIPS Irix 3, ARM aout, and PowerPC targets for the AIX
  operating system.

  You can override the default with :option:`-fdollars-in-identifiers` or
  :option:`-fno-dollars-in-identifiers`.  See :option:`-fdollars-in-identifiers`.

* Non-empty sequences of whitespace characters.

  In textual output, each whitespace sequence is collapsed to a single
  space.  For aesthetic reasons, the first token on each non-directive
  line of output is preceded with sufficient spaces that it appears in the
  same column as it did in the original source file.

* The numeric value of character constants in preprocessor expressions.

  The preprocessor and compiler interpret character constants in the
  same way; i.e. escape sequences such as :samp:`\\a` are given the
  values they would have on the target machine.

  The compiler evaluates a multi-character character constant a character
  at a time, shifting the previous value left by the number of bits per
  target character, and then or-ing in the bit-pattern of the new
  character truncated to the width of a target character.  The final
  bit-pattern is given type ``int``, and is therefore signed,
  regardless of whether single characters are signed or not.
  If there are more
  characters in the constant than would fit in the target ``int`` the
  compiler issues a warning, and the excess leading characters are
  ignored.

  For example, ``'ab'`` for a target with an 8-bit ``char`` would be
  interpreted as :samp:`(int) ((unsigned char) 'a' * 256 + (unsigned char)
  'b')`, and ``'\234a'`` as :samp:`(int) ((unsigned char) '\\234' *
  256 + (unsigned char) 'a')`.

* Source file inclusion.

  For a discussion on how the preprocessor locates header files,
  :ref:`include-operation`.

* Interpretation of the filename resulting from a macro-expanded
  :samp:`#include` directive.

  See :ref:`computed-includes`.

* Treatment of a :samp:`#pragma` directive that after macro-expansion
  results in a standard pragma.

  No macro expansion occurs on any :samp:`#pragma` directive line, so the
  question does not arise.

  Note that GCC does not yet implement any of the standard
  pragmas.
