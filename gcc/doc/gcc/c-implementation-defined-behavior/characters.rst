..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _characters-implementation:

Characters
**********

* The number of bits in a byte (C90 3.4, C99 and C11 3.6).

  Determined by ABI.

* The values of the members of the execution character set (C90,
  C99 and C11 5.2.1).

  Determined by ABI.

* The unique value of the member of the execution character set produced
  for each of the standard alphabetic escape sequences (C90, C99 and C11
  5.2.2).

  Determined by ABI.

* The value of a ``char`` object into which has been stored any
  character other than a member of the basic execution character set
  (C90 6.1.2.5, C99 and C11 6.2.5).

  Determined by ABI.

* Which of ``signed char`` or ``unsigned char`` has the same
  range, representation, and behavior as 'plain' ``char`` (C90
  6.1.2.5, C90 6.2.1.1, C99 and C11 6.2.5, C99 and C11 6.3.1.1).

  .. index:: fsigned-char, funsigned-char

  Determined by ABI.  The options :option:`-funsigned-char` and
  :option:`-fsigned-char` change the default.  See :ref:`c-dialect-options`.

* The mapping of members of the source character set (in character
  constants and string literals) to members of the execution character
  set (C90 6.1.3.4, C99 and C11 6.4.4.4, C90, C99 and C11 5.1.1.2).

  Determined by ABI.

* The value of an integer character constant containing more than one
  character or containing a character or escape sequence that does not map
  to a single-byte execution character (C90 6.1.3.4, C99 and C11 6.4.4.4).

  See :ref:`cpp:implementation-defined-behavior`.

* The value of a wide character constant containing more than one
  multibyte character or a single multibyte character that maps to
  multiple members of the extended execution character set, or
  containing a multibyte character or escape sequence not represented in
  the extended execution character set (C90 6.1.3.4, C99 and C11
  6.4.4.4).

  See :ref:`cpp:implementation-defined-behavior`.

* The current locale used to convert a wide character constant consisting
  of a single multibyte character that maps to a member of the extended
  execution character set into a corresponding wide character code (C90
  6.1.3.4, C99 and C11 6.4.4.4).

  See :ref:`cpp:implementation-defined-behavior`.

* Whether differently-prefixed wide string literal tokens can be
  concatenated and, if so, the treatment of the resulting multibyte
  character sequence (C11 6.4.5).

  Such tokens may not be concatenated.

* The current locale used to convert a wide string literal into
  corresponding wide character codes (C90 6.1.4, C99 and C11 6.4.5).

  See :ref:`cpp:implementation-defined-behavior`.

* The value of a string literal containing a multibyte character or escape
  sequence not represented in the execution character set (C90 6.1.4,
  C99 and C11 6.4.5).

  See :ref:`cpp:implementation-defined-behavior`.

* The encoding of any of ``wchar_t``, ``char16_t``, and
  ``char32_t`` where the corresponding standard encoding macro
  (``__STDC_ISO_10646__``, ``__STDC_UTF_16__``, or
  ``__STDC_UTF_32__``) is not defined (C11 6.10.8.2).

  See :ref:`cpp:implementation-defined-behavior`.  ``char16_t`` and
  ``char32_t`` literals are always encoded in UTF-16 and UTF-32
  respectively.