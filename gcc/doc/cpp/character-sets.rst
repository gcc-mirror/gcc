..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _character-sets:

Character sets
**************

Source code character set processing in C and related languages is
rather complicated.  The C standard discusses two character sets, but
there are really at least four.

The files input to CPP might be in any character set at all.  CPP's
very first action, before it even looks for line boundaries, is to
convert the file into the character set it uses for internal
processing.  That set is what the C standard calls the :dfn:`source`
character set.  It must be isomorphic with ISO 10646, also known as
Unicode.  CPP uses the UTF-8 encoding of Unicode.

The character sets of the input files are specified using the
:option:`-finput-charset=` option.

All preprocessing work (the subject of the rest of this manual) is
carried out in the source character set.  If you request textual
output from the preprocessor with the :option:`-E` option, it will be
in UTF-8.

After preprocessing is complete, string and character constants are
converted again, into the :dfn:`execution` character set.  This
character set is under control of the user; the default is UTF-8,
matching the source character set.  Wide string and character
constants have their own character set, which is not called out
specifically in the standard.  Again, it is under control of the user.
The default is UTF-16 or UTF-32, whichever fits in the target's
``wchar_t`` type, in the target machine's byte
order [#f1]_.

Octal and hexadecimal escape sequences do not undergo
conversion; ``'\x12'`` has the value 0x12 regardless of the currently
selected execution character set.  All other escapes are replaced by
the character in the source character set that they represent, then
converted to the execution character set, just like unescaped
characters.

In identifiers, characters outside the ASCII range can be specified
with the :samp:`\\u` and :samp:`\\U` escapes or used directly in the input
encoding.  If strict ISO C90 conformance is specified with an option
such as :option:`-std=c90`, or :option:`-fno-extended-identifiers` is
used, then those constructs are not permitted in identifiers.

.. [#f1] UTF-16 does not meet the requirements of the C
  standard for a wide character set, but the choice of 16-bit
  ``wchar_t`` is enshrined in some system ABIs so we cannot fix
  this.
