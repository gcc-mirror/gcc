..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: C standard, C standards, ANSI C standard, ANSI C, ANSI C89, C89, ANSI X3.159-1989, X3.159-1989, ISO C standard, ISO C, ISO C90, ISO/IEC 9899, ISO 9899, C90, ISO C94, C94, ISO C95, C95, ISO C99, C99, ISO C9X, C9X, ISO C11, C11, ISO C1X, C1X, ISO C17, C17, ISO C2X, C2X, Technical Corrigenda, TC1, Technical Corrigendum 1, TC2, Technical Corrigendum 2, TC3, Technical Corrigendum 3, AMD1, freestanding implementation, freestanding environment, hosted implementation, hosted environment, __STDC_HOSTED__, std, ansi, pedantic, pedantic-errors

C Language
**********

The original ANSI C standard (X3.159-1989) was ratified in 1989 and
published in 1990.  This standard was ratified as an ISO standard
(ISO/IEC 9899:1990) later in 1990.  There were no technical
differences between these publications, although the sections of the
ANSI standard were renumbered and became clauses in the ISO standard.
The ANSI
standard, but not the ISO standard, also came with a Rationale
document.
This standard, in both its forms, is commonly known as :dfn:`C89`, or
occasionally as :dfn:`C90`, from the dates of ratification.
To select this standard in GCC, use one of the options
:option:`-ansi`, :option:`-std=c90` or :option:`-std=iso9899:1990` ; to obtain
all the diagnostics required by the standard, you should also specify
:option:`-pedantic` (or :option:`-pedantic-errors` if you want them to be
errors rather than warnings).  See :ref:`c-dialect-options`.

Errors in the 1990 ISO C standard were corrected in two Technical
Corrigenda published in 1994 and 1996.  GCC does not support the
uncorrected version.

An amendment to the 1990 standard was published in 1995.  This
amendment added digraphs and ``__STDC_VERSION__`` to the language,
but otherwise concerned the library.  This amendment is commonly known
as :dfn:`AMD1`; the amended standard is sometimes known as :dfn:`C94` or
:dfn:`C95`.  To select this standard in GCC, use the option
:option:`-std=iso9899:199409` (with, as for other standard versions,
:option:`-pedantic` to receive all required diagnostics).

A new edition of the ISO C standard was published in 1999 as ISO/IEC
9899:1999, and is commonly known as :dfn:`C99`.  (While in
development, drafts of this standard version were referred to as
:dfn:`C9X`.)  GCC has substantially
complete support for this standard version; see
https://gcc.gnu.org/c99status.html for details.  To select this
standard, use :option:`-std=c99` or :option:`-std=iso9899:1999`.

Errors in the 1999 ISO C standard were corrected in three Technical
Corrigenda published in 2001, 2004 and 2007.  GCC does not support the
uncorrected version.

A fourth version of the C standard, known as :dfn:`C11`, was published
in 2011 as ISO/IEC 9899:2011.  (While in development, drafts of this
standard version were referred to as :dfn:`C1X`.)
GCC has substantially complete support
for this standard, enabled with :option:`-std=c11` or
:option:`-std=iso9899:2011`.  A version with corrections integrated was
prepared in 2017 and published in 2018 as ISO/IEC 9899:2018; it is
known as :dfn:`C17` and is supported with :option:`-std=c17` or
:option:`-std=iso9899:2017` ; the corrections are also applied with
:option:`-std=c11`, and the only difference between the options is the
value of ``__STDC_VERSION__``.

A further version of the C standard, known as :dfn:`C2X`, is under
development; experimental and incomplete support for this is enabled
with :option:`-std=c2x`.

By default, GCC provides some extensions to the C language that, on
rare occasions conflict with the C standard.  See :ref:`c-extensions`.
Some features that are part of the C99 standard
are accepted as extensions in C90 mode, and some features that are part
of the C11 standard are accepted as extensions in C90 and C99 modes.
Use of the
:option:`-std` options listed above disables these extensions where
they conflict with the C standard version selected.  You may also
select an extended version of the C language explicitly with
:option:`-std=gnu90` (for C90 with GNU extensions), :option:`-std=gnu99`
(for C99 with GNU extensions) or :option:`-std=gnu11` (for C11 with GNU
extensions).

The default, if no C language dialect options are given,
is :option:`-std=gnu17`.

The ISO C standard defines (in clause 4) two classes of conforming
implementation.  A :dfn:`conforming hosted implementation` supports the
whole standard including all the library facilities; a :dfn:`conforming
freestanding implementation` is only required to provide certain
library facilities: those in ``<float.h>``, ``<limits.h>``,
``<stdarg.h>``, and ``<stddef.h>`` ; since AMD1, also those in
``<iso646.h>`` ; since C99, also those in ``<stdbool.h>`` and
``<stdint.h>`` ; and since C11, also those in ``<stdalign.h>``
and ``<stdnoreturn.h>``.  In addition, complex types, added in C99, are not
required for freestanding implementations.

The standard also defines two environments for programs, a
:dfn:`freestanding environment`, required of all implementations and
which may not have library facilities beyond those required of
freestanding implementations, where the handling of program startup
and termination are implementation-defined; and a :dfn:`hosted
environment`, which is not required, in which all the library
facilities are provided and startup is through a function ``int
main (void)`` or ``int main (int, char *[])``.  An OS kernel is an example
of a program running in a freestanding environment;
a program using the facilities of an
operating system is an example of a program running in a hosted environment.

.. index:: ffreestanding

GCC aims towards being usable as a conforming freestanding
implementation, or as the compiler for a conforming hosted
implementation.  By default, it acts as the compiler for a hosted
implementation, defining ``__STDC_HOSTED__`` as ``1`` and
presuming that when the names of ISO C functions are used, they have
the semantics defined in the standard.  To make it act as a conforming
freestanding implementation for a freestanding environment, use the
option :option:`-ffreestanding` ; it then defines
``__STDC_HOSTED__`` to ``0`` and does not make assumptions about the
meanings of function names from the standard library, with exceptions
noted below.  To build an OS kernel, you may well still need to make
your own arrangements for linking and startup.
See :ref:`c-dialect-options`.

GCC does not provide the library facilities required only of hosted
implementations, nor yet all the facilities required by C99 of
freestanding implementations on all platforms.
To use the facilities of a hosted
environment, you need to find them elsewhere (for example, in the
GNU C library).  See :ref:`standard-libraries`.

Most of the compiler support routines used by GCC are present in
:samp:`libgcc`, but there are a few exceptions.  GCC requires the
freestanding environment provide ``memcpy``, ``memmove``,
``memset`` and ``memcmp``.
Finally, if ``__builtin_trap`` is used, and the target does
not implement the ``trap`` pattern, then GCC emits a call
to ``abort``.

For references to Technical Corrigenda, Rationale documents and
information concerning the history of C that is available online, see
https://gcc.gnu.org/readings.html
