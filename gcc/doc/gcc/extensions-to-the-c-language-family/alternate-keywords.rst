..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: alternate keywords, keywords, alternate

.. _alternate-keywords:

Alternate Keywords
******************

:option:`-ansi` and the various :option:`-std` options disable certain
keywords.  This causes trouble when you want to use GNU C extensions, or
a general-purpose header file that should be usable by all programs,
including ISO C programs.  The keywords ``asm``, ``typeof`` and
``inline`` are not available in programs compiled with
:option:`-ansi` or :option:`-std` (although ``inline`` can be used in a
program compiled with :option:`-std=c99` or a later standard).  The
ISO C99 keyword
``restrict`` is only available when :option:`-std=gnu99` (which will
eventually be the default) or :option:`-std=c99` (or the equivalent
:option:`-std=iso9899:1999`), or an option for a later standard
version, is used.

The way to solve these problems is to put :samp:`__` at the beginning and
end of each problematical keyword.  For example, use ``__asm__``
instead of ``asm``, and ``__inline__`` instead of ``inline``.

Other C compilers won't accept these alternative keywords; if you want to
compile with another compiler, you can define the alternate keywords as
macros to replace them with the customary keywords.  It looks like this:

.. code-block:: c++

  #ifndef __GNUC__
  #define __asm__ asm
  #endif

.. index:: __extension__, pedantic

:option:`-pedantic` and other options cause warnings for many GNU C extensions.
You can
prevent such warnings within one expression by writing
``__extension__`` before the expression.  ``__extension__`` has no
effect aside from this.