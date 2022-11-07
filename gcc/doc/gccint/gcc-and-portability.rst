..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: portability, GCC and portability

.. _portability:

GCC and Portability
-------------------

GCC itself aims to be portable to any machine where ``int`` is at least
a 32-bit type.  It aims to target machines with a flat (non-segmented) byte
addressed data address space (the code address space can be separate).
Target ABIs may have 8, 16, 32 or 64-bit ``int`` type.  ``char``
can be wider than 8 bits.

GCC gets most of the information about the target machine from a machine
description which gives an algebraic formula for each of the machine's
instructions.  This is a very clean way to describe the target.  But when
the compiler needs information that is difficult to express in this
fashion, ad-hoc parameters have been defined for machine descriptions.
The purpose of portability is to reduce the total work needed on the
compiler; it was not of interest for its own sake.

.. index:: endianness, autoincrement addressing, availability, abort

GCC does not contain machine dependent code, but it does contain code
that depends on machine parameters such as endianness (whether the most
significant byte has the highest or lowest address of the bytes in a word)
and the availability of autoincrement addressing.  In the RTL-generation
pass, it is often necessary to have multiple strategies for generating code
for a particular kind of syntax tree, strategies that are usable for different
combinations of parameters.  Often, not all possible cases have been
addressed, but only the common ones or only the ones that have been
encountered.  As a result, a new target may require additional
strategies.  You will know
if this happens because the compiler will call ``abort``.  Fortunately,
the new strategies can be added in a machine-independent fashion, and will
affect only the target machines that need them.