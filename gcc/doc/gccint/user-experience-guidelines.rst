..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: user experience guidelines, guidelines, user experience

.. _user-experience-guidelines:

User Experience Guidelines
--------------------------

To borrow a slogan from
`Elm <https://elm-lang.org/news/compilers-as-assistants>`_,

**Compilers should be assistants, not adversaries.**  A compiler should
not just detect bugs, it should then help you understand why there is a bug.
It should not berate you in a robot voice, it should give you specific hints
that help you write better code. Ultimately, a compiler should make
programming faster and more fun!
Evan Czaplicki

This chapter provides guidelines on how to implement diagnostics and
command-line options in ways that we hope achieve the above ideal.

.. toctree::
  :maxdepth: 2

  guidelines-for-diagnostics
  guidelines-for-options
