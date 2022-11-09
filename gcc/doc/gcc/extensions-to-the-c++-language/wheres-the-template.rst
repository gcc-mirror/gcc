..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: template instantiation

.. _template-instantiation:

Where's the Template?
*********************

C++ templates were the first language feature to require more
intelligence from the environment than was traditionally found on a UNIX
system.  Somehow the compiler and linker have to make sure that each
template instance occurs exactly once in the executable if it is needed,
and not at all otherwise.  There are two basic approaches to this
problem, which are referred to as the Borland model and the Cfront model.

Borland model
  Borland C++ solved the template instantiation problem by adding the code
  equivalent of common blocks to their linker; the compiler emits template
  instances in each translation unit that uses them, and the linker
  collapses them together.  The advantage of this model is that the linker
  only has to consider the object files themselves; there is no external
  complexity to worry about.  The disadvantage is that compilation time
  is increased because the template code is being compiled repeatedly.
  Code written for this model tends to include definitions of all
  templates in the header file, since they must be seen to be
  instantiated.

Cfront model
  The AT&T C++ translator, Cfront, solved the template instantiation
  problem by creating the notion of a template repository, an
  automatically maintained place where template instances are stored.  A
  more modern version of the repository works as follows: As individual
  object files are built, the compiler places any template definitions and
  instantiations encountered in the repository.  At link time, the link
  wrapper adds in the objects in the repository and compiles any needed
  instances that were not previously emitted.  The advantages of this
  model are more optimal compilation speed and the ability to use the
  system linker; to implement the Borland model a compiler vendor also
  needs to replace the linker.  The disadvantages are vastly increased
  complexity, and thus potential for error; for some code this can be
  just as transparent, but in practice it can been very difficult to build
  multiple programs in one directory and one program in multiple
  directories.  Code written for this model tends to separate definitions
  of non-inline member templates into a separate file, which should be
  compiled separately.

G++ implements the Borland model on targets where the linker supports it,
including ELF targets (such as GNU/Linux), Mac OS X and Microsoft Windows.
Otherwise G++ implements neither automatic model.

You have the following options for dealing with template instantiations:

* Do nothing.  Code written for the Borland model works fine, but
  each translation unit contains instances of each of the templates it
  uses.  The duplicate instances will be discarded by the linker, but in
  a large program, this can lead to an unacceptable amount of code
  duplication in object files or shared libraries.

  Duplicate instances of a template can be avoided by defining an explicit
  instantiation in one object file, and preventing the compiler from doing
  implicit instantiations in any other object files by using an explicit
  instantiation declaration, using the ``extern template`` syntax:

  .. code-block:: c++

    extern template int max (int, int);

  This syntax is defined in the C++ 2011 standard, but has been supported by
  G++ and other compilers since well before 2011.

  Explicit instantiations can be used for the largest or most frequently
  duplicated instances, without having to know exactly which other instances
  are used in the rest of the program.  You can scatter the explicit
  instantiations throughout your program, perhaps putting them in the
  translation units where the instances are used or the translation units
  that define the templates themselves; you can put all of the explicit
  instantiations you need into one big file; or you can create small files
  like

  .. code-block:: c++

    #include "Foo.h"
    #include "Foo.cc"

    template class Foo<int>;
    template ostream& operator <<
                    (ostream&, const Foo<int>&);

  for each of the instances you need, and create a template instantiation
  library from those.

  This is the simplest option, but also offers flexibility and
  fine-grained control when necessary. It is also the most portable
  alternative and programs using this approach will work with most modern
  compilers.

*
  .. index:: fno-implicit-templates

  Compile your code with :option:`-fno-implicit-templates` to disable the
  implicit generation of template instances, and explicitly instantiate
  all the ones you use.  This approach requires more knowledge of exactly
  which instances you need than do the others, but it's less
  mysterious and allows greater control if you want to ensure that only
  the intended instances are used.

  If you are using Cfront-model code, you can probably get away with not
  using :option:`-fno-implicit-templates` when compiling files that don't
  :samp:`#include` the member template definitions.

  If you use one big file to do the instantiations, you may want to
  compile it without :option:`-fno-implicit-templates` so you get all of the
  instances required by your explicit instantiations (but not by any
  other files) without having to specify them as well.

  In addition to forward declaration of explicit instantiations
  (with ``extern``), G++ has extended the template instantiation
  syntax to support instantiation of the compiler support data for a
  template class (i.e. the vtable) without instantiating any of its
  members (with ``inline``), and instantiation of only the static data
  members of a template class, without the support data or member
  functions (with ``static``):

  .. code-block:: c++

    inline template class Foo<int>;
    static template class Foo<int>;
