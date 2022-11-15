..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GNU Compiler Collection, GCC

.. _gnu-fortran-and-gcc:

GNU Fortran and GCC
*******************

GNU Fortran is a part of GCC, the :dfn:`GNU Compiler Collection`.  GCC
consists of a collection of front ends for various languages, which
translate the source code into a language-independent form called
:dfn:`GENERIC`.  This is then processed by a common middle end which
provides optimization, and then passed to one of a collection of back
ends which generate code for different computer architectures and
operating systems.

Functionally, this is implemented with a driver program (:command:`gcc`)
which provides the command-line interface for the compiler.  It calls
the relevant compiler front-end program (e.g., :command:`f951` for
Fortran) for each file in the source code, and then calls the assembler
and linker as appropriate to produce the compiled output.  In a copy of
GCC that has been compiled with Fortran language support enabled,
:command:`gcc` recognizes files with :samp:`.f`, :samp:`.for`, :samp:`.ftn`,
:samp:`.f90`, :samp:`.f95`, :samp:`.f03` and :samp:`.f08` extensions as
Fortran source code, and compiles it accordingly.  A :command:`gfortran`
driver program is also provided, which is identical to :command:`gcc`
except that it automatically links the Fortran runtime libraries into the
compiled program.

Source files with :samp:`.f`, :samp:`.for`, :samp:`.fpp`, :samp:`.ftn`, :samp:`.F`,
:samp:`.FOR`, :samp:`.FPP`, and :samp:`.FTN` extensions are treated as fixed form.
Source files with :samp:`.f90`, :samp:`.f95`, :samp:`.f03`, :samp:`.f08`,
:samp:`.F90`, :samp:`.F95`, :samp:`.F03` and :samp:`.F08` extensions are
treated as free form.  The capitalized versions of either form are run
through preprocessing.  Source files with the lower case :samp:`.fpp`
extension are also run through preprocessing.

This manual specifically documents the Fortran front end, which handles
the programming language's syntax and semantics.  The aspects of GCC
which relate to the optimization passes and the back-end code generation
that relate to the optimization passes and the back-end code generation
are documented in the GCC manual; see :ref:`gcc:top`.
The two manuals together provide a complete reference for the GNU
Fortran compiler.
