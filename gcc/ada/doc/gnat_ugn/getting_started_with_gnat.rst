.. role:: switch(samp)

.. _Getting_Started_with_GNAT:

*************************
Getting Started with GNAT
*************************

This chapter describes how to use GNAT's command line interface to build
executable Ada programs.
On most platforms a visually oriented Integrated Development Environment
is also available, the GNAT Programming Studio (GNAT Studio).
GNAT Studio offers a graphical "look and feel", support for development in
other programming languages, comprehensive browsing features, and
many other capabilities.
For information on GNAT Studio please refer to
:title:`Using the GNAT Programming Studio`.


.. _Running_GNAT:

Running GNAT
============

Three steps are needed to create an executable file from an Ada source
file:

*   The source file(s) must be compiled.
*   The file(s) must be bound using the GNAT binder.
*   All appropriate object files must be linked to produce an executable.

All three steps are most commonly handled by using the ``gnatmake``
utility program that, given the name of the main program, automatically
performs the necessary compilation, binding and linking steps.

.. _Running_a_Simple_Ada_Program:

Running a Simple Ada Program
============================

Any text editor may be used to prepare an Ada program.
(If Emacs is used, the optional Ada mode may be helpful in laying out the
program.)
The program text is a normal text file. We will assume in our initial
example that you have used your editor to prepare the following
standard format text file:


.. code-block:: ada

  with Ada.Text_IO; use Ada.Text_IO;
  procedure Hello is
  begin
     Put_Line ("Hello WORLD!");
  end Hello;

This file should be named :file:`hello.adb`.
With the normal default file naming conventions, GNAT requires
that each file
contain a single compilation unit whose file name is the
unit name,
with periods replaced by hyphens; the
extension is :file:`ads` for a
spec and :file:`adb` for a body.
You can override this default file naming convention by use of the
special pragma ``Source_File_Name`` (for further information please
see :ref:`Using_Other_File_Names`).
Alternatively, if you want to rename your files according to this default
convention, which is probably more convenient if you will be using GNAT
for all your compilations, then the ``gnatchop`` utility
can be used to generate correctly-named source files
(see :ref:`Renaming_Files_with_gnatchop`).

You can compile the program using the following command (``$`` is used
as the command prompt in the examples in this document):

.. code-block:: sh

  $ gcc -c hello.adb


``gcc`` is the command used to run the compiler. This compiler is
capable of compiling programs in several languages, including Ada and
C. It assumes that you have given it an Ada program if the file extension is
either :file:`.ads` or :file:`.adb`, and it will then call
the GNAT compiler to compile the specified file.

The :switch:`-c` switch is required. It tells ``gcc`` to only do a
compilation. (For C programs, ``gcc`` can also do linking, but this
capability is not used directly for Ada programs, so the :switch:`-c`
switch must always be present.)

This compile command generates a file
:file:`hello.o`, which is the object
file corresponding to your Ada program. It also generates
an 'Ada Library Information' file :file:`hello.ali`,
which contains additional information used to check
that an Ada program is consistent.
To build an executable file,
use ``gnatbind`` to bind the program
and ``gnatlink`` to link it. The
argument to both ``gnatbind`` and ``gnatlink`` is the name of the
:file:`ALI` file, but the default extension of :file:`.ali` can
be omitted. This means that in the most common case, the argument
is simply the name of the main program:

.. code-block:: sh

  $ gnatbind hello
  $ gnatlink hello

A simpler method of carrying out these steps is to use ``gnatmake``,
a master program that invokes all the required
compilation, binding and linking tools in the correct order. In particular,
``gnatmake`` automatically recompiles any sources that have been
modified since they were last compiled, or sources that depend
on such modified sources, so that 'version skew' is avoided.

.. index:: Version skew (avoided by ``gnatmake``)

.. code-block:: sh

  $ gnatmake hello.adb

The result is an executable program called :file:`hello`, which can be
run by entering:

.. code-block:: sh

  $ hello

assuming that the current directory is on the search path
for executable programs.

and, if all has gone well, you will see::

  Hello WORLD!

appear in response to this command.

.. _Running_a_Program_with_Multiple_Units:

Running a Program with Multiple Units
=====================================

Consider a slightly more complicated example that has three files: a
main program, and the spec and body of a package:


.. code-block:: ada

  package Greetings is
     procedure Hello;
     procedure Goodbye;
  end Greetings;

  with Ada.Text_IO; use Ada.Text_IO;
  package body Greetings is
     procedure Hello is
     begin
        Put_Line ("Hello WORLD!");
     end Hello;

     procedure Goodbye is
     begin
        Put_Line ("Goodbye WORLD!");
     end Goodbye;
  end Greetings;

  with Greetings;
  procedure Gmain is
  begin
     Greetings.Hello;
     Greetings.Goodbye;
  end Gmain;

Following the one-unit-per-file rule, place this program in the
following three separate files:



*greetings.ads*
  spec of package ``Greetings``


*greetings.adb*
  body of package ``Greetings``


*gmain.adb*
  body of main program

To build an executable version of
this program, we could use four separate steps to compile, bind, and link
the program, as follows:

.. code-block:: sh

  $ gcc -c gmain.adb
  $ gcc -c greetings.adb
  $ gnatbind gmain
  $ gnatlink gmain

Note that there is no required order of compilation when using GNAT.
In particular it is perfectly fine to compile the main program first.
Also, it is not necessary to compile package specs in the case where
there is an accompanying body; you only need to compile the body. If you want
to submit these files to the compiler for semantic checking and not code
generation, then use the :switch:`-gnatc` switch:

.. code-block:: sh

  $ gcc -c greetings.ads -gnatc

Although the compilation can be done in separate steps as in the
above example, in practice it is almost always more convenient
to use the ``gnatmake`` tool. All you need to know in this case
is the name of the main program's source file. The effect of the above four
commands can be achieved with a single one:

.. code-block:: sh

  $ gnatmake gmain.adb

In the next section we discuss the advantages of using ``gnatmake`` in
more detail.

.. _Using_the_gnatmake_Utility:

Using the ``gnatmake`` Utility
==============================

If you work on a program by compiling single components at a time using
``gcc``, you typically keep track of the units you modify. In order to
build a consistent system, you compile not only these units, but also any
units that depend on the units you have modified.
For example, in the preceding case,
if you edit :file:`gmain.adb`, you only need to recompile that file. But if
you edit :file:`greetings.ads`, you must recompile both
:file:`greetings.adb` and :file:`gmain.adb`, because both files contain
units that depend on :file:`greetings.ads`.

``gnatbind`` will warn you if you forget one of these compilation
steps, so that it is impossible to generate an inconsistent program as a
result of forgetting to do a compilation. Nevertheless it is tedious and
error-prone to keep track of dependencies among units.
One approach to handle the dependency-bookkeeping is to use a
makefile. However, makefiles present maintenance problems of their own:
if the dependencies change as you change the program, you must make
sure that the makefile is kept up-to-date manually, which is also an
error-prone process.

The ``gnatmake`` utility takes care of these details automatically.
Invoke it using either one of the following forms:

.. code-block:: sh

  $ gnatmake gmain.adb
  $ gnatmake gmain

The argument is the name of the file containing the main program;
you may omit the extension. ``gnatmake``
examines the environment, automatically recompiles any files that need
recompiling, and binds and links the resulting set of object files,
generating the executable file, :file:`gmain`.
In a large program, it
can be extremely helpful to use ``gnatmake``, because working out by hand
what needs to be recompiled can be difficult.

Note that ``gnatmake`` takes into account all the Ada rules that
establish dependencies among units. These include dependencies that result
from inlining subprogram bodies, and from
generic instantiation. Unlike some other
Ada make tools, ``gnatmake`` does not rely on the dependencies that were
found by the compiler on a previous compilation, which may possibly
be wrong when sources change. ``gnatmake`` determines the exact set of
dependencies from scratch each time it is run.
