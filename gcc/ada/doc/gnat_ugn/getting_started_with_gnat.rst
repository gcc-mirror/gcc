.. role:: switch(samp)

.. _Getting_Started_with_GNAT:

*************************
Getting Started with GNAT
*************************

This chapter describes how to use GNAT's command line interface to build
executable Ada programs.
On most platforms a visually oriented Integrated Development Environment
is also available: GNAT Studio.
GNAT Studio offers a graphical "look and feel", support for development in
other programming languages, comprehensive browsing features, and
many other capabilities.
For information on GNAT Studio please refer to the
:title:`GNAT Studio documentation`.


.. _System_Requirements:

System Requirements
===================

Even though any machine can run the GNAT toolset and GNAT Studio IDE, 
to get the best experience we recommend using a machine with as many cores
as possible, allowing individual compilations to run in parallel.
A comfortable setup for a compiler server is a machine with 24 physical cores
or more, with at least 48 GB of memory (2 GB per core).

For a desktop machine, we recommend a minimum of 4 cores (8 is preferred),
with at least 2GB per core (so 8 to 16GB).

In addition, for running and smoothly navigating sources in GNAT Studio, we
recommend at least 1.5 GB, plus 3 GB of RAM per million source lines of code.
So we recommend at least 3 GB for 500K lines of code and
7.5 GB for 2 million lines of code.

Using fast, local drives can make a significant difference in build
and link times. You should avoid network drives such as NFS, SMB, or
worse, configuration management filesystems (such as ClearCase dynamic
views) as much as possible since these will produce very degraded
performance (typically 2 to 3 times slower than on fast, local
drives). If you cannot avoid using such slow drives for accessing
source code, you should at least configure your project file so
the result of the compilation is stored on a drive local to the
machine performing the compilation. You can do this by setting the
``Object_Dir`` project file attribute.

.. _Running_GNAT:

Running GNAT
============

You need to take three steps to create an executable file from an Ada
source file:

*   You must compile the source file(s).
*   You must bind the file(s) using the GNAT binder.
*   You must link all appropriate object files to produce an executable.

You most commonly perform all three steps by using the ``gnatmake``
utility program.  You pass it the name of the main program and it automatically
performs the necessary compilation, binding, and linking steps.

.. _Running_a_Simple_Ada_Program:

Running a Simple Ada Program
============================

You may use any text editor to prepare an Ada program.
(If you use Emacs, an optional Ada mode may be helpful in laying out the
program.)
The program text is a normal text file. We will assume in our initial
example that you have used your editor to prepare the following
standard format text file named :file:`hello.adb`:


.. code-block:: ada

  with Ada.Text_IO; use Ada.Text_IO;
  procedure Hello is
  begin
     Put_Line ("Hello WORLD!");
  end Hello;

With the normal default file naming conventions, GNAT requires
that each file
contain a single compilation unit whose file name is the
unit name with periods replaced by hyphens; the
extension is :file:`ads` for a
spec and :file:`adb` for a body.
You can override this default file naming convention by use of the
special pragma ``Source_File_Name`` (see :ref:`Using_Other_File_Names`).
Alternatively, if you want to rename your files according to this default
convention, which is probably more convenient if you will be using GNAT
for all your compilations, then you use can use the ``gnatchop`` utility
to generate correctly-named source files
(see :ref:`Renaming_Files_with_gnatchop`).

You can compile the program using the following command (``$`` is used
as the command prompt in the examples in this document):

.. code-block:: sh

  $ gcc -c hello.adb


``gcc`` is the command used to run the compiler. It is
capable of compiling programs in several languages, including Ada and
C. It assumes you have given it an Ada program if the file extension is
either :file:`.ads` or :file:`.adb`, in which case it will call
the GNAT compiler to compile the specified file.

The :switch:`-c` switch is required. It tells ``gcc`` to only do a
compilation. (For C programs, ``gcc`` can also do linking, but this
capability is not used directly for Ada programs, so you must always
specify the :switch:`-c`.)

This compile command generates a file
:file:`hello.o`, which is the object
file corresponding to your Ada program. It also generates
an 'Ada Library Information' file :file:`hello.ali`,
which contains additional information used to check
that an Ada program is consistent.

To build an executable file, use either ``gnatmake`` or ``gprbuild`` with
the name of the main file: these tools are builders that perform
all the necessary build steps in the correct order.
In particular, these builders automatically recompile any sources that have
been modified since they were last compiled, as well as sources that depend
on such modified sources, so that 'version skew' is avoided.

.. index:: Version skew (avoided by ``gnatmake``)

.. code-block:: sh

  $ gnatmake hello.adb

The result is an executable program called :file:`hello`, which you can
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

Consider a slightly more complicated example with three files: a
main program and the spec and body of a package:


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

Note that there is no required order of compilation when using GNAT.
In particular it is perfectly fine to compile the main program first.
Also, it is not necessary to compile package specs in the case where
there is an accompanying body; you only need compile the body. If you want
to submit these files to the compiler for semantic checking and not code
generation, use the :switch:`-gnatc` switch:

.. code-block:: sh

  $ gcc -c greetings.ads -gnatc

Although you can do the compilation in separate steps, in practice it's
almost always more convenient to use the ``gnatmake`` or ``gprbuild`` tools:

.. code-block:: sh

  $ gnatmake gmain.adb

