.. _About_This_Guide:

About This Guide
~~~~~~~~~~~~~~~~

.. only:: PRO

   For ease of exposition, 'GNAT Pro' will be referred to simply as
   'GNAT' in the remainder of this document.

This guide describes the use of GNAT,
a compiler and software development
toolset for the full Ada programming language.
It documents the features of the compiler and tools, and explains
how to use them to build Ada applications.

GNAT implements Ada 95, Ada 2005, Ada 2012, and Ada 202x, and it may also be
invoked in Ada 83 compatibility mode.
By default, GNAT assumes Ada 2012, but you can override with a
compiler switch (:ref:`Compiling_Different_Versions_of_Ada`)
to explicitly specify the language version.
Throughout this manual, references to 'Ada' without a year suffix
apply to all Ada versions of the language, starting with Ada 95.

What This Guide Contains
========================

This guide contains the following chapters:

* :ref:`Getting_Started_with_GNAT` describes how to get started compiling
  and running Ada programs with the GNAT Ada programming environment.

* :ref:`The_GNAT_Compilation_Model` describes the compilation model used
  by GNAT.

* :ref:`Building_Executable_Programs_With_GNAT` describes how to use the
  main GNAT tools to build executable programs, and it also gives examples of
  using the GNU make utility with GNAT.

* :ref:`GNAT_Utility_Programs` explains the various utility programs that
  are included in the GNAT environment.

* :ref:`GNAT_and_Program_Execution` covers a number of topics related to
  running, debugging, and tuning the performance of programs developed
  with GNAT.

Appendices cover several additional topics:

* :ref:`Platform_Specific_Information` describes the different run-time
  library implementations and also presents information on how to use
  GNAT on several specific platforms.

* :ref:`Example_of_Binder_Output_File` shows the source code for the binder
  output file for a sample program.

* :ref:`Elaboration_Order_Handling_in_GNAT` describes how GNAT helps
  you deal with elaboration order issues.

* :ref:`Inline_Assembler` shows how to use the inline assembly facility
  in an Ada program.



What You Should Know before Reading This Guide
==============================================

.. index:: Ada 95 Language Reference Manual

.. index:: Ada 2005 Language Reference Manual

This guide assumes a basic familiarity with the Ada 95 language, as
described in the International Standard ANSI/ISO/IEC-8652:1995, January
1995.
Reference manuals for Ada 95, Ada 2005, and Ada 2012 are included in
the GNAT documentation package.


Related Information
===================

For further information about Ada and related tools, please refer to the
following documents:

* :title:`Ada 95 Reference Manual`, :title:`Ada 2005 Reference Manual`, and
  :title:`Ada 2012 Reference Manual`, which contain reference
  material for the several revisions of the Ada language standard.

* :title:`GNAT Reference_Manual`, which contains all reference material for the GNAT
  implementation of Ada.

* :title:`Using GNAT Studio`, which describes the GNAT Studio
  Integrated Development Environment.

* :title:`GNAT Studio Tutorial`, which introduces the
  main GNAT Studio features through examples.

* :title:`Debugging with GDB`,
  for all details on the use of the GNU source-level debugger.

* :title:`GNU Emacs Manual`,
  for full information on the extensible editor and programming
  environment Emacs.


Conventions
===========
.. index:: Conventions, typographical

.. index:: Typographical conventions

Following are examples of the typographical and graphic conventions used
in this guide:

* ``Functions``, ``utility program names``, ``standard names``,
  and ``classes``.

* ``Option flags``

* :file:`File names`

* ``Variables``

* *Emphasis*

* [optional information or parameters]

* Examples are described by text

  ::

    and then shown this way.

* Commands that are entered by the user are shown as preceded by a prompt string
  comprising the ``$`` character followed by a space.

* Full file names are shown with the '/' character
  as the directory separator; e.g., :file:`parent-dir/subdir/myfile.adb`.
  If you are using GNAT on a Windows platform, please note that
  the '\\' character should be used instead.
