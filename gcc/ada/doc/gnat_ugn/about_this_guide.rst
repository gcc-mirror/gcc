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

GNAT implements Ada 95, Ada 2005 and Ada 2012, and it may also be
invoked in Ada 83 compatibility mode.
By default, GNAT assumes Ada 2012, but you can override with a
compiler switch (:ref:`Compiling_Different_Versions_of_Ada`)
to explicitly specify the language version.
Throughout this manual, references to 'Ada' without a year suffix
apply to all Ada 95/2005/2012 versions of the language.

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
  are included in the GNAT environment

* :ref:`GNAT_and_Program_Execution` covers a number of topics related to
  running, debugging, and tuning the performace of programs developed
  with GNAT

Appendices cover several additional topics:

* :ref:`Platform_Specific_Information` describes the different run-time
  library implementations and also presents information on how to use
  GNAT on several specific platforms

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
It does not require knowledge of the features introduced by Ada 2005
or Ada 2012.
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

* :title:`Using the GNAT Programming Studio`, which describes the GPS
  Integrated Development Environment.

* :title:`GNAT Programming Studio Tutorial`, which introduces the
  main GPS features through examples.

* :title:`Debugging with GDB`,
  for all details on the use of the GNU source-level debugger.

* :title:`GNU Emacs Manual`,
  for full information on the extensible editor and programming
  environment Emacs.


A Note to Readers of Previous Versions of the Manual
====================================================

In early 2015 the GNAT manuals were transitioned to the
reStructuredText (rst) / Sphinx documentation generator technology.
During that process the :title:`GNAT User's Guide` was reorganized
so that related topics would be described together in the same chapter
or appendix.  Here's a summary of the major changes realized in
the new document structure.

* :ref:`The_GNAT_Compilation_Model` has been extended so that it now covers
  the following material:

  - The `gnatname`, `gnatkr`, and `gnatchop` tools
  - :ref:`Configuration_Pragmas`
  - :ref:`GNAT_and_Libraries`
  - :ref:`Conditional_Compilation` including :ref:`Preprocessing_with_gnatprep`
    and :ref:`Integrated_Preprocessing`
  - :ref:`Generating_Ada_Bindings_for_C_and_C++_headers`
  - :ref:`Using_GNAT_Files_with_External_Tools`

* :ref:`Building_Executable_Programs_With_GNAT` is a new chapter consolidating
  the following content:

  - :ref:`The_GNAT_Make_Program_gnatmake`
  - :ref:`Compiling_with_GCC`
  - :ref:`Binding_with_gnatbind`
  - :ref:`Linking_with_gnatlink`
  - :ref:`Using_the_GNU_make_Utility`

* :ref:`GNAT_Utility_Programs` is a new chapter consolidating the information about several
  GNAT tools:

  .. only:: PRO or GPL

    - :ref:`The_File_Cleanup_Utility_gnatclean`
    - :ref:`The_GNAT_Library_Browser_gnatls`
    - :ref:`The_Cross-Referencing_Tools_gnatxref_and_gnatfind`
    - :ref:`The_Ada_to_HTML_Converter_gnathtml`
    - :ref:`The_Ada-to-XML_Converter_gnat2xml`
    - :ref:`The_Program_Property_Verifier_gnatcheck`
    - :ref:`The_GNAT_Metrics_Tool_gnatmetric`
    - :ref:`The_GNAT_Pretty-Printer_gnatpp`
    - :ref:`The_Body_Stub_Generator_gnatstub`
    - :ref:`The_Unit_Test_Generator_gnattest`

  .. only:: FSF

    - :ref:`The_File_Cleanup_Utility_gnatclean`
    - :ref:`The_GNAT_Library_Browser_gnatls`
    - :ref:`The_Cross-Referencing_Tools_gnatxref_and_gnatfind`
    - :ref:`The_Ada_to_HTML_Converter_gnathtml`

* :ref:`GNAT_and_Program_Execution` is a new chapter consolidating the following:

  - :ref:`Running_and_Debugging_Ada_Programs`
  - :ref:`Code_Coverage_and_Profiling`
  - :ref:`Improving_Performance`
  - :ref:`Overflow Check Handling in GNAT <Overflow_Check_Handling_in_GNAT>`
  - :ref:`Performing Dimensionality Analysis in GNAT <Performing_Dimensionality_Analysis_in_GNAT>`
  - :ref:`Stack_Related_Facilities`
  - :ref:`Memory_Management_Issues`

* :ref:`Platform_Specific_Information` is a new appendix consolidating the following:

  - :ref:`Run_Time_Libraries`
  - :ref:`Microsoft_Windows_Topics`
  - :ref:`Mac_OS_Topics`

* The `Compatibility and Porting Guide` appendix has been moved to the
  :title:`GNAT Reference Manual`. It now includes a section
  `Writing Portable Fixed-Point Declarations` which was previously
  a separate chapter in the :title:`GNAT User's Guide`.


Conventions
===========
.. index:: Conventions, typographical

.. index:: Typographical conventions

Following are examples of the typographical and graphic conventions used
in this guide:

* `Functions`, `utility program names`, `standard names`,
  and `classes`.

* `Option flags`

* :file:`File names`

* `Variables`

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
