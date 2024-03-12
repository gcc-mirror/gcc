.. role:: switch(samp)

.. |rightarrow| unicode:: 0x2192

.. _GNAT_Utility_Programs:

*********************
GNAT Utility Programs
*********************

This chapter describes a number of utility programs:

.. only:: PRO or GPL

  * :ref:`The_File_Cleanup_Utility_gnatclean`
  * :ref:`The_GNAT_Library_Browser_gnatls`
  * :ref:`The_GNAT_Pretty_Printer_gnatpp`
  * :ref:`The_Body_Stub_Generator_gnatstub`
  * :ref:`The_Backtrace_Symbolizer_gnatsymbolize`

  It also describes how several of these tools can be used in conjunction
  with project files: :ref:`Using_Project_Files_with_GNAT_Tools`

.. only:: FSF

  * :ref:`The_File_Cleanup_Utility_gnatclean`
  * :ref:`The_GNAT_Library_Browser_gnatls`

Other GNAT utilities are described elsewhere in this manual:

* :ref:`Handling_Arbitrary_File_Naming_Conventions_with_gnatname`
* :ref:`File_Name_Krunching_with_gnatkr`
* :ref:`Renaming_Files_with_gnatchop`
* :ref:`Preprocessing_with_gnatprep`


.. _The_File_Cleanup_Utility_gnatclean:

The File Cleanup Utility ``gnatclean``
======================================

.. index:: File cleanup tool
.. index:: gnatclean

``gnatclean`` is a tool that allows the deletion of files produced by the
compiler, binder and linker, including ALI files, object files, tree files,
expanded source files, library files, interface copy source files, binder
generated files and executable files.

.. _Running_gnatclean:

Running ``gnatclean``
---------------------

The ``gnatclean`` command has the form:

  ::

      $ gnatclean switches names

where ``names`` is a list of source file names. Suffixes :file:`.ads` and
:file:`adb` may be omitted. If a project file is specified using switch
:switch:`-P`, then ``names`` may be completely omitted.

In normal mode, ``gnatclean`` delete the files produced by the compiler and,
if switch :switch:`-c` is not specified, by the binder and
the linker. In informative-only mode, specified by switch
:switch:`-n`, the list of files that would have been deleted in
normal mode is listed, but no file is actually deleted.


.. _Switches_for_gnatclean:

Switches for ``gnatclean``
--------------------------

``gnatclean`` recognizes the following switches:

.. index:: --version (gnatclean)

:switch:`--version`
  Display copyright and version, then exit disregarding all other options.

.. index:: --help (gnatclean)

:switch:`--help`
  If :switch:`--version` was not used, display usage, then exit disregarding
  all other options.

:switch:`--subdirs={subdir}`
  Actual object directory of each project file is the subdirectory subdir of the
  object directory specified or defaulted in the project file.

:switch:`--unchecked-shared-lib-imports`
  By default, shared library projects are not allowed to import static library
  projects. When this switch is used on the command line, this restriction is
  relaxed.

.. index:: -c (gnatclean)

:switch:`-c`
  Only attempt to delete the files produced by the compiler, not those produced
  by the binder or the linker. The files that are not to be deleted are library
  files, interface copy files, binder generated files and executable files.

.. index:: -D (gnatclean)

:switch:`-D {dir}`
  Indicate that ALI and object files should normally be found in directory ``dir``.

.. index:: -F (gnatclean)

:switch:`-F`
  When using project files, if some errors or warnings are detected during
  parsing and verbose mode is not in effect (no use of switch
  -v), then error lines start with the full path name of the project
  file, rather than its simple file name.

.. index:: -h (gnatclean)

:switch:`-h`
  Output a message explaining the usage of ``gnatclean``.

.. index:: -n (gnatclean)

:switch:`-n`
  Informative-only mode. Do not delete any files. Output the list of the files
  that would have been deleted if this switch was not specified.

.. index:: -P (gnatclean)

:switch:`-P{project}`
  Use project file ``project``. Only one such switch can be used.
  When cleaning a project file, the files produced by the compilation of the
  immediate sources or inherited sources of the project files are to be
  deleted. This is not depending on the presence or not of executable names
  on the command line.

.. index:: -q (gnatclean)

:switch:`-q`
  Quiet output. If there are no errors, do not output anything, except in
  verbose mode (switch -v) or in informative-only mode
  (switch -n).

.. index:: -r (gnatclean)

:switch:`-r`
  When a project file is specified (using switch -P),
  clean all imported and extended project files, recursively. If this switch
  is not specified, only the files related to the main project file are to be
  deleted. This switch has no effect if no project file is specified.

.. index:: -v (gnatclean)

:switch:`-v`
  Verbose mode.

.. index:: -vP (gnatclean)

:switch:`-vP{x}`
  Indicates the verbosity of the parsing of GNAT project files.
  :ref:`Switches_Related_to_Project_Files`.

.. index:: -X (gnatclean)

:switch:`-X{name}={value}`
  Indicates that external variable ``name`` has the value ``value``.
  The Project Manager will use this value for occurrences of
  ``external(name)`` when parsing the project file.
  See :ref:`Switches_Related_to_Project_Files`.

.. index:: -aO (gnatclean)

:switch:`-aO{dir}`
  When searching for ALI and object files, look in directory ``dir``.

.. index:: -I (gnatclean)

:switch:`-I{dir}`
  Equivalent to :switch:`-aO{dir}`.

.. index:: -I- (gnatclean)

.. index:: Source files, suppressing search

:switch:`-I-`
  Do not look for ALI or object files in the directory
  where ``gnatclean`` was invoked.



.. _The_GNAT_Library_Browser_gnatls:

The GNAT Library Browser ``gnatls``
===================================

.. index:: Library browser
.. index:: ! gnatls

``gnatls`` is a tool that outputs information about compiled
units. It gives the relationship between objects, unit names and source
files. It can also be used to check the source dependencies of a unit
as well as various characteristics.

.. _Running_gnatls:

Running ``gnatls``
------------------

The ``gnatls`` command has the form

  ::

      $ gnatls switches object_or_ali_file

The main argument is the list of object or :file:`ali` files
(see :ref:`The_Ada_Library_Information_Files`)
for which information is requested.

In normal mode, without additional option, ``gnatls`` produces a
four-column listing. Each line represents information for a specific
object. The first column gives the full path of the object, the second
column gives the name of the principal unit in this object, the third
column gives the status of the source and the fourth column gives the
full path of the source representing this unit.
Here is a simple example of use:


  ::

     $ gnatls *.o
     ./demo1.o            demo1            DIF demo1.adb
     ./demo2.o            demo2             OK demo2.adb
     ./hello.o            h1                OK hello.adb
     ./instr-child.o      instr.child      MOK instr-child.adb
     ./instr.o            instr             OK instr.adb
     ./tef.o              tef              DIF tef.adb
     ./text_io_example.o  text_io_example   OK text_io_example.adb
     ./tgef.o             tgef             DIF tgef.adb

The first line can be interpreted as follows: the main unit which is
contained in
object file :file:`demo1.o` is demo1, whose main source is in
:file:`demo1.adb`. Furthermore, the version of the source used for the
compilation of demo1 has been modified (DIF). Each source file has a status
qualifier which can be:

*OK (unchanged)*
  The version of the source file used for the compilation of the
  specified unit corresponds exactly to the actual source file.

*MOK (slightly modified)*
  The version of the source file used for the compilation of the
  specified unit differs from the actual source file but not enough to
  require recompilation. If you use gnatmake with the option
  :switch:`-m` (minimal recompilation), a file marked
  MOK will not be recompiled.

*DIF (modified)*
  No version of the source found on the path corresponds to the source
  used to build this object.

*??? (file not found)*
  No source file was found for this unit.

*HID (hidden,  unchanged version not first on PATH)*
  The version of the source that corresponds exactly to the source used
  for compilation has been found on the path but it is hidden by another
  version of the same source that has been modified.


.. _Switches_for_gnatls:

Switches for ``gnatls``
-----------------------

``gnatls`` recognizes the following switches:


.. index:: --version (gnatls)

:switch:`--version`
  Display copyright and version, then exit disregarding all other options.


.. index:: --help (gnatls)

:switch:`--help`
  If :switch:`--version` was not used, display usage, then exit disregarding
  all other options.


.. index:: -a (gnatls)

:switch:`-a`
  Consider all units, including those of the predefined Ada library.
  Especially useful with :switch:`-d`.


.. index:: -d (gnatls)

:switch:`-d`
  List sources from which specified units depend on.


.. index:: -h (gnatls)

:switch:`-h`
  Output the list of options.


.. index:: -o (gnatls)

:switch:`-o`
  Only output information about object files.


.. index:: -s (gnatls)

:switch:`-s`
  Only output information about source files.


.. index:: -u (gnatls)

:switch:`-u`
  Only output information about compilation units.


.. index:: -files (gnatls)

:switch:`-files={file}`
  Take as arguments the files listed in text file ``file``.
  Text file ``file`` may contain empty lines that are ignored.
  Each nonempty line should contain the name of an existing file.
  Several such switches may be specified simultaneously.


.. index:: -aO (gnatls)

.. index:: -aI (gnatls)

.. index:: -I (gnatls)

.. index:: -I- (gnatls)

:switch:`-aO{dir}`, :switch:`-aI{dir}`, :switch:`-I{dir}`, :switch:`-I-`, :switch:`-nostdinc`
  Source path manipulation. Same meaning as the equivalent ``gnatmake``
  flags (:ref:`Switches_for_gnatmake`).


.. index:: -aP (gnatls)

:switch:`-aP{dir}`
  Add ``dir`` at the beginning of the project search dir.


.. index:: --RTS (gnatls)

:switch:`--RTS={rts-path}`
  Specifies the default location of the runtime library. Same meaning as the
  equivalent ``gnatmake`` flag (:ref:`Switches_for_gnatmake`).


.. index:: -v (gnatls)

:switch:`-v`
  Verbose mode. Output the complete source, object and project paths. Do not use
  the default column layout but instead use long format giving as much as
  information possible on each requested units, including special
  characteristics such as:

  * *Preelaborable*: The unit is preelaborable in the Ada sense.

  * *No_Elab_Code*:  No elaboration code has been produced by the compiler for this unit.

  * *Pure*: The unit is pure in the Ada sense.

  * *Elaborate_Body*: The unit contains a pragma Elaborate_Body.

  * *Remote_Types*: The unit contains a pragma Remote_Types.

  * *Shared_Passive*: The unit contains a pragma Shared_Passive.

  * *Predefined*: This unit is part of the predefined environment and cannot be modified
    by the user.

  * *Remote_Call_Interface*: The unit contains a pragma Remote_Call_Interface.


.. _Example_of_gnatls_Usage:

Example of ``gnatls`` Usage
---------------------------

Example of using the verbose switch. Note how the source and
object paths are affected by the -I switch.

  ::

      $ gnatls -v -I.. demo1.o

      GNATLS 5.03w (20041123-34)
      Copyright 1997-2004 Free Software Foundation, Inc.

      Source Search Path:
         <Current_Directory>
         ../
         /home/comar/local/adainclude/

      Object Search Path:
         <Current_Directory>
         ../
         /home/comar/local/lib/gcc-lib/x86-linux/3.4.3/adalib/

      Project Search Path:
         <Current_Directory>
         /home/comar/local/lib/gnat/

      ./demo1.o
         Unit =>
           Name   => demo1
           Kind   => subprogram body
           Flags  => No_Elab_Code
           Source => demo1.adb    modified

The following is an example of use of the dependency list.
Note the use of the -s switch
which gives a straight list of source files. This can be useful for
building specialized scripts.

  ::

      $ gnatls -d demo2.o
      ./demo2.o   demo2        OK demo2.adb
                               OK gen_list.ads
                               OK gen_list.adb
                               OK instr.ads
                               OK instr-child.ads

      $ gnatls -d -s -a demo1.o
      demo1.adb
      /home/comar/local/adainclude/ada.ads
      /home/comar/local/adainclude/a-finali.ads
      /home/comar/local/adainclude/a-filico.ads
      /home/comar/local/adainclude/a-stream.ads
      /home/comar/local/adainclude/a-tags.ads
      gen_list.ads
      gen_list.adb
      /home/comar/local/adainclude/gnat.ads
      /home/comar/local/adainclude/g-io.ads
      instr.ads
      /home/comar/local/adainclude/system.ads
      /home/comar/local/adainclude/s-exctab.ads
      /home/comar/local/adainclude/s-finimp.ads
      /home/comar/local/adainclude/s-finroo.ads
      /home/comar/local/adainclude/s-secsta.ads
      /home/comar/local/adainclude/s-stalib.ads
      /home/comar/local/adainclude/s-stoele.ads
      /home/comar/local/adainclude/s-stratt.ads
      /home/comar/local/adainclude/s-tasoli.ads
      /home/comar/local/adainclude/s-unstyp.ads
      /home/comar/local/adainclude/unchconv.ads


.. only:: PRO or GPL

   .. _The_GNAT_Pretty_Printer_gnatpp:

   The GNAT Pretty Printer ``gnatpp``
   ==================================

   .. index:: ! gnatpp
   .. index:: pretty printer

   The ``gnatpp`` tool is a utility for source reformatting / pretty
   printing. It takes an Ada source file as input and generates a
   reformatted version as output. You can specify various style
   directives via switches; e.g., identifier case conventions, rules of
   indentation, and comment layout.

   ``gnatpp`` is a project-aware tool
   (see :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
   the project-related switches). The project file package that can specify
   ``gnatpp`` switches is named ``Pretty_Printer``.

   ``gnatpp`` cannot process sources that contain preprocessing
   directives.

   The ``gnatpp`` command has the form

     ::

        $ gnatpp [ switches ] filename

   where

   * ``switches`` is an optional sequence of switches defining such properties
     as the formatting rules, the source search path, and the destination for
     the output source file

   * ``filename`` is the name of the source file to reformat; wildcards
     or several file names on the same gnatpp command are allowed. The
     file name may contain path information; it does not have to follow
     the GNAT file naming rules

   Note that it is no longer necessary to specify the Ada language version;
   ``gnatpp`` can process Ada source code written in any version from Ada 83
   onward without specifying any language version switch.


   .. _Switches_for_gnatpp:

   Switches for ``gnatpp``
   -----------------------

   The following subsections describe the various switches accepted by
   ``gnatpp``, organized by category.

   You specify a switch by supplying a name and generally also a value.
   In many cases the values for a switch with a given name are incompatible
   with each other
   (for example the switch that controls the casing of a reserved word may have
   exactly one value: upper case, lower case, or
   mixed case) and thus exactly one such switch can be in effect for an
   invocation of ``gnatpp``.
   If more than one is supplied, the last one is used.
   However, some values for the same switch are mutually compatible.
   You may supply several such switches to ``gnatpp``, but then
   each must be specified in full, with both the name and the value.
   Abbreviated forms (the name appearing once, followed by each value) are
   not permitted.


   .. _Layout_Control:

   Layout Control
   ^^^^^^^^^^^^^^

   .. index:: Layout control in gnatpp

   ``gnatpp`` provides a layout switch which controls the general
   formatting style:

   .. index:: layout(gnatpp)

   :switch:`--layout=default|minimal|compact|tall`

   :switch:`default`
     The default layout will follow a compact style but add aligment and put
     some keywords on a separate line.
     
     Alignment is added in the the following constructs:

     * ``:`` in declarations,
     * ``:=`` in initializations in declarations,
     * ``:=`` in assignment statements,
     * ``=>`` in associations, and
     * ``at`` keywords in the component clauses in record representation
       clauses.

     In addition, ``in`` and ``out`` keywords in parameter specifications are
     also lined up.

     The keyword ``is`` is placed on a separate line in a subprogram body in
     case the spec occupies more than one line.

     The keyword ``return`` is placed on a separate line if a subprogram spec
     does not fit on one line.

   :switch:`minimal`
     The minimal layout will avoid changing the source layout by keeping all
     line breaks from the original source (it will not insert or delete any).
     It will add indentation where appropriate as long as it does not exceed
     the line length limit.

   :switch:`compact`
     The compact layout will avoid adding line breaks and alignment by packing
     as many subexpressions on the same line as possible.

     Whole-line comments that form a paragraph will be filled in typical word
     processor style (that is, moving words between lines to make them similar
     in length, except the last one which may be shorter).

     For each whole-line comment that does not end with two hyphens, inserts
     spaces if necessary after the starting two hyphens to ensure that there
     are at least two spaces preceding the first non-blank character of the
     comment.

   :switch:`tall`
     The tall layout will favor adding lines breaks and alignment. It adds
     all the alignment and line breaks defined in the ``default`` option,
     and in addition:

     * Places the keyword ``loop`` in FOR and WHILE loop statements on a
       separate line
     * Places the keyword ``then`` in IF statements on a separate line
     * Places each keyword ``use`` in USE clauses on a separate line
     * Splits the line just before the keyword ``record`` in a RECORD type
       declaration
     * Indents named blocks and loop statments with respect to the name
     * When necessary, splits binary operators always before the operator
     * Inserts an extra blank before various occurrences of ``(`` and ``:``
     * When it is necessary to split a line between two subexpressions (because
       otherwise the construct would exceed --max-line-length), then all such
       subexpressions are placed on separate lines
     * Formats enumeration type declarations “vertically”, e.g. each
       enumeration literal goes on a separate line
     * Formats array type declarations “vertically”, e.g. for multidimensional
       arrays, each index_subtype_definition or discrete_subtype_definition
       goes on a separate line
     * Format aggregates “vertically” if named notation is used for all
       component_associations, e.g. each component_association goes on a
       separate line
     * Formats case statements, case expressions, and variant parts with
       additional line breaks
     * Inserts blank lines where appropriate (between bodies and other large
       constructs)
     * Similarly to the :switch:`compact` layout, two spaces are added in the
       beginning of a whole-line comment when needed


   .. _Casing_Control:

   Casing Control
   ^^^^^^^^^^^^^^

   .. index:: Casing control in gnatpp

   ``gnatpp`` allows you to specify the casing for reserved words, pragma
   names, attribute designators and identifiers. For identifiers you may define
   a general rule for name casing but also override this rule via a set of
   dictionary files.

   Three types of casing are supported: 'Lower Case', 'Upper Case', and
   'Mixed Case'. 'Mixed case' means that the first letter, and also each
   letter immediately following an underscore, are converted to their
   uppercase forms and all the other letters are converted to their lowercase
   forms.

   .. index:: --name-case-as-declared (gnatpp)

   :switch:`--name-case-as-declared, -nD`
     Name casing for defining occurrences are as they appear in the source file
     (this is the default).

   .. index:: --name-upper-case (gnatpp)

   :switch:`--name-upper-case, -nU`
     Names are in upper case.

   .. index:: --name-lower-case (gnatpp)

   :switch:`--name-lower-case, -nL`
     Names are in lower case.

   .. index:: --name-mixed-case (gnatpp)

   :switch:`--name-mixed-case, -nM`
     Names are in mixed case.

   .. index:: --attribute-lower-case (gnatpp)

   :switch:`--attribute-lower-case, -aL`
     Attribute designators are lower case.

   .. index:: --attribute-upper-case (gnatpp)

   :switch:`--attribute-upper-case, -aU`
     Attribute designators are upper case.

   .. index:: --attribute-mixed-case (gnatpp)

   :switch:`--attribute-mixed-case, -aM`
     Attribute designators are mixed case (this is the default).

   .. index:: --keyword-lower-case (gnatpp)

   :switch:`--keyword-lower-case, -kL`
     Keywords (technically, these are known in Ada as *reserved words*) are
     lower case (this is the default).

   .. index:: --keyword-upper-case (gnatpp)

   :switch:`--keyword-upper-case, -kU`
     Keywords are upper case.

   .. index:: --enum-case-as-declared (gnatpp)

   :switch:`--enum-case-as-declared, -neD`
     Enumeration literal casing for defining occurrences are as they appear in
     the source file. Overrides -n casing setting.

   .. index:: --enum-upper-case (gnatpp)

   :switch:`--enum-upper-case, -neU`
     Enumeration literals are in upper case. Overrides -n casing
     setting.

   .. index:: --enum-lower-case (gnatpp)

   :switch:`--enum-lower-case, -neL`
     Enumeration literals are in lower case. Overrides -n casing
     setting.

   .. index:: --enum-mixed-case (gnatpp)

   :switch:`--enum-mixed-case, -neM`
     Enumeration literals are in mixed case. Overrides -n casing
     setting.

   .. index:: --type-case-as-declared (gnatpp)

   :switch:`--type-case-as-declared, -ntD`
     Names introduced by type and subtype declarations are always
     cased as they appear in the declaration in the source file.
     Overrides -n casing setting.

   .. index:: --type-upper-case (gnatpp)

   :switch:`--type-upper-case, -ntU`
     Names introduced by type and subtype declarations are always in
     upper case. Overrides -n casing setting.

   .. index:: --type-lower-case (gnatpp)

   :switch:`--type-lower-case, -ntL`
     Names introduced by type and subtype declarations are always in
     lower case. Overrides -n casing setting.

   .. index:: --type-mixed-case (gnatpp)

   :switch:`--type-mixed-case, -ntM`
     Names introduced by type and subtype declarations are always in
     mixed case. Overrides -n casing setting.

   .. index:: --number-upper-case (gnatpp)

   :switch:`--number-upper-case, -nnU`
     Names introduced by number declarations are always in
     upper case. Overrides -n casing setting.

   .. index:: --number-lower-case (gnatpp)

   :switch:`--number-lower-case, -nnL`
     Names introduced by number declarations are always in
     lower case. Overrides -n casing setting.

   .. index:: --number-mixed-case (gnatpp)

   :switch:`--number-mixed-case, -nnM`
     Names introduced by number declarations are always in
     mixed case. Overrides -n casing setting.

   .. index:: --pragma-lower-case (gnatpp)

   :switch:`--pragma-lower-case, -pL`
     Pragma names are lower case.

   .. index:: --pragma-upper-case (gnatpp)

   :switch:`--pragma-upper-case, -pU`
     Pragma names are upper case.

   .. index:: --pragma-mixed-case (gnatpp)

   :switch:`--pragma-mixed-case, -pM`
     Pragma names are mixed case (this is the default).

   .. index:: --constant-case-as-non-constant (gnatpp)

   :switch:`--constant-case-as-non-constant, -cN`
     Constant object declaration names have the same case as the name casing
     (this is the default).

   .. index:: --constant-case-as-declared (gnatpp)

   :switch:`--constant-case-as-declared, -cD`
     Constant object declaration names are as they appear in the source file.

   .. index:: --constant-lower-case (gnatpp)

   :switch:`--constant-lower-case, -cL`
     Constant object declaration names are lower case.

   .. index:: --constant-upper-case (gnatpp)

   :switch:`--constan-upper-case, -cU`
     Constant object declaration names are upper case.

   .. index:: --constant-mixed-case (gnatpp)

   :switch:`--constant-mixed-case, -cM`
     Constant object declaration names are mixed case.

   .. index:: --syntax-only (gnatpp)

   :switch:`--syntax-only`
     Disable the semantic analysis (name resolution) done by libadalang.
     This means gnatpp will not be able to support any of the
     "as-declared" switches.

   .. index:: --dictionary (gnatpp)

   :switch:`--dictionary={file}, -D={file}`
     Use ``file`` as a *dictionary file* that defines
     the casing for a set of specified names,
     thereby overriding the effect on these names by
     any explicit or implicit
     -n switch.
     To supply more than one dictionary file,
     use several ``--dictionary`` switches.

     ``gnatpp`` implicitly uses a *default dictionary file*
     to define the casing for the Ada predefined names and
     the names declared in the GNAT libraries.

   .. index:: --dictionary=- (gnatpp)

   :switch:`--dictionary=-, -D=-`
     Do not use the default dictionary file;
     instead, use the casing
     defined by a ``-n`` switch and any explicit
     dictionary file(s)

   The structure of a dictionary file, and details on the conventions
   used in the default dictionary file, are defined in :ref:`Name_Casing`.

   The :switch:`--dictionary=-` and
   :switch:`--dictionary={file}` switches are mutually
   compatible.

   This group of ``gnatpp`` switches controls the layout of comments and
   complex syntactic constructs. See :ref:`Formatting_Comments` for details
   on their effect.


   .. _General_Text_Layout_Control:

   General Text Layout Control
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^

   These switches allow control over line length and indentation.

   .. index:: --max-line-length (gnatpp)

   :switch:`--max-line-length={nnn}, -M={nnn}`
     Maximum line length, ``nnn`` from 32...256, the default value is 79


   .. index:: --indentation (gnatpp)

   :switch:`--indentation={nnn}, -i={nnn}`
     Indentation level, ``nnn`` from 1...9, the default value is 3


   .. index:: --indent-continuation (gnatpp)

   :switch:`--indent-continuation={nnn}, -cl={nnn}`
     Indentation level for continuation lines (relative to the line being
     continued), ``nnn`` from 1...9.
     The default
     value is one less than the (normal) indentation level, unless the
     indentation is set to 1 (in which case the default value for continuation
     line indentation is also 1)


   .. _Other_Formatting_Options:

   Other Formatting Options
   ^^^^^^^^^^^^^^^^^^^^^^^^

   These switches control other formatting not listed above.

   .. index:: --decimal-grouping  (gnatpp)

   :switch:`--decimal-grouping={n}`
     Put underscores in decimal literals (numeric literals without a base)
     every ``n`` characters. If a literal already has one or more
     underscores, it is not modified. For example, with
     ``--decimal-grouping=3``, ``1000000`` will be changed to
     ``1_000_000``.


   .. index:: --based-grouping  (gnatpp)

   :switch:`--based-grouping={n}`
     Same as ``--decimal-grouping``, but for based literals. For
     example, with ``--based-grouping=4``, ``16#0001FFFE#`` will be
     changed to ``16#0001_FFFE#``.

   .. index:: --call-threshold (gnatpp)

   :switch:`--call-threshold={nnn}`
     If the number of parameter associations is greater than ``nnn`` and if at
     least one association uses named notation, start each association from
     a new line. If ``nnn`` is 0, no check for the number of associations
     is made; this is the default.

   .. index:: --par-threshold (gnatpp)

   :switch:`--par-threshold={nnn}`
     If the number of parameter specifications is greater than ``nnn``
     (or equal to ``nnn`` in case of a function), start each specification from
     a new line. If ``nnn`` is 0, and :switch:`--no-separate-is` was not
     specified, then the ``is`` is placed on a separate line. This feature is
     disabled by default.


   .. _Setting_the_Source_Search_Path:

   Setting the Source Search Path
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   To define the search path for the input source file, ``gnatpp``
   uses the same switches as the GNAT compiler, with the same effects:

   .. index:: -I (gnatpp)


   :switch:`-I{dir}`

   .. index:: -I- (gnatpp)

   :switch:`-I-`

   .. index:: -gnatec (gnatpp)

   :switch:`-gnatec={path}`


   .. _Output_File_Control-gnatpp:

   Output File Control
   ^^^^^^^^^^^^^^^^^^^

   By default the output overwrites the input file.
   The output may be redirected by the following switches:


   .. index:: --replace (gnatpp)

   :switch:`--replace, -rnb`
     This is the default.
     Replace the input source file with the reformatted output without
     creating any backup copy of the input source.


   .. index:: --output-dir (gnatpp)

   :switch:`--output-dir={dir}`
     Generate output file in directory :file:`dir` with the same name as
     the input file. If :file:`dir` is the same as the directory
     containing the input file, the input file is not processed; use
     ``--replace`` if you want to update the input file in
     place.


   .. index:: --pipe (gnatpp)

   :switch:`--pipe, -pipe`
     Send the output to ``Standard_Output``


   .. index:: --output (gnatpp)

   :switch:`--output={output_file}, -o={output_file}`
     Write the output into ``output_file``.
     If ``output_file`` already exists, ``gnatpp`` terminates without
     reading or processing the input file.


   .. index:: --output-force (gnatpp)

   :switch:`--output-force={output_file}, -of={output_file}`
     Write the output into ``output_file``, overwriting the existing file
     (if one is present).


   .. index:: --replace-backup (gnatpp)

   :switch:`--replace-backup, -r`
     Replace the input source file with the reformatted output, and copy the
     original input source into the file whose name is obtained by appending
     the :file:`.npp` suffix to the name of the input file.
     If a file with this name already exists, ``gnatpp`` terminates without
     reading or processing the input file.


   .. index:: --replace-force-backup (gnatpp)

   :switch:`--replace-force-backup, -rf`
     Like ``--replace-backup`` except that if the file with the specified name
     already exists, it is overwritten.


   .. index:: --eol (gnatpp)

   :switch:`--eol={xxx}`
     Specifies the line-ending style of the reformatted output file. The
     ``xxx`` string specified with the switch may be:

     * *dos* - MS DOS style, lines end with CR LF characters*
     * *crlf*  - the same as *dos*
     * *unix* - UNIX style, lines end with LF character*
     * *lf* -  the same as *unix*

     The default is to use the same end-of-line convention as the input.

   .. index:: --wide-character-encoding (gnatpp)

   :switch:`--wide-character-encoding={e}, -W={e}`
     Specify the wide character encoding method for the input and output
     files. ``e`` is one of the following:

     * *8* - UTF-8 encoding

     * *b* - Brackets encoding (default value)

   Options ``--output-file`` and ``--output-force`` are allowed only if
   the call to gnatpp contains only one file to reformat.

   Option ``--eol`` and ``--wide-character-encoding`` cannot be used together
   with the ``--pipe`` option.


   .. _Other_gnatpp_Switches:

   Other ``gnatpp`` Switches
   ^^^^^^^^^^^^^^^^^^^^^^^^^

   The additional ``gnatpp`` switches are defined in this subsection.


   .. index:: --version  (gnatpp)

   :switch:`--version`
     Display copyright and version, then exit disregarding all other options.


   .. index:: --help  (gnatpp)

   :switch:`--help`
     Display usage, then exit disregarding all other options.


   .. index:: -P  (gnatpp)

   :switch:`-P {file}`
     Indicates the name of the project file that describes the set of sources
     to be processed. The exact set of argument sources depends on other
     options specified; see below.


   .. index:: -U  (gnatpp)

   :switch:`-U`
     If a project file is specified and no argument source is explicitly
     specified (either directly or by means of ``--files`` option), process
     all the units of the closure of the argument project. Otherwise this
     option has no effect.

   :switch:`-U {main_unit}`
     If a project file is specified and no argument source is explicitly
     specified (either directly or by means of ``--files`` option), process
     the closure of units rooted at ``main_unit``. Otherwise this option
     has no effect.


   .. index:: -X  (gnatpp)

   :switch:`-X{name}={value}`
     Indicates that external variable ``name`` in the argument project
     has the value ``value``. Has no effect if no project is specified.


   .. index:: --RTS (gnatpp)

   :switch:`--RTS={rts-path}`
     Specifies the default location of the runtime library. Same meaning as the
     equivalent ``gnatmake`` flag (:ref:`Switches_for_gnatmake`).


   .. index:: --incremental  (gnatpp)

   :switch:`--incremental`
     Incremental processing on a per-file basis. Source files are only
     processed if they have been modified, or if files they depend on have
     been modified. This is similar to the way gnatmake/gprbuild only
     compiles files that need to be recompiled. A project file is required
     in this mode, and the gnat driver (as in *gnat pretty*) is not
     supported.
     (Note: this switch is not yet supported in the libadalang-based
     version of gnatpp.)


   .. index:: --pp-off  (gnatpp)

   :switch:`--pp-off={xxx}`
     Use :switch:`--xxx` as the command to turn off pretty printing, instead
     of the default ``--!pp off``.


   .. index:: --pp-on  (gnatpp)

   :switch:`--pp-on={xxx}`
     Use :switch:`--xxx` as the command to turn pretty printing back on,
     instead of the default ``--!pp on``.


   .. index:: --files (gnatpp)

   :switch:`--files={filename}, -files={filename}`
     Take as arguments the files listed in text file ``file``.
     Text file ``file`` may contain empty lines that are ignored.
     Each nonempty line should contain the name of an existing file.
     Several such switches may be specified simultaneously.


   .. index:: --ignore (gnatpp)

   :switch:`--ignore={filename}`
     Do not process the sources listed in a specified file. This option cannot
     be used in incremental mode.


   .. index:: --jobs (gnatpp)

   :switch:`--jobs={n}, -j={n}`
     With ``--incremental``, use *n* ``gnatpp`` processes to perform
     pretty printing in parallel. If *n* is 0, then the maximum number
     processes is the number of core processors on the platform.


   .. index:: --verbose (gnatpp)

   :switch:`--verbose, -v`
     Verbose mode


   .. index:: --quiet (gnatpp)

   :switch:`--quiet, -q`
     Quiet mode

   If a project file is specified and no argument source is explicitly
   specified (either directly or by means of ``--files`` option), and no
   ``-U`` is specified, then the set of processed sources is
   all the immediate units of the argument project.


   .. _Formatting_Rules:

   Formatting Rules
   ----------------

   The following subsections show how ``gnatpp`` treats, comments, program
   layout, name casing and how to disable ``gnatpp`` in source code regions.
   They provide more details of the switches shown above.


   .. _Disabling_Pretty_Printing:

   Disabling Pretty Printing
   ^^^^^^^^^^^^^^^^^^^^^^^^^

   Pretty printing is highly heuristic in nature, and sometimes doesn't
   do exactly what you want. If you wish to format a certain region of
   code by hand, you can turn off pretty printing in that region by
   surrounding it with special comments that start with ``--!pp off``
   and ``--!pp on``. The text in that region will then be reproduced
   verbatim in the output with no formatting.

   To disable pretty printing for the whole file, put ``--!pp off`` at
   the top, with no following ``--!pp on``.

   The comments must appear on a line by themselves, with nothing
   preceding except spaces. The initial text of the comment must be
   exactly ``--!pp off`` or ``--!pp on`` (case sensitive), but may
   be followed by arbitrary additional text. For example:

     .. code-block:: ada

        package Interrupts is
           --!pp off -- turn off pretty printing so "Interrupt_Kind" lines up
           type            Interrupt_Kind is
             (Asynchronous_Interrupt_Kind,
               Synchronous_Interrupt_Kind,
                     Green_Interrupt_Kind);
           --!pp on -- reenable pretty printing
           ...

   You can specify different comment strings using the ``--pp-off``
   and ``--pp-on`` switches. For example, if you say:

     ::

        $ gnatpp --pp-off=' pp-' *.ad?

   then gnatpp will recognize comments of the form
   ``-- pp-`` instead of ``--!pp off`` for disabling pretty
   printing. Note that the leading ``--`` of the comment is not
   included in the argument to these switches.


   .. _Formatting_Comments:

   Formatting Comments
   ^^^^^^^^^^^^^^^^^^^

   Only ``--layout=compact`` and ``--layout=tall`` format comments.

   Comments in Ada code are of two kinds:

   * a *whole-line comment*, which appears by itself (possibly preceded by
     white space) on a line

   * an *end-of-line comment*, which follows some other Ada code on
     the same line.

   A whole-line comment is indented according to the surrounding code,
   with some exceptions. Comments that start in column 1 are kept
   there. If possible, comments are not moved so far to the right that
   the maximum line length is exceeded. Special-form comments such as
   SPARK-style ``--#...`` are left alone.

   For an end-of-line comment, ``gnatpp`` tries to leave the same
   number of spaces between the end of the preceding Ada code and the
   beginning of the comment as appear in the original source.

   For each whole-line comment that does not end with two hyphens or that are
   not special-form comments, ``gnatpp`` inserts spaces if necessary after the
   starting two hyphens to ensure that there are at least two spaces between
   these hyphens and the first non-blank character of the comment.

   With ``--layout=compact``, whole-line comments that form a paragraph will be
   filled in typical word processor style (that is, moving words between lines
   to make the lines other than the last similar in length).


   .. _Name_Casing:

   Name Casing
   ^^^^^^^^^^^

   ``gnatpp`` always converts the usage occurrence of a (simple) name to
   the same casing as the corresponding defining identifier.

   You control the casing for defining occurrences via the ``--name...``
   switches. With ``--name-case-as-declared``, which is the default,
   defining occurrences appear exactly as in the source file where they
   are declared. The other values for this switch --
   ``--name-upper-case``, ``--name-lower-case``, ``--name-mixed-case``
   -- result in upper, lower, or mixed case, respectively. If
   ``gnatpp`` changes the casing of a defining occurrence, it
   analogously changes the casing of all the usage occurrences of this
   name.

   If the defining occurrence of a name is not in the source compilation
   unit currently being processed by ``gnatpp``, the casing of each
   reference to this name is changed according to the switch (subject to
   the dictionary file mechanism described below). Thus ``gnatpp`` acts
   as though the switch had affected the casing for the defining
   occurrence of the name.

   The options
   :switch:`--attribute...`,
   :switch:`--keyword...`,
   :switch:`--enum...`,
   :switch:`--type...`,
   :switch:`--number...`, and
   :switch:`--pragma...`
   allow finer-grained control over casing for
   attributes, keywords, enumeration literals,
   types, named numbers and pragmas, respectively.
   :switch:`--type...` cover subtypes as well.

   Some names may need to be spelled with casing conventions that are not
   covered by the upper-, lower-, and mixed-case transformations.
   You can arrange correct casing by placing such names in a
   *dictionary file*,
   and then supplying a ``--dictionary`` switch.
   The casing of names from dictionary files overrides
   any ``--name...`` switch.

   To handle the casing of Ada predefined names and the names from GNAT libraries,
   ``gnatpp`` assumes a default dictionary file.
   The name of each predefined entity is spelled with the same casing as is used
   for the entity in the :title:`Ada Reference Manual` (usually mixed case).
   The name of each entity in the GNAT libraries is spelled with the same casing
   as is used in the declaration of that entity.

   The ``--dictionary=-`` switch suppresses the use of
   the default dictionary file. Instead, the casing for predefined and
   GNAT-defined names will be established by the
   ``-n`` switch or explicit dictionary files. For
   example, by default the names ``Ada.Text_IO`` and
   ``GNAT.OS_Lib`` will appear as just shown, even in the presence of
   a ``--name-upper-case`` switch. To ensure that even
   such names are rendered in uppercase, additionally supply the
   --dictionary=- switch (or else place these names
   in upper case in a dictionary file).

   A dictionary file is a plain text file; each line in this file can be
   either a blank line (containing only space characters), an Ada comment
   line, or the specification of exactly one *casing schema*.

   A casing schema is a string that has the following syntax:

     ::

        casing_schema ::= identifier | simple_identifier

        simple_identifier ::= letter{letter_or_digit}


   (See :title:`Ada Reference Manual`, Section 2.3) for the definition of the
   ``identifier`` lexical element and the ``letter_or_digit`` category.)

   The casing schema string can be followed by white space and/or an Ada-style
   comment; any amount of white space is allowed before the string.

   If a dictionary file is passed as
   the value of a :switch:`--dictionary={file}` switch
   then for every
   simple name and every identifier, ``gnatpp`` checks if the dictionary
   defines the casing for the name or for some of its parts (the term 'subword'
   is used below to denote the part of a name which is delimited by '_' or by
   the beginning or end of the word and which does not contain any '_' inside):

   * if the whole name is in the dictionary, ``gnatpp`` uses for this name
     the casing defined by the dictionary; no subwords are checked for this word

   * for every subword ``gnatpp`` checks if the dictionary contains the
     corresponding string of the form ``simple_identifier``,
     and if it does, the casing of this ``simple_identifier`` is used
     for this subword

   * if the whole name does not contain any '_' inside, and if for this name
     the dictionary contains two entries -- one of the form ``identifier``,
     and another of the form ``simple_identifier`` -- then the first one
     is applied to define the casing of this name

   * if more than one dictionary file is passed as ``gnatpp`` switches, each
     dictionary adds new casing exceptions and overrides all the existing casing
     exceptions set by the previous dictionaries

   * when ``gnatpp`` checks if the word or subword is in the dictionary,
     this check is not case sensitive

   For example, suppose we have the following source to reformat:

     .. code-block:: ada

        procedure test is
           name1 : integer := 1;
           name4_name3_name2 : integer := 2;
           name2_name3_name4 : Boolean;
           name1_var : Float;
        begin
           name2_name3_name4 := name4_name3_name2 > name1;
        end;

   And suppose we have two dictionaries:

     ::

        *dict1:*
           NAME1
           *NaMe3*
           *Name1*

        *dict2:*
          *NAME3*

   If ``gnatpp`` is called with the following switches:

     ::

        $ gnatpp --name-mixed-case --dictionary=dict1 --dictionary=dict2 test.adb

   then we will get the following name casing in the ``gnatpp`` output:


     .. code-block:: ada

        procedure Test is
           NAME1             : Integer := 1;
           Name4_NAME3_Name2 : Integer := 2;
           Name2_NAME3_Name4 : Boolean;
           Name1_Var         : Float;
        begin
           Name2_NAME3_Name4 := Name4_NAME3_Name2 > NAME1;
        end Test;

   .. _Preprocessor_directives:

   Preprocessor Directives
   ^^^^^^^^^^^^^^^^^^^^^^^

   ``gnatpp`` has some support for preprocessor directives.
   You can use preprocessor symbols, as in ``$symbol``.
   In addition, you can use conditional compilation,
   so long as the program text is syntactically legal Ada code
   after removing all the preprocessor directives (lines starting
   with ``#``). For example, ``gnatpp`` can format the following:

     .. code-block:: ada

        package P is
        #IF SOMETHING
           X : constant Integer := 123;
        #ELSE
           X : constant Integer := 456;
        #END IF;
        end P;

   which will be formatted as if it were:

     .. code-block:: ada

        package P is
           X : constant Integer := 123;
           X : constant Integer := 456;
        end P;

   except that the ``#`` lines will be preserved.
   However, ``gnatpp`` cannot format the following:

     .. code-block:: ada

        procedure P is
        begin
        #IF SOMETHING
           if X = 0 then
        #ELSE
           if X = 1 then
        #END IF;
              null;
           end if;
        end P;

   because removing the ``#`` lines gives:

     .. code-block:: ada

        procedure P is
        begin
           if X = 0 then
           if X = 1 then
              null;
           end if;
        end P;

   which is not syntactically legal.

   Legacy Switches
   ^^^^^^^^^^^^^^^

   .. index:: --alignment (gnatpp)

   :switch:`--alignment, --no-alignment`
      Programs can be easier to read if certain constructs are vertically
      aligned.

        * ``:`` in declarations,
        * ``:=`` in initializations in declarations,
        * ``:=`` in assignment statements,
        * ``=>`` in associations, and
        * ``at`` keywords in the component clauses in record representation
          clauses.

   .. index:: --align-modes (gnatpp)

   :switch:`--align-modes, --no-align-modes`

      Line up ``in`` and ``out`` in parameter specifications.

   .. index:: --compact (gnatpp)

   :switch:`--compact, --no-compact`
     In calls and similar, ``--compact`` packs as many
     subexpressions on the same line as possible. Example:

     .. code-block:: ada

        Some_Procedure
          (Short_One, Another_Short_One,
           A_Very_Very_Very_Very_Very_Very_Very_Very_Long_One);

     On the contrary, with ``--no-compact``, in calls and similar, if it is
     necessary to split a line between two subexpressions (because otherwise
     the construct would exceed --max-line-length), then all such
     subexpressions are placed on separate lines. Example:

     .. code-block:: ada

        Some_Procedure
          (Short_One,
           Another_Short_One,
           A_Very_Very_Very_Very_Very_Very_Very_Very_Long_One);

   .. index:: --end-id (gnatpp)

   :switch:`--end-id, --no-end-id`
     Insert the name of a unit after ``end``. Otherwise, leave whatever comes
     after ``end``, if anything, alone.

   .. index:: --separate-is (gnatpp)

   :switch:`--separate-is, --no-separate-is`
     Place the keyword ``is`` on a separate line in a subprogram body in
     case if the spec occupies more than one line.

   .. index:: --separate-return (gnatpp)

   :switch:`--separate-return, --no-separate-return`
     If a subprogram spec does not fit on one line, place the ``return`` on
     a separate line. Otherwise try to place it in the same line as the last
     parameter specification.

   .. index:: --separate-loop (gnatpp)

   :switch:`--separate-loop, --no-separate-loop`
     Place the keyword ``loop`` in FOR and WHILE loop statements
     on a separate line.


   .. index:: --separate-then (gnatpp)

   :switch:`--separate-then, --no-separate-then`
     Place the keyword ``then`` in IF statements
     on a separate line.

   .. index:: --separate-loop-then (gnatpp)

   :switch:`--separate-loop-then, --no-separate-loop-then`
     Equivalent to :switch:`--separate-loop` :switch:`--separate-then`.

   .. index:: --split-line-before-record (gnatpp)

   :switch:`--split-line-before-record, --no-split-line-before-record`
     Split the line just before ``record`` in a record type declaration.

   .. index:: --indent-named-statements (gnatpp)

   :switch:`--indent-named-statements, --no-indent-named-statements`
     Named block and loop statements are indented with respect to
     the name.

   .. index:: --split-line-before-op (gnatpp)

   :switch:`--split-line-before-op, --no-split-line-before-op`
     If it is necessary to split a line at a binary operator, by default
     the line is split after the operator. With this option, it is split
     before the operator.

   .. index:: --use-on-new-line (gnatpp)

   :switch:`--use-on-new-line, --no-use-one-new-line`
     Start each USE clause in a context clause from a separate line.

   .. index:: --vertical-enum-types (gnatpp)

   :switch:`--vertical-enum-types, --no-vertical-enum-types`
     Format enumeration type declarations "vertically", e.g. each
     enumeration literal goes on a separate line.

   .. index:: --vertical-array-types (gnatpp)

   :switch:`--vertical-array-types, --no-vertical-array-types`
     Format array type declarations "vertically", e.g. for
     multidimensional arrays, each index_subtype_definition or
     discrete_subtype_definition goes on a separate line.

   .. index:: --vertical-named-aggregates (gnatpp)

   :switch:`--vertical-named-aggregates, --no-vertical-named-aggregates`
     Format aggregates "vertically" if named notation is used for all
     component_associations, e.g. each component_association
     goes on a separate line.

   .. index:: --vertical-case-alternatives (gnatpp)

   :switch:`--vertical-case-alternatives, --no-vertical-case-alternatives`
     Format case statements, case expressions, and variant parts with
     additional line breaks.

   .. index:: --RM-style-spacing (gnatpp)

   :switch:`--RM-style-spacing`
     Do not insert an extra blank before various occurrences of
     '(' and ':'. Alignment is off by default in this mode;
     use :switch:`--alignment` to turn it on.

   .. index:: --insert-blank-lines (gnatpp)

   :switch:`--insert-blank-lines, --no-insert-blank-lines`
     Insert blank lines where appropriate (between bodies and other large
     constructs).

   .. index:: --preserve-blank-lines (gnatpp)

   :switch:`--preserve-blank-lines, --no-preserve-blank-lines`
     Preserve blank lines in the input. By default, gnatpp will squeeze
     multiple blank lines down to one.

   .. index:: --preserve-line-breaks (gnatpp)

   :switch:`--preserve-line-breaks, --no-preserve-line-breaks`
     Preserve line breaks in the input, to the extent possible.
     By default, line breaks are also inserted at appropriate
     places.

   .. index:: --source-line-breaks (gnatpp)

   :switch:`--source-line-breaks, --no-source-line-breaks`
     Keep the line breaks from the source; do not insert or delete any
     line breaks.

   .. index:: --spaces-only (gnatpp)

   :switch:`--spaces-only, --no-spaces-only`
     Disable all formatting except for inserting and removing spaces.
     This implies --source-line-breaks.

   .. index:: -c (gnatpp)

   :switch:`--comments-unchanged, -c0`
     All comments remain unchanged.

   :switch:`--comments-gnat-indentation, -c1`
     GNAT-style comment line indentation.

   :switch:`--comments-gnat-beginning, -c3`
     GNAT-style comment beginning.

   :switch:`--comments-fill, -c4`
     Fill comment blocks.

   :switch:`--comments-special, -c5`
     Keep unchanged special form comments.

   :switch:`--comments-only`
     Format just the comments.


.. only:: PRO or GPL

  .. _The_Body_Stub_Generator_gnatstub:

  The Body Stub Generator ``gnatstub``
  ====================================

  .. index:: ! gnatstub

  ``gnatstub`` creates empty but compilable bodies
  for library unit declarations, and empty but compilable
  subunits for body stubs.

  ``gnatstub`` is a project-aware tool.
  (See :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
  the project-related switches but note that ``gnatstub`` does not support
  the :switch:`-U`, :switch:`-U {main_unit}`, :switch:`--subdirs={dir}`, or
  :switch:`--no-objects-dir` switches.)
  The project file package that can specify
  ``gnatstub`` switches is named ``gnatstub``.


  By default, all the program unit bodies generated by ``gnatstub``
  raise ``Program_Error``, which will catch accidental calls of
  generated stubs. This behavior can be changed with option
  ``--no-exception`` (see below).

  .. _Running_gnatstub:

  Running ``gnatstub``
  --------------------

  ``gnatstub`` invocation has the following form:

    ::

       $ gnatstub [ switches ] {filename}

  where

  * *filename*
      is the name of the source file that contains a library unit declaration
      for which a body must be created or a library unit body for which subunits
      must be created for the body stubs declared in this body.
      The file name may contain path information.
      If the name does not follow GNAT file naming conventions and the set
      of switches does not contain a project file that defines naming
      conventions, the name of the body file must
      be provided
      explicitly as the value of the :switch:`--output={body-name}` option.
      If the file name follows the GNAT file naming
      conventions and the name of the body file is not provided,
      ``gnatstub``
      takes the naming conventions for the generated source from the
      project file provided as a parameter of ``-P`` switch if any,
      or creates the name file to generate using the standard GNAT
      naming conventions.

      Note that it is no longer necessary to specify the Ada language version;
      ``gnatstub`` can process Ada source code written in any version from
      Ada 83 onward without specifying any language version switch.

  * *switches*
      is an optional sequence of switches as described in the next section


  .. _Switches_for_gnatstub:

  Switches for ``gnatstub``
  -------------------------

  .. index:: --version (gnatstub)

  :switch:`--version`
    Display copyright and version, then exit disregarding all other options.


  .. index:: --help (gnatstub)

  :switch:`--help`
    Display usage, then exit disregarding all other options.


  .. index:: -P (gnatstub)

  :switch:`-P {file}`
    Indicates the name of the project file that describes the set of sources
    to be processed. An aggregate project is allowed as the file parameter only
    if it has exactly one non-aggregate project being aggregated.


  .. index:: -X (gnatstub)

  :switch:`-X{name}={value}`
    Indicates that external variable ``name`` in the argument project
    has the value ``value``. Has no effect if no project is specified.


  .. index:: --RTS (gnatstub)

  :switch:`--RTS={rts-path}`
    Specifies the default location of the runtime library. Same meaning as the
    equivalent ``gnatmake`` flag (:ref:`Switches_for_gnatmake`).


  .. index:: --subunits (gnatstub)

  :switch:`--subunits`
    Generate subunits for body stubs. If this switch is specified,
    ``gnatstub`` expects a library unit body as an argument file;
    otherwise a library unit declaration is expected. If a body stub
    already has a corresponding subunit, ``gnatstub`` does not
    generate anything for it.


  .. index:: --force (gnatstub)

  :switch:`--force`
    If the destination directory already contains a file with the name of the
    body file
    for the argument spec file, replace it with the generated body stub.
    This switch cannot be used together with ``--subunits``.


  .. index:: --comment-header-spec (gnatstub)

  :switch:`--comment-header-spec`
    Put the comment header (i.e., all the comments preceding the
    compilation unit) from the source of the library unit declaration
    into the body stub.


  .. index:: --comment-header-sample (gnatstub)

  :switch:`--comment-header-sample`
    Put a sample comment header into the body stub.


  .. index:: --header-file (gnatstub)

  :switch:`--header-file={filename}`
    Use the content of the file as the comment header for a generated body stub.


  .. index:: --max-line-length (gnatstub)

  :switch:`--max-line-length={n}`
    (``n`` is a non-negative integer). Set the maximum line length for
    the output files. The default is 79. The maximum value that can be
    specified is 32767.


  .. index:: --indentation (gnatstub)

  :switch:`--indentation={n}`
    (``n`` is an integer from 1 to 9). Set the indentation level in
    the generated files to ``n``.
    The default indentation is 3.


  .. index:: --alphabetical-order (gnatstub)

  :switch:`--alphabetical-order`
    Order local bodies alphabetically. (By default local bodies are ordered
    in the same way as the corresponding local specs in the argument
    spec file.)


  .. index:: --no-exception (gnatstub)

  :switch:`--no-exception`
    Avoid raising Program_Error in the generated bodies of program unit stubs,
    except in the case of functions, where we have no value to return.


  .. index:: --no-local-header (gnatstub)

  :switch:`--no-local-header`
    Do not place local comment header with unit name before body stub for a
    unit.


  .. index:: --files (gnatstub)

  :switch:`--files={filename}`
    Take as arguments the files listed in text file ``file``.
    Text file ``file`` may contain empty lines that are ignored.
    Each nonempty line should contain the name of an existing file.
    Several such switches may be specified.


  .. index:: --output (gnatstub)

  :switch:`--output={body-name}`
    Body file name. This should be set if the argument file name does
    not follow the default GNAT file naming conventions, and the naming
    conventions are not specified by a project file. If this switch and
    ``-P`` are both omitted, the name for the body will be obtained
    according to the default GNAT file naming conventions.


  .. index:: --output-dir (gnatstub)

  :switch:`--output-dir={dir-name}`
    The directory in which to place the output files.
    If this switch is not set, the generated library unit body is
    placed in the current directory, and generated sununits
    in the directory where the argument body is located.


  .. index:: --wide-character-encoding (gnatstub)

  :switch:`--wide-character-encoding={e}`
    Specify the wide character encoding method for the input and output
    files. ``e`` is one of the following:

    * *8* - UTF-8 encoding

    * *b* - Brackets encoding (default value)


  .. index:: --quiet (gnatstub)
  .. index:: -q (gnatstub)

  :switch:`--quiet` / :switch:`-q`
    Quiet mode.


  .. index:: --verbose (gnatstub)
  .. index:: -v (gnatstub)

  :switch:`--verbose` / :switch:`-v`
    Verbose mode.

  Legacy Switches
  ^^^^^^^^^^^^^^^

  Some switches have a short form, mostly for legacy reasons,
  as shown below.

  .. index:: -M (gnatstub)

  :switch:`-gnatyM{nnn}`
    :switch:`--max-line-length={nnn}`

  .. index:: -i (gnatstub)

  :switch:`-i{nnn}`
    :switch:`--indentation={nnn}`

  .. index:: -gnaty (gnatstub)

  :switch:`-gnaty{nnn}`
    :switch:`--indentation={nnn}`

  .. index:: -f (gnatstub)

  :switch:`-f`
    :switch:`--force`

  .. index:: -gnatyo (gnatstub)

  :switch:`-gnatyo`
    :switch:`--alphabetical-order`

  .. index:: -hg (gnatstub)

  :switch:`-hg`
    :switch:`--comment-header-sample`

  .. index:: -hs (gnatstub)

  :switch:`-hs`
    :switch:`--comment-header-spec`

  .. index:: -o (gnatstub)

  :switch:`-o {output-file}`
    :switch:`--output={output-file}`

  .. index:: -dir (gnatstub)

  :switch:`-dir {dir-name}`
    :switch:`--output-dir={dir-name}`

  .. index:: -W (gnatstub)

  :switch:`-W{e}`
    :switch:`--wide-character-encoding={e}`

  .. index:: -files (gnatstub)

  :switch:`-files {filename}`
    :switch:`--files={filename}`


.. only:: PRO or GPL

  .. _The_Backtrace_Symbolizer_gnatsymbolize:

  Translating Code Addresses into Source Locations with ``gnatsymbolize``
  =======================================================================

  .. index:: ! gnatsymbolize

  ``gnatsymbolize`` is a program which translates addresses into
  their corresponding filename, line number, and function names.

  Running ``gnatsymbolize``
  -------------------------

  ::

       $ gnatsymbolize [ switches ] filename [ addresses ]

  For instance, consider the following Ada program:

     .. code-block:: ada

        package Pck is
           Global_Val : Integer := 0;
           procedure Call_Me_First;
        end Pck;

        with GNAT.IO; use GNAT.IO;
        with GNAT.Traceback; use GNAT.Traceback;
        with GNAT.Debug_Utilities;

        package body Pck is
           procedure Call_Me_Third is
              TB : Tracebacks_Array (1 .. 5);
              TB_len : Natural;
           begin
              Global_Val := Global_Val + 1;

              Call_Chain (TB, TB_Len);
              for K in 1 .. TB_Len loop
                 Put_Line (GNAT.Debug_Utilities.Image_C (TB (K)));
              end loop;
           end Call_Me_Third;

           procedure Call_Me_Second is
           begin
              Call_Me_Third;
           end Call_Me_Second;

           procedure Call_Me_First is
           begin
              Call_Me_Second;
           end Call_Me_First;
        end Pck;

        with GNAT.IO; use GNAT.IO;
        with GNAT.Debug_Utilities;
        with GNAT.Traceback;
        with System;

        with Pck; use Pck;

        procedure Foo is
           LA : constant System.Address := \
             GNAT.Traceback.Executable_Load_Address;

           use type System.Address;

        begin
           if LA /= System.Null_Address then
              Put_Line ("Load address: " & GNAT.Debug_Utilities.Image_C (LA));
           end if;

           Global_Val := 123;
           Call_Me_First;
        end Foo;

  This program, when built and run, prints a list of addresses which
  correspond to the traceback when inside function ``Call_Me_Third``.
  For instance, on x86-64 GNU/Linux:

    ::

       $ gnatmake -g -q foo.adb
       $ ./foo
       Load address: 0x00005586C9D7D000
       0x00005586C9D81105
       0x00005586C9D8119B
       0x00005586C9D811A7
       0x00005586C9D8128C
       0x00005586C9D81069

  ``gnatsymbolize`` can be used to translate those addresses into
  code locations as follow:

    ::

       $ gnatsymbolize --load foo 0x00005586C9D7D000 0x00005586C9D81105 \
           0x00005586C9D8119B 0x00005586C9D811A7 0x00005586C9D8128C \
           0x00005586C9D81069
       0x5586c9d81105 Pck.Call_Me_Third at pck.adb:12
       0x5586c9d8119b Pck.Call_Me_Second at pck.adb:20
       0x5586c9d811a7 Pck.Call_Me_First at pck.adb:25
       0x5586c9d8128c Foo at foo.adb:6
       0x5586c9d81069 Main at b~foo.adb:199

  Switches for ``gnatsymbolize``
  ------------------------------

  ``gnatsymbolize`` recognizes the following switches:

  .. index:: --help (gnatsymbolize)

  :switch:`--help`
    Display the program's usage, and then exit, disregarding all other
    options.

  :switch:`--cache`
    Read the symbolic information from the executable and cache them
    in memory in order to accelerate the translation of each address
    into a symbolic location.

    Depending on the size of the executable and the number of addresses
    to translate, this may not always make ``gnatsymbolize`` faster
    overall.

  :switch:`--dump`
    If :switch:`--cache` is used, dump the contents of the cache on
    Standard Output. Has no effect otherwise.

  :switch:`--count={N}`
    Compute the symbolic traceback ``N`` times in a row. This option
    is mostly useful for measuring the performance of ``gnatsymbolize``,
    particularly in the case where the cache is being used.

  :switch:`--load`
    Interpret the first address as the load address of the executable.
    This is needed for position-independent executables on Linux and Windows.

  Requirements for Correct Operation
  ----------------------------------

  The translation is performed by reading the DWARF debugging
  information produced by the compiler for each unit. All units
  for which the translation is to be done must therefore be compiled
  such that DWARF debugging information is produced. In most cases,
  this is done by simply compiling with ``-g``.

  This program provides a functionality similar to ``addr2line``.
  It has fewer options to tailor its output, but has been designed
  to require fewer of the DWARF sections to be present in the
  executable. In particular, it works for code compiled with ``-g1``.


.. only:: PRO or GPL

   .. _Using_Project_Files_with_GNAT_Tools:

   Using Project Files with GNAT Tools
   ===================================

   This section describes how project files can be used in conjunction
   with a number of GNAT tools.
   For a comprehensive description of project files and the overall
   GNAT Project Manager facility, please refer to the
   *GNAT Project Manager* chapter in the
   *GPRbuild and GPR Companion Tools User's Guide*.

   .. index:: Project-aware tool

   If a tool can take a project file as an option and extract the needed
   information, such a tool is called a *project-aware* tool.

   .. _Switches_Related_to_Project_Files:

   Switches Related to Project Files
   ---------------------------------

   The following switches are used by the project-aware GNAT tools:

   :switch:`-P{project_file}`
      Indicates the name of the project file whose source files are to
      be processed. The exact set of sources depends on other options
      specified, see below.

   :switch:`-U`
      If a project file is supplied, say for project ``proj``,
      but no sources are specified for ``proj`` (either by a
      project attribute or through a tool option that provides a list
      of the files to be used), process all the source files
      from projects imported either directly or indirectly by ``proj``.
      Otherwise this option has no effect.

   :switch:`-U {source_file}`
      Similar to :switch:`-U`, but if no sources are specified then
      process only those source files for units in the closure of
      the Ada source contained in ``source_file``. Note that this option
      expects the source file name but not the Ada unit name as its
      parameter.

   :switch:`-X{name}={val}`
      Indicates that the external variable ``name`` in the project has the
      value ``val``. Has no effect if no project has been specified.

   :switch:`--subdirs={dir}`
      Use the ``dir`` subdirectory of the project's object directory (or the ``dir``
      subdirectory of the project file directory if the project does not specify
      an object directory) for tool output files. Has no effect if no project
      has been specified or if :switch:`--no-objects-dir` is specified.

   :switch:`--no-objects-dir`
      Place all the result files into the current directory (i.e., the directory
      from which the tool invocation command is issued) instead of the project's
      object directory. Has no effect if no project has been specified.

   :switch:`-eL`
      Follow all symbolic links when processing project files.

   If a project file is specified and there is neither a :switch:`-U` option,
   nor a :switch:`-U {main_unit}` option, nor some other explicit option to
   specify the source files, then the sources to be processed are the
   immediate sources of the specified project (i.e., the source files directly
   defined by that project, either implicitly by residing in the project
   source directories, or explicitly through any of the source-related
   attributes).

   .. _Tool-specific_packages_in_project files:

   Tool-specific packages in project files
   ---------------------------------------

   Each project-aware tool may have a corresponding package in a project file;
   the package names are given elsewhere in this manual, in the sections that describe
   the respective tools.

   A tool-specific package in a project file may define the ``Default_Switches``
   attribute indexed by "ada" (as language name). The value of this attribute
   is a list of switches that will be supplied at tool invocation.
   Project-specific switches cannot be specified through this attribute.
