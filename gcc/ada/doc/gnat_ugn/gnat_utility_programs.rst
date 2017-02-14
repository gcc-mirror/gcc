.. _GNAT_Utility_Programs:

*********************
GNAT Utility Programs
*********************

This chapter describes a number of utility programs:

.. only:: PRO or GPL

  * :ref:`The_File_Cleanup_Utility_gnatclean`
  * :ref:`The_GNAT_Library_Browser_gnatls`
  * :ref:`The_Cross-Referencing_Tools_gnatxref_and_gnatfind`
  * :ref:`The_Ada_to_HTML_Converter_gnathtml`
  * :ref:`The_Ada-to-XML_Converter_gnat2xml`
  * :ref:`The_Program_Property_Verifier_gnatcheck`
  * :ref:`The_GNAT_Metrics_Tool_gnatmetric`
  * :ref:`The_GNAT_Pretty-Printer_gnatpp`
  * :ref:`The_Body_Stub_Generator_gnatstub`
  * :ref:`The_Unit_Test_Generator_gnattest`

  It also describes how several of these tools can be used in conjunction
  with project files: :ref:`Using_Project_Files_with_GNAT_Tools`

.. only:: FSF

  * :ref:`The_File_Cleanup_Utility_gnatclean`
  * :ref:`The_GNAT_Library_Browser_gnatls`
  * :ref:`The_Cross-Referencing_Tools_gnatxref_and_gnatfind`
  * :ref:`The_Ada_to_HTML_Converter_gnathtml`

Other GNAT utilities are described elsewhere in this manual:

* :ref:`Handling_Arbitrary_File_Naming_Conventions_with_gnatname`
* :ref:`File_Name_Krunching_with_gnatkr`
* :ref:`Renaming_Files_with_gnatchop`
* :ref:`Preprocessing_with_gnatprep`


.. _The_File_Cleanup_Utility_gnatclean:

The File Cleanup Utility *gnatclean*
====================================

.. index:: File cleanup tool
.. index:: gnatclean

`gnatclean` is a tool that allows the deletion of files produced by the
compiler, binder and linker, including ALI files, object files, tree files,
expanded source files, library files, interface copy source files, binder
generated files and executable files.

.. _Running_gnatclean:

Running `gnatclean`
-------------------

The `gnatclean` command has the form:

  ::

      $ gnatclean switches `names`

where `names` is a list of source file names. Suffixes :file:`.ads` and
:file:`adb` may be omitted. If a project file is specified using switch
:samp:`-P`, then `names` may be completely omitted.

In normal mode, `gnatclean` delete the files produced by the compiler and,
if switch `-c` is not specified, by the binder and
the linker. In informative-only mode, specified by switch
`-n`, the list of files that would have been deleted in
normal mode is listed, but no file is actually deleted.


.. _Switches_for_gnatclean:

Switches for `gnatclean`
------------------------

`gnatclean` recognizes the following switches:

.. index:: --version (gnatclean)

:samp:`--version`
  Display Copyright and version, then exit disregarding all other options.

.. index:: --help (gnatclean)

:samp:`--help`
  If *--version* was not used, display usage, then exit disregarding
  all other options.

:samp:`--subdirs={subdir}`
  Actual object directory of each project file is the subdirectory subdir of the
  object directory specified or defaulted in the project file.

:samp:`--unchecked-shared-lib-imports`
  By default, shared library projects are not allowed to import static library
  projects. When this switch is used on the command line, this restriction is
  relaxed.

.. index:: -c (gnatclean)

:samp:`-c`
  Only attempt to delete the files produced by the compiler, not those produced
  by the binder or the linker. The files that are not to be deleted are library
  files, interface copy files, binder generated files and executable files.

.. index:: -D (gnatclean)

:samp:`-D {dir}`
  Indicate that ALI and object files should normally be found in directory `dir`.

.. index:: -F (gnatclean)

:samp:`-F`
  When using project files, if some errors or warnings are detected during
  parsing and verbose mode is not in effect (no use of switch
  -v), then error lines start with the full path name of the project
  file, rather than its simple file name.

.. index:: -h (gnatclean)

:samp:`-h`
  Output a message explaining the usage of `gnatclean`.

.. index:: -n (gnatclean)

:samp:`-n`
  Informative-only mode. Do not delete any files. Output the list of the files
  that would have been deleted if this switch was not specified.

.. index:: -P (gnatclean)

:samp:`-P{project}`
  Use project file `project`. Only one such switch can be used.
  When cleaning a project file, the files produced by the compilation of the
  immediate sources or inherited sources of the project files are to be
  deleted. This is not depending on the presence or not of executable names
  on the command line.

.. index:: -q (gnatclean)

:samp:`-q`
  Quiet output. If there are no errors, do not output anything, except in
  verbose mode (switch -v) or in informative-only mode
  (switch -n).

.. index:: -r (gnatclean)

:samp:`-r`
  When a project file is specified (using switch -P),
  clean all imported and extended project files, recursively. If this switch
  is not specified, only the files related to the main project file are to be
  deleted. This switch has no effect if no project file is specified.

.. index:: -v (gnatclean)

:samp:`-v`
  Verbose mode.

.. index:: -vP (gnatclean)

:samp:`-vP{x}`
  Indicates the verbosity of the parsing of GNAT project files.
  :ref:`Switches_Related_to_Project_Files`.

.. index:: -X (gnatclean)

:samp:`-X{name}={value}`
  Indicates that external variable `name` has the value `value`.
  The Project Manager will use this value for occurrences of
  `external(name)` when parsing the project file.
  :ref:`Switches_Related_to_Project_Files`.

.. index:: -aO (gnatclean)

:samp:`-aO{dir}`
  When searching for ALI and object files, look in directory `dir`.

.. index:: -I (gnatclean)

:samp:`-I{dir}`
  Equivalent to :samp:`-aO{dir}`.

.. index:: -I- (gnatclean)

.. index:: Source files, suppressing search

:samp:`-I-`
  Do not look for ALI or object files in the directory
  where `gnatclean` was invoked.



.. _The_GNAT_Library_Browser_gnatls:

The GNAT Library Browser `gnatls`
=================================

.. index:: Library browser
.. index:: ! gnatls

`gnatls` is a tool that outputs information about compiled
units. It gives the relationship between objects, unit names and source
files. It can also be used to check the source dependencies of a unit
as well as various characteristics.

Note: to invoke `gnatls` with a project file, use the `gnat`
driver (see :ref:`The_GNAT_Driver_and_Project_Files`).

.. _Running_gnatls:

Running `gnatls`
----------------

The `gnatls` command has the form

  ::

      $ gnatls switches `object_or_ali_file`

The main argument is the list of object or :file:`ali` files
(see :ref:`The_Ada_Library_Information_Files`)
for which information is requested.

In normal mode, without additional option, `gnatls` produces a
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
  require recompilation. If you use gnatmake with the qualifier
  *-m (minimal recompilation)*, a file marked
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

Switches for `gnatls`
---------------------

`gnatls` recognizes the following switches:


.. index:: --version (gnatls)

:samp:`--version`
  Display Copyright and version, then exit disregarding all other options.


.. index:: --help (gnatls)

:samp:`*--help`
  If *--version* was not used, display usage, then exit disregarding
  all other options.


.. index:: -a (gnatls)

:samp:`-a`
  Consider all units, including those of the predefined Ada library.
  Especially useful with *-d*.


.. index:: -d (gnatls)

:samp:`-d`
  List sources from which specified units depend on.


.. index:: -h (gnatls)

:samp:`-h`
  Output the list of options.


.. index:: -o (gnatls)

:samp:`-o`
  Only output information about object files.


.. index:: -s (gnatls)

:samp:`-s`
  Only output information about source files.


.. index:: -u (gnatls)

:samp:`-u`
  Only output information about compilation units.


.. index:: -files (gnatls)

:samp:`-files={file}`
  Take as arguments the files listed in text file `file`.
  Text file `file` may contain empty lines that are ignored.
  Each nonempty line should contain the name of an existing file.
  Several such switches may be specified simultaneously.


.. index:: -aO (gnatls)

.. index:: -aI (gnatls)

.. index:: -I (gnatls)

.. index:: -I- (gnatls)

:samp:`-aO{dir}`, :samp:`-aI{dir}`, :samp:`-I{dir}`, :samp:`-I-`, :samp:`-nostdinc`
  Source path manipulation. Same meaning as the equivalent *gnatmake*
  flags (:ref:`Switches_for_gnatmake`).


.. index:: -aP (gnatls)

:samp:`-aP{dir}`
  Add `dir` at the beginning of the project search dir.


.. index:: --RTS (gnatls)

:samp:`--RTS={rts-path}``
  Specifies the default location of the runtime library. Same meaning as the
  equivalent *gnatmake* flag (:ref:`Switches_for_gnatmake`).


.. index:: -v (gnatls)

:samp:`-v`
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

Example of `gnatls` Usage
-------------------------

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


.. _The_Cross-Referencing_Tools_gnatxref_and_gnatfind:

The Cross-Referencing Tools `gnatxref` and `gnatfind`
=====================================================

.. index:: ! gnatxref
.. index:: ! gnatfind

The compiler generates cross-referencing information (unless
you set the :samp:`-gnatx` switch), which are saved in the :file:`.ali` files.
This information indicates where in the source each entity is declared and
referenced. Note that entities in package Standard are not included, but
entities in all other predefined units are included in the output.

Before using any of these two tools, you need to compile successfully your
application, so that GNAT gets a chance to generate the cross-referencing
information.

The two tools `gnatxref` and `gnatfind` take advantage of this
information to provide the user with the capability to easily locate the
declaration and references to an entity. These tools are quite similar,
the difference being that `gnatfind` is intended for locating
definitions and/or references to a specified entity or entities, whereas
`gnatxref` is oriented to generating a full report of all
cross-references.

To use these tools, you must not compile your application using the
*-gnatx* switch on the *gnatmake* command line
(see :ref:`The_GNAT_Make_Program_gnatmake`). Otherwise, cross-referencing
information will not be generated.

Note: to invoke `gnatxref` or `gnatfind` with a project file,
use the `gnat` driver (see :ref:`The_GNAT_Driver_and_Project_Files`).

.. _gnatxref_Switches:

`gnatxref` Switches
-------------------

The command invocation for `gnatxref` is:

  ::

      $ gnatxref [`switches`] `sourcefile1` [`sourcefile2` ...]

where

*sourcefile1* [, *sourcefile2* ...]
  identify the source files for which a report is to be generated. The
  'with'ed units will be processed too. You must provide at least one file.

  These file names are considered to be regular expressions, so for instance
  specifying :file:`source\*.adb` is the same as giving every file in the current
  directory whose name starts with :file:`source` and whose extension is
  :file:`adb`.

  You shouldn't specify any directory name, just base names. *gnatxref*
  and *gnatfind* will be able to locate these files by themselves using
  the source path. If you specify directories, no result is produced.

The following switches are available for *gnatxref*:


.. index:: --version (gnatxref)

:samp:`-version`
  Display Copyright and version, then exit disregarding all other options.


.. index:: --help (gnatxref)

:samp:`-help`
  If *--version* was not used, display usage, then exit disregarding
  all other options.


.. index:: -a (gnatxref)

:samp:`a`
  If this switch is present, `gnatfind` and `gnatxref` will parse
  the read-only files found in the library search path. Otherwise, these files
  will be ignored. This option can be used to protect Gnat sources or your own
  libraries from being parsed, thus making `gnatfind` and `gnatxref`
  much faster, and their output much smaller. Read-only here refers to access
  or permissions status in the file system for the current user.


.. index:: -aIDIR (gnatxref)

:samp:`aI{DIR}`
  When looking for source files also look in directory DIR. The order in which
  source file search is undertaken is the same as for *gnatmake*.


.. index:: -aODIR (gnatxref)

:samp:`aO{DIR}`
  When searching for library and object files, look in directory
  DIR. The order in which library files are searched is the same as for
  *gnatmake*.


.. index:: -nostdinc (gnatxref)

:samp:`nostdinc`
  Do not look for sources in the system default directory.


.. index:: -nostdlib (gnatxref)

:samp:`nostdlib`
  Do not look for library files in the system default directory.


.. index:: --ext (gnatxref)

:samp:`-ext={extension}`
  Specify an alternate ali file extension. The default is `ali` and other
  extensions (e.g. `gli` for C/C++ sources when using *-fdump-xref*)
  may be specified via this switch. Note that if this switch overrides the
  default, which means that only the new extension will be considered.


.. index:: --RTS (gnatxref)

:samp:`-RTS={rts-path}`
  Specifies the default location of the runtime library. Same meaning as the
  equivalent *gnatmake* flag (:ref:`Switches_for_gnatmake`).


.. index:: -d (gnatxref)

:samp:`d`
  If this switch is set `gnatxref` will output the parent type
  reference for each matching derived types.


.. index:: -f (gnatxref)

:samp:`f`
  If this switch is set, the output file names will be preceded by their
  directory (if the file was found in the search path). If this switch is
  not set, the directory will not be printed.


.. index:: -g (gnatxref)

:samp:`g`
  If this switch is set, information is output only for library-level
  entities, ignoring local entities. The use of this switch may accelerate
  `gnatfind` and `gnatxref`.


.. index:: -IDIR (gnatxref)

:samp:`I{DIR}`
  Equivalent to :samp:`-aODIR -aIDIR`.


.. index:: -pFILE (gnatxref)

:samp:`p{FILE}`
  Specify a project file to use.

  By default, `gnatxref` and `gnatfind` will try to locate a
  project file in the current directory.

  If a project file is either specified or found by the tools, then the content
  of the source directory and object directory lines are added as if they
  had been specified respectively by :samp:`-aI`
  and :samp:`-aO`.

:samp:`u`
  Output only unused symbols. This may be really useful if you give your
  main compilation unit on the command line, as `gnatxref` will then
  display every unused entity and 'with'ed package.


:samp:`v`
  Instead of producing the default output, `gnatxref` will generate a
  :file:`tags` file that can be used by vi. For examples how to use this
  feature, see :ref:`Examples_of_gnatxref_Usage`. The tags file is output
  to the standard output, thus you will have to redirect it to a file.

All these switches may be in any order on the command line, and may even
appear after the file names. They need not be separated by spaces, thus
you can say :samp:`gnatxref -ag` instead of :samp:`gnatxref -a -g`.

.. _gnatfind_Switches:

`gnatfind` Switches
-------------------

The command invocation for `gnatfind` is:

  ::

    $ gnatfind [`switches`] `pattern`[:`sourcefile`[:`line`[:`column`]]]
          [`file1` `file2` ...]

with the following iterpretation of the command arguments:

*pattern*
  An entity will be output only if it matches the regular expression found
  in `pattern`, see :ref:`Regular_Expressions_in_gnatfind_and_gnatxref`.

  Omitting the pattern is equivalent to specifying ``*``, which
  will match any entity. Note that if you do not provide a pattern, you
  have to provide both a sourcefile and a line.

  Entity names are given in Latin-1, with uppercase/lowercase equivalence
  for matching purposes. At the current time there is no support for
  8-bit codes other than Latin-1, or for wide characters in identifiers.

*sourcefile*
  `gnatfind` will look for references, bodies or declarations
  of symbols referenced in :file:`sourcefile`, at line `line`
  and column `column`. See :ref:`Examples_of_gnatfind_Usage`
  for syntax examples.

*line*
  A decimal integer identifying the line number containing
  the reference to the entity (or entities) to be located.


*column*
  A decimal integer identifying the exact location on the
  line of the first character of the identifier for the
  entity reference. Columns are numbered from 1.


*file1 file2 ...*
  The search will be restricted to these source files. If none are given, then
  the search will be conducted for every library file in the search path.
  These files must appear only after the pattern or sourcefile.

  These file names are considered to be regular expressions, so for instance
  specifying :file:`source\*.adb` is the same as giving every file in the current
  directory whose name starts with :file:`source` and whose extension is
  :file:`adb`.

  The location of the spec of the entity will always be displayed, even if it
  isn't in one of :file:`file1`, :file:`file2`, ... The
  occurrences of the entity in the separate units of the ones given on the
  command line will also be displayed.

  Note that if you specify at least one file in this part, `gnatfind` may
  sometimes not be able to find the body of the subprograms.


At least one of 'sourcefile' or 'pattern' has to be present on
the command line.

The following switches are available:

.. index:: --version (gnatfind)

:samp:`--version`
  Display Copyright and version, then exit disregarding all other options.


.. index:: --help (gnatfind)

:samp:`-help`
  If *--version* was not used, display usage, then exit disregarding
  all other options.


.. index:: -a (gnatfind)

:samp:`a`
  If this switch is present, `gnatfind` and `gnatxref` will parse
  the read-only files found in the library search path. Otherwise, these files
  will be ignored. This option can be used to protect Gnat sources or your own
  libraries from being parsed, thus making `gnatfind` and `gnatxref`
  much faster, and their output much smaller. Read-only here refers to access
  or permission status in the file system for the current user.


.. index:: -aIDIR (gnatfind)

:samp:`aI{DIR}`
  When looking for source files also look in directory DIR. The order in which
  source file search is undertaken is the same as for *gnatmake*.


.. index:: -aODIR (gnatfind)

:samp:`aO{DIR}`
  When searching for library and object files, look in directory
  DIR. The order in which library files are searched is the same as for
  *gnatmake*.


.. index:: -nostdinc (gnatfind)

:samp:`nostdinc`
  Do not look for sources in the system default directory.


.. index:: -nostdlib (gnatfind)

:samp:`nostdlib`
  Do not look for library files in the system default directory.


.. index:: --ext (gnatfind)

:samp:`-ext={extension}`
  Specify an alternate ali file extension. The default is `ali` and other
  extensions (e.g. `gli` for C/C++ sources when using *-fdump-xref*)
  may be specified via this switch. Note that if this switch overrides the
  default, which means that only the new extension will be considered.


.. index:: --RTS (gnatfind)

:samp:`-RTS={rts-path}`
  Specifies the default location of the runtime library. Same meaning as the
  equivalent *gnatmake* flag (:ref:`Switches_for_gnatmake`).


.. index:: -d (gnatfind)

:samp:`d`
  If this switch is set, then `gnatfind` will output the parent type
  reference for each matching derived types.


.. index:: -e (gnatfind)

:samp:`e`
  By default, `gnatfind` accept the simple regular expression set for
  `pattern`. If this switch is set, then the pattern will be
  considered as full Unix-style regular expression.


.. index:: -f (gnatfind)

:samp:`f`
  If this switch is set, the output file names will be preceded by their
  directory (if the file was found in the search path). If this switch is
  not set, the directory will not be printed.


.. index:: -g (gnatfind)

:samp:`g`
  If this switch is set, information is output only for library-level
  entities, ignoring local entities. The use of this switch may accelerate
  `gnatfind` and `gnatxref`.


.. index:: -IDIR (gnatfind)

:samp:`I{DIR}`
  Equivalent to :samp:`-aODIR -aIDIR`.


.. index:: -pFILE (gnatfind)

:samp:`p{FILE}`
  Specify a project file.
  By default, `gnatxref` and `gnatfind` will try to locate a
  project file in the current directory.

  If a project file is either specified or found by the tools, then the content
  of the source directory and object directory lines are added as if they
  had been specified respectively by :samp:`-aI` and
  :samp:`-aO`.


.. index:: -r (gnatfind)

:samp:`r`
  By default, `gnatfind` will output only the information about the
  declaration, body or type completion of the entities. If this switch is
  set, the `gnatfind` will locate every reference to the entities in
  the files specified on the command line (or in every file in the search
  path if no file is given on the command line).


.. index:: -s (gnatfind)

:samp:`s`
  If this switch is set, then `gnatfind` will output the content
  of the Ada source file lines were the entity was found.


.. index:: -t (gnatfind)

:samp:`t`
  If this switch is set, then `gnatfind` will output the type hierarchy for
  the specified type. It act like -d option but recursively from parent
  type to parent type. When this switch is set it is not possible to
  specify more than one file.


All these switches may be in any order on the command line, and may even
appear after the file names. They need not be separated by spaces, thus
you can say :samp:`gnatxref -ag` instead of
:samp:`gnatxref -a -g`.

As stated previously, gnatfind will search in every directory in the
search path. You can force it to look only in the current directory if
you specify `*` at the end of the command line.

.. _Project_Files_for_gnatxref_and_gnatfind:

Project Files for *gnatxref* and *gnatfind*
-------------------------------------------

Project files allow a programmer to specify how to compile its
application, where to find sources, etc.  These files are used
primarily by GPS, but they can also be used
by the two tools `gnatxref` and `gnatfind`.

A project file name must end with :file:`.gpr`. If a single one is
present in the current directory, then `gnatxref` and `gnatfind` will
extract the information from it. If multiple project files are found, none of
them is read, and you have to use the :samp:`-p` switch to specify the one
you want to use.

The following lines can be included, even though most of them have default
values which can be used in most cases.
The lines can be entered in any order in the file.
Except for :file:`src_dir` and :file:`obj_dir`, you can only have one instance of
each line. If you have multiple instances, only the last one is taken into
account.

* *src_dir=DIR*
    [default: `"./"`].
    Specifies a directory where to look for source files. Multiple `src_dir`
    lines can be specified and they will be searched in the order they
    are specified.


* *obj_dir=DIR*
    [default: `"./"`].
    Specifies a directory where to look for object and library files. Multiple
    `obj_dir` lines can be specified, and they will be searched in the order
    they are specified


* *comp_opt=SWITCHES*
    [default: `""`].
    Creates a variable which can be referred to subsequently by using
    the `${comp_opt}` notation. This is intended to store the default
    switches given to *gnatmake* and *gcc*.


* *bind_opt=SWITCHES*
    [default: `""`].
    Creates a variable which can be referred to subsequently by using
    the :samp:`${bind_opt}` notation. This is intended to store the default
    switches given to *gnatbind*.


* *link_opt=SWITCHES*
    [default: `""`].
    Creates a variable which can be referred to subsequently by using
    the :samp:`${link_opt}` notation. This is intended to store the default
    switches given to *gnatlink*.


* *main=EXECUTABLE*
    [default: `""`].
    Specifies the name of the executable for the application. This variable can
    be referred to in the following lines by using the :samp:`{${main}` notation.


* *comp_cmd=COMMAND*
    [default: `"gcc -c -I${src_dir} -g -gnatq"`].
    Specifies the command used to compile a single file in the application.


* *make_cmd=COMMAND*
    [default: `"gnatmake ${main} -aI${src_dir}
    -aO${obj_dir} -g -gnatq -cargs ${comp_opt}
    -bargs ${bind_opt} -largs ${link_opt}"`].
    Specifies the command used to recompile the whole application.


* *run_cmd=COMMAND*
    [default: `"${main}"`].
    Specifies the command used to run the application.


* *debug_cmd=COMMAND*
    [default: `"gdb ${main}"`].
    Specifies the command used to debug the application


*gnatxref* and *gnatfind* only take into account the
`src_dir` and `obj_dir` lines, and ignore the others.


.. _Regular_Expressions_in_gnatfind_and_gnatxref:

Regular Expressions in `gnatfind` and `gnatxref`
------------------------------------------------

As specified in the section about *gnatfind*, the pattern can be a
regular expression. Two kinds of regular expressions
are recognized:

* *Globbing pattern*
    These are the most common regular expression. They are the same as are
    generally used in a Unix shell command line, or in a DOS session.

    Here is a more formal grammar:

    ::

        regexp ::= term
        term   ::= elmt            -- matches elmt
        term   ::= elmt elmt       -- concatenation (elmt then elmt)
        term   ::= *               -- any string of 0 or more characters
        term   ::= ?               -- matches any character
        term   ::= [char {char}]   -- matches any character listed
        term   ::= [char - char]   -- matches any character in range

* *Full regular expression*
    The second set of regular expressions is much more powerful. This is the
    type of regular expressions recognized by utilities such as :samp:`grep`.

    The following is the form of a regular expression, expressed in same BNF
    style as is found in the Ada Reference Manual:

    ::

        regexp ::= term {| term}   -- alternation (term or term ...)

        term ::= item {item}       -- concatenation (item then item)

        item ::= elmt              -- match elmt
        item ::= elmt *            -- zero or more elmt's
        item ::= elmt +            -- one or more elmt's
        item ::= elmt ?            -- matches elmt or nothing

        elmt ::= nschar            -- matches given character
        elmt ::= [nschar {nschar}]   -- matches any character listed
        elmt ::= [^ nschar {nschar}] -- matches any character not listed
        elmt ::= [char - char]     -- matches chars in given range
        elmt ::= \\ char            -- matches given character
        elmt ::= .                 -- matches any single character
        elmt ::= ( regexp )        -- parens used for grouping

        char ::= any character, including special characters
        nschar ::= any character except ()[].*+?^

    Here are a few examples:

      ``abcde|fghi``
          will match any of the two strings :samp:`abcde` and :samp:`fghi`,

      ``abc*d``
          will match any string like ``abd``, ``abcd``, ``abccd``,
          ``abcccd``, and so on,

      ``[a-z]+``
          will match any string which has only lowercase characters in it (and at
          least one character.


.. _Examples_of_gnatxref_Usage:

Examples of `gnatxref` Usage
----------------------------

General Usage
^^^^^^^^^^^^^

For the following examples, we will consider the following units:

  .. code-block:: ada

     main.ads:
     1: with Bar;
     2: package Main is
     3:     procedure Foo (B : in Integer);
     4:     C : Integer;
     5: private
     6:     D : Integer;
     7: end Main;

     main.adb:
     1: package body Main is
     2:     procedure Foo (B : in Integer) is
     3:     begin
     4:        C := B;
     5:        D := B;
     6:        Bar.Print (B);
     7:        Bar.Print (C);
     8:     end Foo;
     9: end Main;

     bar.ads:
     1: package Bar is
     2:     procedure Print (B : Integer);
     3: end bar;

The first thing to do is to recompile your application (for instance, in
that case just by doing a ``gnatmake main``, so that GNAT generates
the cross-referencing information.
You can then issue any of the following commands:

  * ``gnatxref main.adb``
    `gnatxref` generates cross-reference information for main.adb
    and every unit 'with'ed by main.adb.

    The output would be:

      ::

          B                                                      Type: Integer
            Decl: bar.ads           2:22
          B                                                      Type: Integer
            Decl: main.ads          3:20
            Body: main.adb          2:20
            Ref:  main.adb          4:13     5:13     6:19
          Bar                                                    Type: Unit
            Decl: bar.ads           1:9
            Ref:  main.adb          6:8      7:8
                 main.ads           1:6
          C                                                      Type: Integer
            Decl: main.ads          4:5
            Modi: main.adb          4:8
            Ref:  main.adb          7:19
          D                                                      Type: Integer
            Decl: main.ads          6:5
            Modi: main.adb          5:8
          Foo                                                    Type: Unit
            Decl: main.ads          3:15
            Body: main.adb          2:15
          Main                                                    Type: Unit
            Decl: main.ads          2:9
            Body: main.adb          1:14
          Print                                                   Type: Unit
            Decl: bar.ads           2:15
            Ref:  main.adb          6:12     7:12


    This shows that the entity `Main` is declared in main.ads, line 2, column 9,
    its body is in main.adb, line 1, column 14 and is not referenced any where.

    The entity `Print` is declared in bar.ads, line 2, column 15 and it
    is referenced in main.adb, line 6 column 12 and line 7 column 12.


  * ``gnatxref package1.adb package2.ads``
    `gnatxref` will generates cross-reference information for
    package1.adb, package2.ads and any other package 'with'ed by any
    of these.


Using gnatxref with vi
^^^^^^^^^^^^^^^^^^^^^^

`gnatxref` can generate a tags file output, which can be used
directly from *vi*. Note that the standard version of *vi*
will not work properly with overloaded symbols. Consider using another
free implementation of *vi*, such as *vim*.

  ::

     $ gnatxref -v gnatfind.adb > tags


The following command will generate the tags file for `gnatfind` itself
(if the sources are in the search path!):

  ::

     $ gnatxref -v gnatfind.adb > tags

From *vi*, you can then use the command :samp:`:tag {entity}`
(replacing `entity` by whatever you are looking for), and vi will
display a new file with the corresponding declaration of entity.


.. _Examples_of_gnatfind_Usage:

Examples of `gnatfind` Usage
----------------------------

* ``gnatfind -f xyz:main.adb``
  Find declarations for all entities xyz referenced at least once in
  main.adb. The references are search in every library file in the search
  path.

  The directories will be printed as well (as the ``-f``
  switch is set)

  The output will look like:

    ::

       directory/main.ads:106:14: xyz <= declaration
       directory/main.adb:24:10: xyz <= body
       directory/foo.ads:45:23: xyz <= declaration

  I.e., one of the entities xyz found in main.adb is declared at
  line 12 of main.ads (and its body is in main.adb), and another one is
  declared at line 45 of foo.ads

* ``gnatfind -fs xyz:main.adb``
  This is the same command as the previous one, but `gnatfind` will
  display the content of the Ada source file lines.

  The output will look like:

  ::

      directory/main.ads:106:14: xyz <= declaration
         procedure xyz;
      directory/main.adb:24:10: xyz <= body
         procedure xyz is
      directory/foo.ads:45:23: xyz <= declaration
         xyz : Integer;

  This can make it easier to find exactly the location your are looking
  for.


* ``gnatfind -r "*x*":main.ads:123 foo.adb``
  Find references to all entities containing an x that are
  referenced on line 123 of main.ads.
  The references will be searched only in main.ads and foo.adb.


* ``gnatfind main.ads:123``
  Find declarations and bodies for all entities that are referenced on
  line 123 of main.ads.

  This is the same as ``gnatfind "*":main.adb:123```

* ``gnatfind mydir/main.adb:123:45``
  Find the declaration for the entity referenced at column 45 in
  line 123 of file main.adb in directory mydir. Note that it
  is usual to omit the identifier name when the column is given,
  since the column position identifies a unique reference.

  The column has to be the beginning of the identifier, and should not
  point to any character in the middle of the identifier.


.. _The_Ada_to_HTML_Converter_gnathtml:

The Ada to HTML Converter `gnathtml`
====================================

.. index:: ! gnathtml

*gnathtml* is a Perl script that allows Ada source files to be browsed using
standard Web browsers. For installation information, see :ref:`Installing_gnathtml`.

Ada reserved keywords are highlighted in a bold font and Ada comments in
a blue font. Unless your program was compiled with the gcc *-gnatx*
switch to suppress the generation of cross-referencing information, user
defined variables and types will appear in a different color; you will
be able to click on any identifier and go to its declaration.

.. _Invoking_gnathtml:

Invoking *gnathtml*
-------------------

The command line is as follows:

  ::

      $ perl gnathtml.pl [`switches`] `ada-files`

You can specify as many Ada files as you want. `gnathtml` will generate
an html file for every ada file, and a global file called :file:`index.htm`.
This file is an index of every identifier defined in the files.

The following switches are available:

.. index:: -83 (gnathtml)

:samp:`83`
  Only the Ada 83 subset of keywords will be highlighted.

.. index:: -cc (gnathtml)

:samp:`cc {color}`
  This option allows you to change the color used for comments. The default
  value is green. The color argument can be any name accepted by html.

.. index:: -d (gnathtml)

:samp:`d`
  If the Ada files depend on some other files (for instance through
  `with` clauses, the latter files will also be converted to html.
  Only the files in the user project will be converted to html, not the files
  in the run-time library itself.

.. index:: -D (gnathtml)

:samp:`D`
  This command is the same as *-d* above, but *gnathtml* will
  also look for files in the run-time library, and generate html files for them.

.. index:: -ext (gnathtml)

:samp:`ext {extension}`
  This option allows you to change the extension of the generated HTML files.
  If you do not specify an extension, it will default to :file:`htm`.

.. index:: -f (gnathtml)

:samp:`f`
  By default, gnathtml will generate html links only for global entities
  ('with'ed units, global variables and types,...).  If you specify
  *-f* on the command line, then links will be generated for local
  entities too.

.. index:: -l (gnathtml)

:samp:`l {number}`
  If this switch is provided and `number` is not 0, then
  `gnathtml` will number the html files every `number` line.

.. index:: -I (gnathtml)

:samp:`I {dir}`
  Specify a directory to search for library files (:file:`.ALI` files) and
  source files. You can provide several -I switches on the command line,
  and the directories will be parsed in the order of the command line.

.. index:: -o (gnathtml)

:samp:`o {dir}`
  Specify the output directory for html files. By default, gnathtml will
  saved the generated html files in a subdirectory named :file:`html/`.

.. index:: -p (gnathtml)

:samp:`p {file}`
  If you are using Emacs and the most recent Emacs Ada mode, which provides
  a full Integrated Development Environment for compiling, checking,
  running and debugging applications, you may use :file:`.gpr` files
  to give the directories where Emacs can find sources and object files.

  Using this switch, you can tell gnathtml to use these files.
  This allows you to get an html version of your application, even if it
  is spread over multiple directories.

.. index:: -sc (gnathtml)

:samp:`sc {color}`
  This switch allows you to change the color used for symbol
  definitions.
  The default value is red. The color argument can be any name accepted by html.

.. index:: -t (gnathtml)

:samp:`t {file}`
  This switch provides the name of a file. This file contains a list of
  file names to be converted, and the effect is exactly as though they had
  appeared explicitly on the command line. This
  is the recommended way to work around the command line length limit on some
  systems.

.. _Installing_gnathtml:

Installing `gnathtml`
---------------------

`Perl` needs to be installed on your machine to run this script.
`Perl` is freely available for almost every architecture and
operating system via the Internet.

On Unix systems, you  may want to modify  the  first line of  the script
`gnathtml`,  to explicitly  specify  where Perl
is located. The syntax of this line is:

  ::

     #!full_path_name_to_perl

Alternatively, you may run the script using the following command line:

  ::

     $ perl gnathtml.pl [`switches`] `files`




.. -- +---------------------------------------------------------------------+
.. -- | The following sections are present only in the PRO and GPL editions |
.. -- +---------------------------------------------------------------------+

.. only:: PRO or GPL

  .. _The_Ada-to-XML_converter_gnat2xml:

  The Ada-to-XML converter *gnat2xml*
  ===================================

  .. index:: ! gnat2xml
  .. index:: XML generation

  The *gnat2xml* tool is an ASIS-based utility that converts
  Ada source code into XML.

  *gnat2xml* is a project-aware tool
  (see :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
  the project-related switches).  The project file package that can specify
  *gnat2xml* switches is named ``gnat2xml``.

  .. _Switches_for_*gnat2xml*:

  Switches for *gnat2xml*
  -----------------------

  *gnat2xml* takes Ada source code as input, and produces XML
  that conforms to the schema.

  Usage:

    ::

       $ gnat2xml [options] filenames [-files filename] [-cargs gcc_switches]

  Options:

     :samp:`--help`
          Generate usage information and quit, ignoring all other options

     :samp:`-h`
          Same as ``--help``

     :samp:`--version`
          Print version and quit, ignoring all other options

     :samp:`-P{file}`
          indicates the name of the project file that describes
          the set of sources to be processed. The exact set of argument
          sources depends on other options specified, see below.

     :samp:`-U`
          If a project file is specified and no argument source is explicitly
          specified, process all the units of the closure of the argument project.
          Otherwise this option has no effect.

     :samp:`-U {main_unit}`
          If a project file is specified and no argument source
          is explicitly specified (either directly or by means of *-files*
          option), process the closure of units rooted at `main_unit`.
          Otherwise this option has no effect.

     :samp:`-X{name}={value}`
          Indicates that external variable `name` in
          the argument project has the value `value`. Has no effect if no
          project is specified as tool argument.

     :samp:`--RTS={rts-path}`
          Specifies the default location of the runtime
          library. Same meaning as the equivalent *gnatmake* flag
          (:ref:`Switches_for_gnatmake`).

     :samp:`--incremental`
          Incremental processing on a per-file basis. Source files are
          only processed if they have been modified, or if files they depend
          on have been modified. This is similar to the way gnatmake/gprbuild
          only compiles files that need to be recompiled. A project file
          is required in this mode.

     :samp:`-j{n}`
           In *--incremental* mode, use `n` *gnat2xml*
           processes to perform XML generation in parallel. If `n` is 0, then
           the maximum number of parallel tree creations is the number of core
           processors on the platform.

     :samp:`--output-dir={dir}`
          Generate one .xml file for each Ada source file, in
          directory :file:`dir`. (Default is to generate the XML to standard
          output.)

     :samp:`-I{include-dir}`
          Directories to search for dependencies.
          You can also set the ADA_INCLUDE_PATH environment variable for this.

     :samp:`--compact`
          Debugging version, with interspersed source, and a more
          compact representation of "sloc". This version does not conform
          to any schema.

     :samp:`--rep-clauses`
          generate representation clauses (see :ref:`Generating_Representation_Clauses`).

     :samp:`-files={filename}`
         The name of a text file containing a list of Ada source files to process

     :samp:`-q`
         Quiet

     :samp:`-v`
         Verbose

     :samp:`-cargs` ...
         Options to pass to gcc

  If a project file is specified and no argument source is explicitly
  specified, and no *-U* is specified, then the set of processed
  sources is all the immediate units of the argument project.

  Example:

    ::

       $ gnat2xml -v -output-dir=xml-files *.ad[sb]

  The above will create \*.xml files in the :file:`xml-files` subdirectory.
  For example, if there is an Ada package Mumble.Dumble, whose spec and
  body source code lives in mumble-dumble.ads and mumble-dumble.adb,
  the above will produce xml-files/mumble-dumble.ads.xml and
  xml-files/mumble-dumble.adb.xml.

  .. _Other_Programs:

  Other Programs
  --------------

  The distribution includes two other programs that are related to
  *gnat2xml*:

  *gnat2xsd* is the schema generator, which generates the schema
  to standard output, based on the structure of Ada as encoded by
  ASIS. You don't need to run *gnat2xsd* in order to use
  *gnat2xml*. To generate the schema, type:


    ::

        $ gnat2xsd > ada-schema.xsd


  *gnat2xml* generates XML files that will validate against
  :file:`ada-schema.xsd`.

  *xml2gnat* is a back-translator that translates the XML back
  into Ada source code. The Ada generated by *xml2gnat* has
  identical semantics to the original Ada code passed to
  *gnat2xml*. It is not textually identical, however --- for
  example, no attempt is made to preserve the original indentation.

  .. _Structure_of_the_XML:

  Structure of the XML
  --------------------

  The primary documentation for the structure of the XML generated by
  *gnat2xml* is the schema (see *gnat2xsd* above). The
  following documentation gives additional details needed to understand
  the schema and therefore the XML.

  The elements listed under Defining Occurrences, Usage Occurrences, and
  Other Elements represent the syntactic structure of the Ada program.
  Element names are given in lower case, with the corresponding element
  type Capitalized_Like_This. The element and element type names are
  derived directly from the ASIS enumeration type Flat_Element_Kinds,
  declared in Asis.Extensions.Flat_Kinds, with the leading ``An_`` or ``A_``
  removed. For example, the ASIS enumeration literal
  An_Assignment_Statement corresponds to the XML element
  assignment_statement of XML type Assignment_Statement.

  To understand the details of the schema and the corresponding XML, it is
  necessary to understand the ASIS standard, as well as the GNAT-specific
  extension to ASIS.

  A defining occurrence is an identifier (or character literal or operator
  symbol) declared by a declaration. A usage occurrence is an identifier
  (or ...) that references such a declared entity. For example, in:


    .. code-block:: ada

       type T is range 1..10;
       X, Y : constant T := 1;


  The first 'T' is the defining occurrence of a type. The 'X' is the
  defining occurrence of a constant, as is the 'Y', and the second 'T' is
  a usage occurrence referring to the defining occurrence of T.

  Each element has a 'sloc' (source location), and subelements for each
  syntactic subtree, reflecting the Ada grammar as implemented by ASIS.
  The types of subelements are as defined in the ASIS standard. For
  example, for the right-hand side of an assignment_statement we have
  the following comment in asis-statements.ads:

    .. code-block:: ada

        ------------------------------------------------------------------------------
        --  18.3  function Assignment_Expression
        ------------------------------------------------------------------------------

           function Assignment_Expression
             (Statement : Asis.Statement)
              return      Asis.Expression;

        ------------------------------------------------------------------------------
        ...
        --  Returns the expression from the right hand side of the assignment.
        ...
        --  Returns Element_Kinds:
        --       An_Expression


  The corresponding sub-element of type Assignment_Statement is:

    ::

        <xsd:element name="assignment_expression_q" type="Expression_Class"/>

  where Expression_Class is defined by an xsd:choice of all the
  various kinds of expression.

  The 'sloc' of each element indicates the starting and ending line and
  column numbers. Column numbers are character counts; that is, a tab
  counts as 1, not as however many spaces it might expand to.

  Subelements of type Element have names ending in '_q' (for ASIS
  "Query"), and those of type Element_List end in '_ql'
  ("Query returning  List").

  Some subelements are 'Boolean'. For example, Private_Type_Definition
  has has_abstract_q and has_limited_q, to indicate whether those
  keywords are present, as in `type T is abstract limited private;`.
  False is represented by a Nil_Element. True is represented
  by an element type specific to that query (for example, Abstract and
  Limited).

  The root of the tree is a Compilation_Unit, with attributes:

  * unit_kind, unit_class, and unit_origin. These are strings that match the
    enumeration literals of types Unit_Kinds, Unit_Classes, and Unit_Origins
    in package Asis.

  * unit_full_name is the full expanded name of the unit, starting from a
    root library unit. So for `package P.Q.R is ...`,
    `unit_full_name="P.Q.R"`. Same for `separate (P.Q) package R is ...`.

  * def_name is the same as unit_full_name for library units; for subunits,
    it is just the simple name.

  * source_file is the name of the Ada source file. For example, for
    the spec of `P.Q.R`, `source_file="p-q-r.ads"`. This allows one to
    interpret the source locations --- the 'sloc' of all elements
    within this Compilation_Unit refers to line and column numbers
    within the named file.

  Defining occurrences have these attributes:

  * def_name is the simple name of the declared entity, as written in the Ada
    source code.

  * def is a unique URI of the form:

    ::

        ada://kind/fully/qualified/name

    where:

    * kind indicates the kind of Ada entity being declared (see below), and

    * fully/qualified/name, is the fully qualified name of the Ada
      entity, with each of 'fully', 'qualified', and 'name' being
      mangled for uniqueness. We do not document the mangling
      algorithm, which is subject to change; we just guarantee that the
      names are unique in the face of overloading.

    * type is the type of the declared object, or `null` for
      declarations of things other than objects.

  Usage occurrences have these attributes:

  * ref_name is the same as the def_name of the corresponding defining
    occurrence. This attribute is not of much use, because of
    overloading; use ref for lookups, instead.

  * ref is the same as the def of the corresponding defining
    occurrence.

  In summary, `def_name` and `ref_name` are as in the source
  code of the declaration, possibly overloaded, whereas `def` and
  `ref` are unique-ified.

  Literal elements have this attribute:

  * lit_val is the value of the literal as written in the source text,
    appropriately escaped (e.g. `"` ---> `&quot;`). This applies
    only to numeric and string literals. Enumeration literals in Ada are
    not really "literals" in the usual sense; they are usage occurrences,
    and have ref_name and ref as described above. Note also that string
    literals used as operator symbols are treated as defining or usage
    occurrences, not as literals.

  Elements that can syntactically represent names and expressions (which
  includes usage occurrences, plus function calls and so forth) have this
  attribute:

  * type. If the element represents an expression or the name of an object,
    'type' is the 'def' for the defining occurrence of the type of that
    expression or name. Names of other kinds of entities, such as package
    names and type names, do not have a type in Ada; these have type="null"
    in the XML.

  Pragma elements have this attribute:

  *  pragma_name is the name of the pragma. For language-defined pragmas, the
     pragma name is redundant with the element kind (for example, an
     assert_pragma element necessarily has pragma_name="Assert"). However, all
     implementation-defined pragmas are lumped together in ASIS as a single
     element kind (for example, the GNAT-specific pragma Unreferenced is
     represented by an implementation_defined_pragma element with
     pragma_name="Unreferenced").

  Defining occurrences of formal parameters and generic formal objects have this
  attribute:

  * mode indicates that the parameter is of mode 'in', 'in out', or 'out'.

  All elements other than Not_An_Element have this attribute:

  * checks is a comma-separated list of run-time checks that are needed
    for that element. The possible checks are: do_accessibility_check,
    do_discriminant_check,do_division_check,do_length_check,
    do_overflow_check,do_range_check,do_storage_check,do_tag_check.

  The "kind" part of the "def" and "ref" attributes is taken from the ASIS
  enumeration type Flat_Declaration_Kinds, declared in
  Asis.Extensions.Flat_Kinds, with the leading ``An_`` or ``A_`` removed, and
  any trailing ``_Declaration`` or ``_Specification`` removed. Thus, the
  possible kinds are as follows:

    ::

        ordinary_type
        task_type
        protected_type
        incomplete_type
        tagged_incomplete_type
        private_type
        private_extension
        subtype
        variable
        constant
        deferred_constant
        single_task
        single_protected
        integer_number
        real_number
        enumeration_literal
        discriminant
        component
        loop_parameter
        generalized_iterator
        element_iterator
        procedure
        function
        parameter
        procedure_body
        function_body
        return_variable
        return_constant
        null_procedure
        expression_function
        package
        package_body
        object_renaming
        exception_renaming
        package_renaming
        procedure_renaming
        function_renaming
        generic_package_renaming
        generic_procedure_renaming
        generic_function_renaming
        task_body
        protected_body
        entry
        entry_body
        entry_index
        procedure_body_stub
        function_body_stub
        package_body_stub
        task_body_stub
        protected_body_stub
        exception
        choice_parameter
        generic_procedure
        generic_function
        generic_package
        package_instantiation
        procedure_instantiation
        function_instantiation
        formal_object
        formal_type
        formal_incomplete_type
        formal_procedure
        formal_function
        formal_package
        formal_package_declaration_with_box

  .. _Generating_Representation_Clauses:

  Generating Representation Clauses
  ---------------------------------

  If the *--rep-clauses* switch is given, *gnat2xml* will
  generate representation clauses for certain types showing the
  representation chosen by the compiler. The information is produced by
  the ASIS 'Data Decomposition' facility --- see the
  `Asis.Data_Decomposition` package for details.

  Not all types are supported. For example, `Type_Model_Kind` must
  be `A_Simple_Static_Model`. Types declared within generic units
  have no representation. The clauses that are generated include
  `attribute_definition_clauses` for `Size` and
  `Component_Size`, as well as
  `record_representation_clauses`.

  There is no guarantee that the generated representation clauses could
  have actually come from legal Ada code; Ada has some restrictions that
  are not necessarily obeyed by the generated clauses.

  The representation clauses are surrounded by comment elements to
  indicate that they are automatically generated, something like this:

    ::

        <comment text="--gen+">
        ...
        <attribute_definition_clause>
        ...
        <comment text="--gen-">
        ...


.. only:: PRO or GPL

  .. _The_Program_Property_Verifier_gnatcheck:

  The Program Property Verifier *gnatcheck*
  =========================================

  .. index:: ! gnatcheck
  .. index:: ASIS

  The *gnatcheck* tool is an ASIS-based utility that checks properties
  of Ada source files according to a given set of semantic rules.

  *gnatcheck* is a project-aware tool
  (see :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
  the project-related switches).  The project file package that can specify
  *gnatcheck* switches is named ``Check``.

  For full details, plese refer to :title:`GNATcheck Reference Manual`.



.. only:: PRO or GPL

  .. _The_GNAT_Metrics_Tool_gnatmetric:

  The GNAT Metrics Tool *gnatmetric*
  ==================================

  .. index:: ! gnatmetric
  .. index:: Metric tool

  The *gnatmetric* tool is an ASIS-based utility
  for computing various program metrics.
  It takes an Ada source file as input and generates a file containing the
  metrics data as output. Various switches control which
  metrics are computed and output.

  *gnatmetric* is a project-aware tool
  (see :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
  the project-related switches).  The project file package that can specify
  *gnatmetric* switches is named ``Metrics``.

  To compute program metrics, *gnatmetric* invokes the Ada
  compiler and generates and uses the ASIS tree for the input source;
  thus the input must be legal Ada code, and the tool should have all the
  information needed to compile the input source. To provide this information,
  you may specify as a tool parameter the project file the input source belongs to
  (or you may call *gnatmetric*
  through the *gnat* driver (see :ref:`The_GNAT_Driver_and_Project_Files`).
  Another possibility is to specify the source search
  path and needed configuration files in *-cargs* section of *gnatmetric*
  call, see the description of the *gnatmetric* switches below.

  If the set of sources to be processed by `gnatmetric` contains sources with
  preprocessing directives
  then the needed options should be provided to run preprocessor as a part of
  the *gnatmetric* call, and the computed metrics
  will correspond to preprocessed sources.

  The *gnatmetric* command has the form

    ::

       $ gnatmetric [`switches`] {`filename`} [-cargs `gcc_switches`]

  where:

  * `switches` specify the metrics to compute and define the destination for
    the output

  * Each `filename` is the name (including the extension) of a source
    file to process. 'Wildcards' are allowed, and
    the file name may contain path information.
    If no `filename` is supplied, then the `switches` list must contain
    at least one
    *-files* switch (see :ref:`Other_gnatmetric_Switches`).
    Including both a *-files* switch and one or more
    `filename` arguments is permitted.

  * `gcc_switches` is a list of switches for
    *gcc*. They will be passed on to all compiler invocations made by
    *gnatmetric* to generate the ASIS trees. Here you can provide
    *-I* switches to form the source search path,
    and use the *-gnatec* switch to set the configuration file,
    use the *-gnat05* switch if sources should be compiled in
    Ada 2005 mode etc.

  The following subsections describe the various switches accepted by
  *gnatmetric*, organized by category.

  .. _Output_File_Control-gnatmetric:

  Output File Control
  -------------------

  .. index:: Output file control in gnatmetric

  *gnatmetric* has two output formats. It can generate a
  textual (human-readable) form, and also XML. By default only textual
  output is generated.

  When generating the output in textual form, *gnatmetric* creates
  for each Ada source file a corresponding text file
  containing the computed metrics, except for the case when the set of metrics
  specified by gnatmetric parameters consists only of metrics that are computed
  for the whole set of analyzed sources, but not for each Ada source.
  By default, the name of the file containing metric information for a source
  is obtained by appending the :file:`.metrix` suffix to the
  name of the input source file. If not otherwise specified and no project file
  is specified as *gnatmetric* option this file is placed in the same
  directory as where the source file is located. If *gnatmetric* has a
  project  file as its parameter, it places all the generated files in the
  object directory of the project (or in the project source directory if the
  project does not define an objects directory), if *--subdirs* option
  is specified, the files are placed in the subrirectory of this directory
  specified by this option.

  All the output information generated in XML format is placed in a single
  file. By default the name of this file is :file:`metrix.xml`.
  If not otherwise specified and if no project file is specified
  as *gnatmetric* option  this file is placed in the
  current directory.

  Some of the computed metrics are summed over the units passed to
  *gnatmetric*; for example, the total number of lines of code.
  By default this information is sent to :file:`stdout`, but a file
  can be specified with the *-og* switch.

  The following switches control the *gnatmetric* output:

  .. index:: -x (gnatmetric)

  :samp:`-x`
    Generate the XML output

  .. index:: -xs (gnatmetric)

  :samp:`-xs`
    Generate the XML output and the XML schema file that describes the structure
    of the XML metric report, this schema is assigned to the XML file. The schema
    file has the same name as the XML output file with :file:`.xml` suffix replaced
    with :file:`.xsd`

  .. index:: -nt (gnatmetric)


  :samp:`-nt`
    Do not generate the output in text form (implies *-x*)

  .. index:: -d (gnatmetric)


  :samp:`-d {output_dir}`
    Put text files with detailed metrics into `output_dir`

  .. index:: -o (gnatmetric)


  :samp:`-o {file_suffix}`
    Use `file_suffix`, instead of :file:`.metrix`
    in the name of the output file.

  .. index:: -og (gnatmetric)


  :samp:`-og {file_name}`
    Put global metrics into `file_name`

  .. index:: -ox (gnatmetric)


  :samp:`-ox {file_name}`
    Put the XML output into `file_name` (also implies *-x*)

  .. index:: -sfn (gnatmetric)


  :samp:`-sfn`
    Use 'short' source file names in the output.  (The *gnatmetric*
    output includes the name(s) of the Ada source file(s) from which the metrics
    are computed.  By default each name includes the absolute path. The
    *-sfn* switch causes *gnatmetric*
    to exclude all directory information from the file names that are output.)


  .. index:: Disable Metrics For Local Units in gnatmetric

  .. _Disable_Metrics_For_Local_Units:

  Disable Metrics For Local Units
  -------------------------------

  *gnatmetric* relies on the GNAT compilation model --
  one compilation
  unit per one source file. It computes line metrics for the whole source
  file, and it also computes syntax
  and complexity metrics for the file's outermost unit.

  By default, *gnatmetric* will also compute all metrics for certain
  kinds of locally declared program units:

  * subprogram (and generic subprogram) bodies;

  * package (and generic package) specs and bodies;

  * task object and type specifications and bodies;

  * protected object and type specifications and bodies.

  .. index:: Eligible local unit (for gnatmetric)

  These kinds of entities will be referred to as
  *eligible local program units*, or simply *eligible local units*,
  in the discussion below.

  Note that a subprogram declaration, generic instantiation,
  or renaming declaration only receives metrics
  computation when it appear as the outermost entity
  in a source file.

  Suppression of metrics computation for eligible local units can be
  obtained via the following switch:


  .. index:: -nolocal (gnatmetric)


  :samp:`-nolocal`
    Do not compute detailed metrics for eligible local program units


  .. _Specifying_a_set_of_metrics_to_compute:

  Specifying a set of metrics to compute
  --------------------------------------

  By default all the metrics are computed and reported. The switches
  described in this subsection allow you to control, on an individual
  basis, whether metrics are computed and
  reported. If at least one positive metric
  switch is specified (that is, a switch that defines that a given
  metric or set of metrics is to be computed), then only
  explicitly specified metrics are reported.

  .. _Line_Metrics_Control:

  Line Metrics Control
  ^^^^^^^^^^^^^^^^^^^^

  .. index:: Line metrics control in gnatmetric

  For any (legal) source file, and for each of its
  eligible local program units, *gnatmetric* computes the following
  metrics:

  * the total number of lines;

  * the total number of code lines (i.e., non-blank lines that are not comments)

  * the number of comment lines

  * the number of code lines containing end-of-line comments;

  * the comment percentage: the ratio between the number of lines that contain
    comments and the number of all non-blank lines, expressed as a percentage;

  * the number of empty lines and lines containing only space characters and/or
    format effectors (blank lines)

  * the average number of code lines in subprogram bodies, task bodies, entry
    bodies and statement sequences in package bodies (this metric is only computed
    across the whole set of the analyzed units)

  *gnatmetric* sums the values of the line metrics for all the
  files being processed and then generates the cumulative results. The tool
  also computes for all the files being processed the average number of code
  lines in bodies.

  You can use the following switches to select the specific line metrics
  to be computed and reported.


  .. index:: --lines (gnatmetric)
  .. index:: --no-lines (gnatmetric)


  :samp:`--lines-all`
    Report all the line metrics


  :samp:`--no-lines-all`
    Do not report any of line metrics


  :samp:`--lines`
    Report the number of all lines


  :samp:`--no-lines`
    Do not report the number of all lines


  :samp:`--lines-code`
    Report the number of code lines


  :samp:`--no-lines-code`
    Do not report the number of code lines


  :samp:`--lines-comment`
    Report the number of comment lines


  :samp:`--no-lines-comment`
    Do not report the number of comment lines


  :samp:`--lines-eol-comment`
    Report the number of code lines containing
    end-of-line comments


  :samp:`--no-lines-eol-comment`
    Do not report the number of code lines containing
    end-of-line comments


  :samp:`--lines-ratio`
    Report the comment percentage in the program text


  :samp:`--no-lines-ratio`
    Do not report the comment percentage in the program text


  :samp:`--lines-blank`
    Report the number of blank lines


  :samp:`--no-lines-blank`
    Do not report the number of blank lines


  :samp:`--lines-average`
    Report the average number of code lines in subprogram bodies, task bodies,
    entry bodies and statement sequences in package bodies. The metric is computed
    and reported for the whole set of processed Ada sources only.


  :samp:`--no-lines-average`
    Do not report the average number of code lines in subprogram bodies,
    task bodies, entry bodies and statement sequences in package bodies.


  .. _Syntax_Metrics_Control:

  Syntax Metrics Control
  ^^^^^^^^^^^^^^^^^^^^^^

  .. index:: Syntax metrics control in gnatmetric

  *gnatmetric* computes various syntactic metrics for the
  outermost unit and for each eligible local unit:

  * *LSLOC ('Logical Source Lines Of Code')*
      The total number of declarations and the total number of statements. Note
      that the definition of declarations is the one given in the reference
      manual:

        "Each of the following is defined to be a declaration: any basic_declaration;
        an enumeration_literal_specification; a discriminant_specification;
        a component_declaration; a loop_parameter_specification; a
        parameter_specification; a subprogram_body; an entry_declaration;
        an entry_index_specification; a choice_parameter_specification;
        a generic_formal_parameter_declaration."

      This means for example that each enumeration literal adds one to the count,
      as well as each subprogram parameter.

      Thus the results from this metric will be significantly greater than might
      be expected from a naive view of counting semicolons.

  * *Maximal static nesting level of inner program units*
      According to :title:`Ada Reference Manual`, 10.1(1):

        "A program unit is either a package, a task unit, a protected unit, a
        protected entry, a generic unit, or an explicitly declared subprogram other
        than an enumeration literal."

  * *Maximal nesting level of composite syntactic constructs*
      This corresponds to the notion of the
      maximum nesting level in the GNAT built-in style checks
      (see :ref:`Style_Checking`)

  * *Number of formal parameters*
      Number of formal parameters of a subprogram; if a subprogram does have
      parameters, then numbers of "in", "out" and "in out" parameters are also
      reported. This metric is reported for subprogram specifications and for
      subprogram instantiations. For subprogram bodies, expression functions
      and null procedures this metric is reported if the construct acts as a
      subprogram declaration but is not a completion of previous declaration.
      This metric is not reported for generic and formal subprograms.

  For the outermost unit in the file, *gnatmetric* additionally computes
  the following metrics:

  * *Public subprograms*
      This metric is computed for package specs. It is the
      number of subprograms and generic subprograms declared in the visible
      part (including the visible part of nested packages, protected objects, and
      protected types).


  * *All subprograms*
      This metric is computed for bodies and subunits. The
      metric is equal to a total number of subprogram bodies in the compilation
      unit.
      Neither generic instantiations nor renamings-as-a-body nor body stubs
      are counted. Any subprogram body is counted, independently of its nesting
      level and enclosing constructs. Generic bodies and bodies of protected
      subprograms are counted in the same way as 'usual' subprogram bodies.


  * *Public types*
      This metric is computed for package specs and
      generic package declarations. It is the total number of types
      that can be referenced from outside this compilation unit, plus the
      number of types from all the visible parts of all the visible generic
      packages. Generic formal types are not counted.  Only types, not subtypes,
      are included.

      Along with the total number of public types, the following
      types are counted and reported separately:

      * *Abstract types*

      * *Root tagged types^ (abstract, non-abstract, private, non-private). Type
        extensions are *not* counted

      * *Private types* (including private extensions)

      * *Task types*

      * *Protected types*

  * *All types*
      This metric is computed for any compilation unit. It is equal to the total
      number of the declarations of different types given in the compilation unit.
      The private and the corresponding full type declaration are counted as one
      type declaration. Incomplete type declarations and generic formal types
      are not counted.
      No distinction is made among different kinds of types (abstract,
      private etc.); the total number of types is computed and reported.

  By default, all the syntax metrics are computed and reported. You can use the
  following switches to select specific syntax metrics.


  .. index:: --syntax (gnatmetric)
  .. index:: --no-syntax (gnatmetric)


  :samp:`--syntax-all`
    Report all the syntax metrics


  :samp:`--no-syntax-all`
    Do not report any of syntax metrics


  :samp:`--declarations`
    Report the total number of declarations


  :samp:`--no-declarations`
    Do not report the total number of declarations


  :samp:`--statements`
    Report the total number of statements


  :samp:`--no-statements`
    Do not report the total number of statements


  :samp:`--public-subprograms`
    Report the number of public subprograms in a compilation unit


  :samp:`--no-public-subprograms`
    Do not report the number of public subprograms in a compilation unit


  :samp:`--all-subprograms`
    Report the number of all the subprograms in a compilation unit


  :samp:`--no-all-subprograms`
    Do not report the number of all the subprograms in a compilation unit


  :samp:`--public-types`
    Report the number of public types in a compilation unit


  :samp:`--no-public-types`
    Do not report the number of public types in a compilation unit


  :samp:`--all-types`
    Report the number of all the types in a compilation unit


  :samp:`--no-all-types`
    Do not report the number of all the types in a compilation unit


  :samp:`--unit-nesting`
    Report the maximal program unit nesting level


  :samp:`--no-unit-nesting`
    Do not report the maximal program unit nesting level


  :samp:`--construct-nesting`
    Report the maximal construct nesting level


  :samp:`--no-construct-nesting`
    Do not report the maximal construct nesting level

  :samp:`--param-number`
    Report the number of subprogram parameters


  :samp:`--no-param-number`
    Do not report the number of subprogram parameters


  .. _Complexity_Metrics_Control:

  Complexity Metrics Control
  ^^^^^^^^^^^^^^^^^^^^^^^^^^

  .. index:: Complexity metrics control in gnatmetric

  For a program unit that is an executable body (a subprogram body (including
  generic bodies), task body, entry body or a package body containing
  its own statement sequence) *gnatmetric* computes the following
  complexity metrics:

  * McCabe cyclomatic complexity;

  * McCabe essential complexity;

  * maximal loop nesting level;

  * extra exit points (for subprograms);

  The McCabe cyclomatic complexity metric is defined
  in `http://www.mccabe.com/pdf/mccabe-nist235r.pdf <http://www.mccabe.com/pdf/mccabe-nist235r.pdf>`_

  According to McCabe, both control statements and short-circuit control forms
  should be taken into account when computing cyclomatic complexity.
  For Ada 2012 we have also take into account conditional expressions
  and quantified expressions. For each body, we compute three metric values:

  * the complexity introduced by control
    statements only, without taking into account short-circuit forms
    (referred as `statement complexity` in *gnatmetric* output),

  * the complexity introduced by short-circuit control forms only
    (referred as `expression complexity` in *gnatmetric* output), and

  * the total
    cyclomatic complexity, which is the sum of these two values
    (referred as `cyclomatic complexity` in *gnatmetric* output).

  The cyclomatic complexity is also computed for Ada 2012 expression functions.
  An expression function cannot have statements as its components, so only one
  metric value is computed as a cyclomatic complexity of an expression function.

  The origin of cyclomatic complexity metric is the need to estimate the number
  of independent paths in the control flow graph that in turn gives the number
  of tests needed to satisfy paths coverage testing completeness criterion.
  Considered from the testing point of view, a static Ada `loop` (that is,
  the `loop` statement having static subtype in loop parameter
  specification) does not add to cyclomatic complexity. By providing
  *--no-static-loop* option a user
  may specify that such loops should not be counted when computing the
  cyclomatic complexity metric

  The Ada essential complexity metric is a McCabe cyclomatic complexity metric
  counted for the code that is reduced by excluding all the pure structural Ada
  control statements. An compound statement is considered as a non-structural
  if it contains a `raise` or `return` statement as it subcomponent,
  or if it contains a `goto` statement that transfers the control outside
  the operator. A selective accept statement with `terminate` alternative
  is considered as non-structural statement. When computing this metric,
  `exit` statements are treated in the same way as `goto`
  statements unless *-ne* option is specified.

  The Ada essential complexity metric defined here is intended to quantify
  the extent to which the software is unstructured. It is adapted from
  the McCabe essential complexity metric defined in
  http://www.mccabe.com/pdf/mccabe-nist235r.pdf
  but is modified to be more
  suitable for typical Ada usage. For example, short circuit forms
  are not penalized as unstructured in the Ada essential complexity metric.

  When computing cyclomatic and essential complexity, *gnatmetric* skips
  the code in the exception handlers and in all the nested program units. The
  code of assertions and predicates (that is, subprogram preconditions and
  postconditions, subtype predicates and type invariants) is also skipped.

  By default, all the complexity metrics are computed and reported.
  For more fine-grained control you can use
  the following switches:


  .. index:: -complexity (gnatmetric)
  .. index:: --no-complexity (gnatmetric)


  :samp:`--complexity-all`
    Report all the complexity metrics


  :samp:`--no-complexity-all`
    Do not report any of complexity metrics


  :samp:`--complexity-cyclomatic`
    Report the McCabe Cyclomatic Complexity


  :samp:`--no-complexity-cyclomatic`
    Do not report the McCabe Cyclomatic Complexity


  :samp:`--complexity-essential`
    Report the Essential Complexity


  :samp:`--no-complexity-essential`
    Do not report the Essential Complexity


  :samp:`--loop-nesting`
    Report maximal loop nesting level


  :samp:`-no-loop-nesting`
    Do not report maximal loop nesting level


  :samp:`--complexity-average`
    Report the average McCabe Cyclomatic Complexity for all the subprogram bodies,
    task bodies, entry bodies and statement sequences in package bodies.
    The metric is computed and reported for whole set of processed Ada sources
    only.


  :samp:`--no-complexity-average`
    Do not report the average McCabe Cyclomatic Complexity for all the subprogram
    bodies, task bodies, entry bodies and statement sequences in package bodies

  .. index:: -ne (gnatmetric)


  :samp:`-ne`
    Do not consider `exit` statements as `goto`\ s when
    computing Essential Complexity

  .. index:: --no-static-loop (gnatmetric)


  :samp:`--no-static-loop`
    Do not consider static loops when computing cyclomatic complexity


  :samp:`--extra-exit-points`
    Report the extra exit points for subprogram bodies. As an exit point, this
    metric counts `return` statements and raise statements in case when the
    raised exception is not handled in the same body. In case of a function this
    metric subtracts 1 from the number of exit points, because a function body
    must contain at least one `return` statement.


  :samp:`--no-extra-exit-points`
    Do not report the extra exit points for subprogram bodies


  .. _Coupling_Metrics_Control:

  Coupling Metrics Control
  ^^^^^^^^^^^^^^^^^^^^^^^^

  .. index:: Coupling metrics control in gnatmetric

  .. index:: Coupling metrics (in gnatmetric)

  Coupling metrics measure the dependencies between a given entity and other
  entities in the program. This information is useful since high coupling
  may signal potential issues with maintainability as the program evolves.

  *gnatmetric* computes the following coupling metrics:


  * *object-oriented coupling*, for classes in traditional object-oriented
    sense;

  * *unit coupling*, for all the program units making up a program;

  * *control coupling*, reflecting dependencies between a unit and
    other units that contain subprograms.

  .. index:: fan-out coupling
  .. index:: efferent coupling

  Two kinds of coupling metrics are computed:

  * fan-out coupling ('efferent coupling'):
    the number of entities the given entity depends upon. This metric
    reflects how the given entity depends on the changes in the
    'external world'.

  .. index:: fan-in coupling
  .. index:: afferent coupling

  * fan-in coupling ('afferent' coupling):
    the number of entities that depend on a given entity.
    This metric reflects how the 'external world' depends on the changes in a
    given entity.

  Object-oriented coupling metrics measure the dependencies
  between a given class (or a group of classes) and the other classes in the
  program. In this subsection the term 'class' is used in its traditional
  object-oriented programming sense (an instantiable module that contains data
  and/or method members). A *category* (of classes) is a group of closely
  related classes that are reused and/or modified together.

  A class `K`'s fan-out coupling is the number of classes
  that `K` depends upon.
  A category's fan-out coupling is the number of classes outside the
  category that the classes inside the category depend upon.

  A class `K`'s fan-in coupling is the number of classes
  that depend upon `K`.
  A category's fan-in coupling is the number of classes outside the
  category that depend on classes belonging to the category.

  Ada's object-oriented paradigm separates the instantiable entity
  (type) from the module (package), so the definition of the coupling
  metrics for Ada maps the class and class category notions
  onto Ada constructs.

  For the coupling metrics, several kinds of modules that define a tagged type
  or an interface type  -- library packages, library generic packages, and
  library generic package instantiations -- are considered to be classes.
  A category consists of a library package (or
  a library generic package) that defines a tagged or an interface type,
  together with all its descendant (generic) packages that define tagged
  or interface types. Thus a
  category is an Ada hierarchy of library-level program units. Class
  coupling in Ada is referred to as 'tagged coupling', and category coupling
  is referred to as 'hierarchy coupling'.

  For any package serving as a class, its body and subunits (if any) are
  considered together with its spec when computing dependencies, and coupling
  metrics are reported for spec units only. Dependencies between classes
  mean Ada semantic dependencies. For object-oriented coupling
  metrics, only dependencies on units treated as classes are
  considered.

  Similarly, for unit and control coupling an entity is considered to be the
  conceptual construct consisting of the entity's specification, body, and
  any subunits (transitively).
  *gnatmetric* computes
  the dependencies of all these units as a whole, but
  metrics are only reported for spec
  units (or for a subprogram body unit in case if there is no
  separate spec for the given subprogram).

  For unit coupling, dependencies are computed between all kinds of program
  units. For control coupling, the dependencies of a given unit are limited to
  those units that define subprograms. Thus control fan-out coupling is reported
  for all units, but control fan-in coupling is only reported for units
  that define subprograms.

  The following simple example illustrates the difference between unit coupling
  and control coupling metrics:

    .. code-block:: ada

         package Lib_1 is
             function F_1 (I : Integer) return Integer;
         end Lib_1;

         package Lib_2 is
             type T_2 is new Integer;
         end Lib_2;

         package body Lib_1 is
             function F_1 (I : Integer) return Integer is
             begin
                return I + 1;
             end F_1;
         end Lib_1;

         with Lib_2; use Lib_2;
         package Pack is
             Var : T_2;
             function Fun (I : Integer) return Integer;
         end Pack;

         with Lib_1; use Lib_1;
         package body Pack is
             function Fun (I : Integer) return Integer is
             begin
                return F_1 (I);
             end Fun;
         end Pack;

  If we apply *gnatmetric* with the *--coupling-all* option to
  these units, the result will be:

    ::

       Coupling metrics:
       =================
           Unit Lib_1 (C:\\customers\\662\\L406-007\\lib_1.ads)
              control fan-out coupling  : 0
              control fan-in coupling   : 1
              unit fan-out coupling     : 0
              unit fan-in coupling      : 1

           Unit Pack (C:\\customers\\662\\L406-007\\pack.ads)
              control fan-out coupling  : 1
              control fan-in coupling   : 0
              unit fan-out coupling     : 2
              unit fan-in coupling      : 0

           Unit Lib_2 (C:\\customers\\662\\L406-007\\lib_2.ads)
              control fan-out coupling  : 0
              unit fan-out coupling     : 0
              unit fan-in coupling      : 1

  The result does not contain values for object-oriented
  coupling because none of the argument units contains a tagged type and
  therefore none of these units can be treated as a class.

  The `Pack` package (spec and body) depends on two
  units -- `Lib_1` `and Lib_2` -- and so its unit fan-out coupling
  is 2. Since nothing depends on it, its unit fan-in coupling is 0, as
  is its control fan-in coupling. Only one of the units `Pack` depends
  upon defines a subprogram, so its control fan-out coupling is 1.

  `Lib_2` depends on nothing, so its fan-out metrics are 0. It does
  not define any subprograms, so it has no control fan-in metric.
  One unit (`Pack`) depends on it , so its unit fan-in coupling is 1.

  `Lib_1` is similar to `Lib_2`, but it does define a subprogram.
  Its control fan-in coupling is 1 (because there is one unit
  depending on it).

  When computing coupling metrics, *gnatmetric* counts only
  dependencies between units that are arguments of the *gnatmetric*
  invocation. Coupling metrics are program-wide (or project-wide) metrics, so
  you should invoke *gnatmetric* for
  the complete set of sources comprising your program. This can be done
  by invoking *gnatmetric* with the corresponding project file
  and with the *-U* option.

  By default, all the coupling metrics are disabled. You can use the following
  switches to specify the coupling metrics to be computed and reported:

  .. index:: --tagged-coupling (gnatmetric)
  .. index:: --hierarchy-coupling (gnatmetric)
  .. index:: --unit-coupling (gnatmetric)
  .. index:: --control-coupling (gnatmetric)

  :samp:`--coupling-all`
    Report all the coupling metrics


  :samp:`--tagged-coupling-out`
    Report tagged (class) fan-out coupling


  :samp:`--tagged-coupling-in`
    Report tagged (class) fan-in coupling


  :samp:`--hierarchy-coupling-out`
    Report hierarchy (category) fan-out coupling


  :samp:`--hierarchy-coupling-in`
    Report hierarchy (category) fan-in coupling


  :samp:`--unit-coupling-out`
    Report unit fan-out coupling


  :samp:`--unit-coupling-in`
    Report unit fan-in coupling


  :samp:`--control-coupling-out`
    Report control fan-out coupling


  :samp:`--control-coupling-in`
    Report control fan-in coupling


  .. _Other_gnatmetric_Switches:

  Other `gnatmetric` Switches
  ---------------------------

  Additional *gnatmetric* switches are as follows:


  .. index:: --version (gnatmetric)

  :samp:`--version`
    Display Copyright and version, then exit disregarding all other options.


  .. index:: --help (gnatmetric)

  :samp:`--help`
    Display usage, then exit disregarding all other options.


  .. index:: -P (gnatmetric)

  :samp:`-P {file}`
    Indicates the name of the project file that describes the set of sources
    to be processed. The exact set of argument sources depends on other options
    specified, see below.


  .. index:: -U (gnatmetric)

  :samp:`-U`
    If a project file is specified and no argument source is explicitly
    specified (either directly or by means of *-files* option), process
    all the units of the closure of the argument project. Otherwise this option
    has no effect.


  :samp:`-U {main_unit}`
    If a project file is specified and no argument source is explicitly
    specified (either directly or by means of *-files* option), process
    the closure of units rooted at `main_unit`. Otherwise this option
    has no effect.


  .. index:: -X (gnatmetric)

  :samp:`-X{name}={value}`
    Indicates that external variable `name` in the argument project
    has the value `value`. Has no effect if no project is specified as
    tool argument.


  .. index:: --RTS (gnatmetric)

  :samp:`--RTS={rts-path}`
    Specifies the default location of the runtime library. Same meaning as the
    equivalent *gnatmake* flag (see :ref:`Switches_for_gnatmake`).


  .. index:: --subdirs=dir (gnatmetric)

  :samp:`--subdirs={dir}`
    Use the specified subdirectory of the project objects file (or of the
    project file directory if the project does not specify an object directory)
    for tool output files. Has no effect if no project is specified as
    tool argument r if *--no_objects_dir* is specified.


  .. index:: --no_objects_dir (gnatmetric)

  :samp:`--no_objects_dir`
    Place all the result files into the current directory instead of
    project objects directory. This corresponds to the *gnatcheck*
    behavior when it is called with the project file from the
    GNAT driver. Has no effect if no project is specified.


  .. index:: -files (gnatmetric)

  :samp:`-files {filename}`
    Take the argument source files from the specified file. This file should be an
    ordinary text file containing file names separated by spaces or
    line breaks. You can use this switch more than once in the same call to
    *gnatmetric*. You also can combine this switch with
    an explicit list of files.


  .. index:: -j (gnatmetric)

  :samp:`-j{n}`
    Use `n` processes to carry out the tree creations (internal representations
    of the argument sources). On a multiprocessor machine this speeds up processing
    of big sets of argument sources. If `n` is 0, then the maximum number of
    parallel tree creations is the number of core processors on the platform.

  .. index:: -t (gnatmetric)


  :samp:`-t`
    Print out execution time.


  .. index:: -v (gnatmetric)

  :samp:`-v`
    Verbose mode;
    *gnatmetric* generates version information and then
    a trace of sources being processed.


  .. index:: -q (gnatmetric)

  :samp:`-q`
    Quiet mode.

  If a project file is specified and no argument source is explicitly
  specified (either directly or by means of *-files* option), and no
  *-U* is specified, then the set of processed sources is
  all the immediate units of the argument project.


.. only:: PRO or GPL

   .. _The_GNAT_Pretty-Printer_gnatpp:

   The GNAT Pretty-Printer *gnatpp*
   ================================

   .. index:: ! gnatpp
   .. index:: Pretty-Printer

   The *gnatpp* tool is an ASIS-based utility
   for source reformatting / pretty-printing.
   It takes an Ada source file as input and generates a reformatted
   version as output.
   You can specify various style directives via switches; e.g.,
   identifier case conventions, rules of indentation, and comment layout.

   *gnatpp* is a project-aware tool
   (see :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
   the project-related switches).  The project file package that can specify
   *gnatpp* switches is named ``Pretty_Printer``.

   To produce a reformatted file, *gnatpp* invokes the Ada
   compiler and generates and uses the ASIS tree for the input source;
   thus the input must be legal Ada code, and the tool should have all the
   information needed to compile the input source. To provide this information,
   you may specify as a tool parameter the project file the input source belongs to
   (or you may call *gnatpp*
   through the *gnat* driver (see :ref:`The_GNAT_Driver_and_Project_Files`).
   Another possibility is to specify the source search
   path and needed configuration files in ``-cargs`` section of *gnatpp*
   call, see the description of the *gnatpp* switches below.

   *gnatpp* cannot process sources that contain preprocessing directives.

   The *gnatpp* command has the form

     ::

        $ gnatpp [`switches`] `filename` [-cargs `gcc_switches`]

   where

   * `switches` is an optional sequence of switches defining such properties as
     the formatting rules, the source search path, and the destination for the
     output source file

   * `filename` is the name (including the extension) of the source file to
     reformat; wildcards or several file names on the same gnatpp command are
     allowed. The file name may contain path information; it does not have to
     follow the GNAT file naming rules

   * `gcc_switches` is a list of switches for
     *gcc*. They will be passed on to all compiler invocations made by
     *gnatpp* to generate the ASIS trees. Here you can provide
     ``-I`` switches to form the source search path,
     use the ``-gnatec`` switch to set the configuration file, etc.


   .. _Switches_for_gnatpp:

   Switches for *gnatpp*
   ---------------------

   The following subsections describe the various switches accepted by
   *gnatpp*, organized by category.

   You specify a switch by supplying a name and generally also a value.
   In many cases the values for a switch with a given name are incompatible with
   each other
   (for example the switch that controls the casing of a reserved word may have
   exactly one value: upper case, lower case, or
   mixed case) and thus exactly one such switch can be in effect for an
   invocation of *gnatpp*.
   If more than one is supplied, the last one is used.
   However, some values for the same switch are mutually compatible.
   You may supply several such switches to *gnatpp*, but then
   each must be specified in full, with both the name and the value.
   Abbreviated forms (the name appearing once, followed by each value) are
   not permitted.

   .. _Alignment_Control:

   Alignment Control
   ^^^^^^^^^^^^^^^^^

   .. index:: Alignment control in gnatpp

   Programs can be easier to read if certain constructs are vertically aligned.
   By default, alignment of the following constructs is set ON:

     * ``:`` in declarations,
     * ``:=`` in initializations in declarations,
     * ``:=`` in assignment statements,
     * ``=>`` in associations, and
     * ``at`` keywords in the component clauses in record representation clauses.


   .. index:: -A0 (gnatpp)
   .. index:: -A1 (gnatpp)


   :samp:`-A0`
     Set alignment to OFF


   :samp:`-A1`
     Set alignment to ON

   .. _Casing_Control:


   Casing Control
   ^^^^^^^^^^^^^^

   .. index:: Casing control in gnatpp

   *gnatpp* allows you to specify the casing for reserved words,
   pragma names, attribute designators and identifiers.
   For identifiers you may define a
   general rule for name casing but also override this rule
   via a set of dictionary files.

   Three types of casing are supported: lower case, upper case, and mixed case.
   'Mixed case' means that the first letter, and also each letter immediately
   following an underscore, are converted to their uppercase forms;
   all the other letters are converted to their lowercase forms.

   .. index:: -a (gnatpp)


   :samp:`-aL`
     Attribute designators are lower case


   :samp:`-aU`
     Attribute designators are upper case


   :samp:`-aM`
     Attribute designators are mixed case (this is the default)

   .. index:: -k (gnatpp)


   :samp:`-kL`
     Keywords (technically, these are known in Ada as *reserved words*) are
     lower case (this is the default)


   :samp:`-kU`
     Keywords are upper case

   .. index:: -n (gnatpp)


   :samp:`-nD`
     Name casing for defining occurrences are as they appear in the source file
     (this is the default)


   :samp:`-nU`
     Names are in upper case


   :samp:`-nL`
     Names are in lower case


   :samp:`-nM`
     Names are in mixed case

   .. index:: -ne (gnatpp)


   :samp:`-neD`
     Enumeration literal casing for defining occurrences are as they appear in the
     source file. Overrides -n casing setting.


   :samp:`-neU`
     Enumeration literals are in upper case.  Overrides -n casing
     setting.


   :samp:`-neL`
     Enumeration literals are in lower case. Overrides -n casing
     setting.


   :samp:`-neM`
     Enumeration literals are in mixed case. Overrides -n casing
     setting.

   .. index:: -nt (gnatpp)


   :samp:`-ntD`
     Names introduced by type and subtype declarations are always
     cased as they appear in the declaration in the source file.
     Overrides -n casing setting.


   :samp:`-ntU`
     Names introduced by type and subtype declarations are always in
     upper case. Overrides -n casing setting.


   :samp:`-ntL`
     Names introduced by type and subtype declarations are always in
     lower case. Overrides -n casing setting.


   :samp:`-ntM`
     Names introduced by type and subtype declarations are always in
     mixed case. Overrides -n casing setting.


   :samp:`-nnU`
     Names introduced by number declarations are always in
     upper case. Overrides -n casing setting.


   :samp:`-nnL`
     Names introduced by number declarations are always in
     lower case. Overrides -n casing setting.


   :samp:`-nnM`
     Names introduced by number declarations are always in
     mixed case. Overrides -n casing setting.

   .. index:: -p (gnatpp)


   :samp:`-pL`
     Pragma names are lower case


   :samp:`-pU`
     Pragma names are upper case


   :samp:`-pM`
     Pragma names are mixed case (this is the default)


   .. index:: -D (gnatpp)

   :samp:`-D{file}`
     Use `file` as a *dictionary file* that defines
     the casing for a set of specified names,
     thereby overriding the effect on these names by
     any explicit or implicit
     -n switch.
     To supply more than one dictionary file,
     use several ``-D`` switches.

     *gnatpp* implicitly uses a *default dictionary file*
     to define the casing for the Ada predefined names and
     the names declared in the GNAT libraries.


   .. index:: -D- (gnatpp)

   :samp:`-D-`
     Do not use the default dictionary file;
     instead, use the casing
     defined by a ``-n`` switch and any explicit
     dictionary file(s)

   The structure of a dictionary file, and details on the conventions
   used in the default dictionary file, are defined in :ref:`Name_Casing`.

   The ``-D-`` and
   ``-D-``\ `file` switches are mutually
   compatible.

   This group of *gnatpp* switches controls the layout of comments and
   complex syntactic constructs.  See :ref:`Formatting_Comments` for details
   on their effect.


   .. index:: -c (gnatpp)


   :samp:`-c0`
     All comments remain unchanged.


   :samp:`-c1`
     GNAT-style comment line indentation.
     This is the default.


   :samp:`-c3`
     GNAT-style comment beginning.


   :samp:`-c4`
     Fill comment blocks.


   :samp:`-c5`
     Keep unchanged special form comments.
     This is the default.


   .. index:: --comments-only (gnatpp)

   :samp:`--comments-only`
     Format just the comments.

   .. index:: --no-end-id (gnatpp)


   :samp:`--no-end-id`
     Do not insert the name of a unit after `end`; leave whatever comes
     after `end`, if anything, alone.

   .. index:: --no-separate-is (gnatpp)


   :samp:`--no-separate-is`
     Do not place the keyword `is` on a separate line in a subprogram body in
     case if the spec occupies more than one line.

   .. index:: --separate-loop-then (gnatpp)


   :samp:`--separate-loop-then`
     Place the keyword `loop` in FOR and WHILE loop statements and the
     keyword `then` in IF statements on a separate line.

   .. index:: --no-separate-loop-then (gnatpp)


   :samp:`--no-separate-loop-then`
     Do not place the keyword `loop` in FOR and WHILE loop statements and the
     keyword `then` in IF statements on a separate line. This option is
     incompatible with ``--separate-loop-then`` option.

   .. index:: --use-on-new-line (gnatpp)


   :samp:`--use-on-new-line`
     Start each USE clause in a context clause from a separate line.

   .. index:: --insert-blank-lines (gnatpp)


   :samp:`--insert-blank-lines`
     Insert blank lines where appropriate (between bodies and other large
     constructs).

   .. index:: --preserve-blank-lines (gnatpp)


   :samp:`--preserve-blank-lines`
     Preserve blank lines in the input. By default, gnatpp will squeeze
     multiple blank lines down to one.


   The ``-c`` switches are compatible with one another, except that
   the ``-c0`` switch disables all other comment formatting
   switches.


   .. _General_Text_Layout_Control:

   General Text Layout Control
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^

   These switches allow control over line length and indentation.

   .. index:: -M (gnatpp)

   :samp:`-M{nnn}`
     Maximum line length, `nnn` from 32...256, the default value is 79


   .. index:: -i (gnatpp)

   :samp:`-i{nnn}`
     Indentation level, `nnn` from 1...9, the default value is 3


   .. index:: -cl (gnatpp)

   :samp:`-cl{nnn}`
     Indentation level for continuation lines (relative to the line being
     continued), `nnn` from 1...9.
     The default
     value is one less than the (normal) indentation level, unless the
     indentation is set to 1 (in which case the default value for continuation
     line indentation is also 1)


   .. _Other_Formatting_Options:

   Other Formatting Options
   ^^^^^^^^^^^^^^^^^^^^^^^^

   These switches control other formatting not listed above.

   .. index:: --decimal-grouping  (gnatpp)

   :samp:`--decimal-grouping={n}`
     Put underscores in decimal literals (numeric literals without a base)
     every `n` characters. If a literal already has one or more
     underscores, it is not modified. For example, with
     `--decimal-grouping=3`, `1000000` will be changed to
     `1_000_000`.


   .. index:: --based-grouping  (gnatpp)

   :samp:`--based-grouping={n}`
     Same as `--decimal-grouping`, but for based literals. For
     example, with `--based-grouping=4`, `16#0001FFFE#` will be
     changed to `16#0001_FFFE#`.


   .. index:: --split-line-before-op (gnatpp)

   :samp:`--split-line-before-op`
     If it is necessary to split a line at a binary operator, by default
     the line is split after the operator. With this option, it is split
     before the operator.


   .. index:: --RM-style-spacing (gnatpp)

   :samp:`--RM-style-spacing`
     Do not insert an extra blank before various occurrences of
     '(' and ':'. This also turns off alignment.


   .. index:: -ff (gnatpp)

   :samp:`-ff`
     Insert a Form Feed character after a pragma Page.


   .. index:: --call_threshold (gnatpp)

   :samp:`--call_threshold={nnn}`
     If the number of parameter associations is greater than `nnn` and if at
     least one association uses named notation, start each association from
     a new line. If `nnn` is 0, no check for the number of associations
     is made; this is the default.


   .. index:: --par_threshold (gnatpp)

   :samp:`--par_threshold={nnn}`
     If the number of parameter specifications is greater than `nnn`
     (or equal to `nnn` in case of a function), start each specification from
     a new line. This feature is disabled by default.


   .. _Setting_the_Source_Search_Path:

   Setting the Source Search Path
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   To define the search path for the input source file, *gnatpp*
   uses the same switches as the GNAT compiler, with the same effects:

   .. index:: -I (gnatpp)


   :samp:`-I{dir}`

   .. index:: -I- (gnatpp)

   :samp:`-I-`

   .. index:: -gnatec (gnatpp)

   :samp:`-gnatec={path}`


   .. _Output_File_Control-gnatpp:

   Output File Control
   ^^^^^^^^^^^^^^^^^^^

   By default the output is sent to a file whose name is obtained by appending
   the :file:`.pp` suffix to the name of the input file.
   If the file with this name already exists, it is overwritten.
   Thus if the input file is :file:`my_ada_proc.adb` then
   *gnatpp* will produce :file:`my_ada_proc.adb.pp`
   as output file.
   The output may be redirected by the following switches:


   .. index:: --output-dir (gnatpp)

   :samp:`--output-dir={dir}`
     Generate output file in directory :file:`dir` with the same name as the input
     file. If :file:`dir` is the same as the directory containing the input file,
     the input file is not processed; use ``-rnb``
     if you want to update the input file in place.


   .. index:: -pipe (gnatpp)

   :samp:`-pipe`
     Send the output to `Standard_Output`


   .. index:: -o (gnatpp)

   :samp:`-o {output_file}`
     Write the output into `output_file`.
     If `output_file` already exists, *gnatpp* terminates without
     reading or processing the input file.


   .. index:: -of (gnatpp)

   :samp:`-of {output_file}`
     Write the output into `output_file`, overwriting the existing file
     (if one is present).


   .. index:: -r (gnatpp)

   :samp:`-r`
     Replace the input source file with the reformatted output, and copy the
     original input source into the file whose name is obtained by appending the
     :file:`.npp` suffix to the name of the input file.
     If a file with this name already exists, *gnatpp* terminates without
     reading or processing the input file.


   .. index:: -rf (gnatpp)

   :samp:`-rf`
     Like ``-r`` except that if the file with the specified name
     already exists, it is overwritten.


   .. index:: -rnb (gnatpp)

   :samp:`-rnb`
     Replace the input source file with the reformatted output without
     creating any backup copy of the input source.


   .. index:: --eol (gnatpp)

   :samp:`--eol={xxx}`
     Specifies the line-ending style of the reformatted output file. The `xxx`
     string specified with the switch may be:

     * *dos* - MS DOS style, lines end with CR LF characters*
     * *crlf*  - the same as *dos*
     * *unix* - UNIX style, lines end with LF character*
     * *lf* -  the same as *unix*

   .. index:: -W (gnatpp)

   :samp:`-W{e}`
     Specify the wide character encoding method for the input and output files.
     `e` is one of the following:

     * *h* - Hex encoding

     * *u* - Upper half encoding

     * *s* - Shift/JIS encoding

     * *e* - EUC encoding

     * *8* - UTF-8 encoding

     * *b* - Brackets encoding (default value)

   Options ``-o`` and ``-of`` are allowed only if the call to gnatpp
   contains only one file to reformat.

   Option ``--eol`` and ``-W`` cannot be used together
   with the ``-pipe`` option.


   .. _Other_gnatpp_Switches:

   Other `gnatpp` Switches
   ^^^^^^^^^^^^^^^^^^^^^^^

   The additional *gnatpp* switches are defined in this subsection.


   .. index:: --version  (gnatpp)

   :samp:`--version`
     Display copyright and version, then exit disregarding all other options.


   .. index:: --help  (gnatpp)

   :samp:`--help`
     Display usage, then exit disregarding all other options.


   .. index:: -P  (gnatpp)

   :samp:`-P {file}`
     Indicates the name of the project file that describes the set of sources
     to be processed. The exact set of argument sources depends on other options
     specified; see below.


   .. index:: -U  (gnatpp)

   :samp:`-U`
     If a project file is specified and no argument source is explicitly
     specified (either directly or by means of ``-files`` option), process
     all the units of the closure of the argument project. Otherwise this option
     has no effect.


   :samp:`-U {main_unit}`
     If a project file is specified and no argument source is explicitly
     specified (either directly or by means of ``-files`` option), process
     the closure of units rooted at `main_unit`. Otherwise this option
     has no effect.


   .. index:: -X  (gnatpp)

   :samp:`-X{name}={value}`
     Indicates that external variable `name` in the argument project
     has the value `value`. Has no effect if no project is specified as
     tool argument.


   .. index:: --RTS (gnatpp)

   :samp:`--RTS={rts-path}`
     Specifies the default location of the runtime library. Same meaning as the
     equivalent *gnatmake* flag (:ref:`Switches_for_gnatmake`).


   .. index:: --incremental  (gnatpp)

   :samp:`--incremental`
     Incremental processing on a per-file basis. Source files are only
     processed if they have been modified, or if files they depend on have
     been modified. This is similar to the way gnatmake/gprbuild only
     compiles files that need to be recompiled. A project file is required
     in this mode, and the gnat driver (as in *gnat pretty*) is not
     supported.


   .. index:: --pp-off  (gnatpp)

   :samp:`--pp-off={xxx}`
     Use `--xxx` as the command to turn off pretty printing, instead
     of the default `--!pp off`.


   .. index:: --pp-on  (gnatpp)

   :samp:`--pp-on={xxx}`
     Use `--xxx` as the command to turn pretty printing back on, instead
     of the default `--!pp on`.


   .. index:: -files (gnatpp)

   :samp:`-files {filename}`
     Take the argument source files from the specified file. This file should be an
     ordinary text file containing file names separated by spaces or
     line breaks. You can use this switch more than once in the same call to
     *gnatpp*. You also can combine this switch with an explicit list of
     files.


   .. index:: -j (gnatpp)

   :samp:`-j{n}`
     Without ``--incremental``, use `n` processes to carry out the
     tree creations (internal representations of the argument sources). On
     a multiprocessor machine this speeds up processing of big sets of
     argument sources. If `n` is 0, then the maximum number of parallel
     tree creations is the number of core processors on the platform. This
     option cannot be used together with ``-r``,
     ``-rf`` or
     ``-rnb`` option.

     With ``--incremental``, use `n` *gnatpp* processes to
     perform pretty-printing in parallel. `n` = 0 means the same as
     above. In this case, ``-r``,
     ``-rf`` or
     ``-rnb`` options are allowed.

   .. index:: -t (gnatpp)


   :samp:`-t`
     Print out execution time.


   .. index:: -v (gnatpp)

   :samp:`-v`
     Verbose mode


   .. index:: -q (gnatpp)

   :samp:`-q`
     Quiet mode

   If a project file is specified and no argument source is explicitly
   specified (either directly or by means of ``-files`` option), and no
   ``-U`` is specified, then the set of processed sources is
   all the immediate units of the argument project.


   .. _Formatting_Rules:

   Formatting Rules
   ----------------

   The following subsections show how *gnatpp* treats white space,
   comments, program layout, and name casing.
   They provide detailed descriptions of the switches shown above.


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


   .. _White_Space_and_Empty_Lines:

   White Space and Empty Lines
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^

   *gnatpp* does not have an option to control space characters.
   It will add or remove spaces according to the style illustrated by the
   examples in the :title:`Ada Reference Manual`.
   The output file will contain no lines with trailing white space.

   By default, a sequence of one or more blank lines in the input is
   converted to a single blank line in the output; multiple blank lines
   are squeezed down to one.
   The ``--preserve-blank-lines`` option
   turns off the squeezing; each blank line in the input is copied
   to the output.
   The ``--insert-blank-lines`` option
   causes additional blank lines to be inserted if not already
   present in the input (e.g. between bodies).


   .. _Formatting_Comments:

   Formatting Comments
   ^^^^^^^^^^^^^^^^^^^

   Comments in Ada code are of two kinds:

   * a *whole-line comment*, which appears by itself (possibly preceded by
     white space) on a line

   * an *end-of-line comment*, which follows some other Ada code on
     the same line.

   A whole-line comment is indented according to the surrounding code,
   with some exceptions.
   Comments that start in column 1 are kept there.
   If possible, comments are not moved so far to the right that the maximum
   line length is exceeded.
   The ``-c0`` option
   turns off comment formatting.
   Special-form comments such as SPARK-style ``--#...`` are left alone.

   For an end-of-line comment, *gnatpp* tries to leave the same
   number of spaces between the end of the preceding Ada code and the
   beginning of the comment as appear in the original source.

   The ``-c3`` switch
   (GNAT style comment beginning) has the following
   effect:

     * For each whole-line comment that does not end with two hyphens,
       *gnatpp* inserts spaces if necessary after the starting two hyphens
       to ensure that there are at least two spaces between these hyphens and the
       first non-blank character of the comment.

   The ``-c4`` switch specifies that
   whole-line comments that form a paragraph will be filled in typical
   word processor style (that is, moving words between lines to make the
   lines other than the last similar in length ).

   The ``--comments-only`` switch specifies that only the comments
   are formatted; the rest of the program text is left alone. The
   comments are formatted according to the -c3 and -c4 switches; other
   formatting switches are ignored. For example,
   ``--comments-only -c4`` means to fill comment paragraphs, and do nothing else.
   Likewise,
   ``--comments-only -c3`` ensures comments start with at least two
   spaces after `--`, and ``--comments-only -c3 -c4`` does
   both. If ``--comments-only`` is given without ``-c3`` or
   ``-c4``, then gnatpp doesn't format anything.


   .. _Name_Casing:

   Name Casing
   ^^^^^^^^^^^

   *gnatpp* always converts the usage occurrence of a (simple) name to
   the same casing as the corresponding defining identifier.

   You control the casing for defining occurrences via the
   ``-n`` switch.
   With ``-nD`` ('as declared', which is the default),
   defining occurrences appear exactly as in the source file
   where they are declared.
   The other values for this switch --
   ``-nU``,
   ``-nL``,
   ``-nM`` --
   result in
   upper, lower, or mixed case, respectively.
   If *gnatpp* changes the casing of a defining
   occurrence, it analogously changes the casing of all the
   usage occurrences of this name.

   If the defining occurrence of a name is not in the source compilation unit
   currently being processed by *gnatpp*, the casing of each reference to
   this name is changed according to the value of the ``-n``
   switch (subject to the dictionary file mechanism described below).
   Thus *gnatpp* acts as though the ``-n`` switch
   had affected the
   casing for the defining occurrence of the name.

   The options
   :samp:`-a{x}`,
   :samp:`-k{x}`,
   :samp:`-ne{x}`,
   :samp:`-nt{x}`,
   :samp:`-nn{x}`, and
   :samp:`-p{x}`
   allow finer-grained control over casing for
   attributes, keywords, enumeration literals,
   types, named numbers and pragmas, respectively.
   :samp:`-nt{x}` covers subtypes and
   task and protected bodies as well.

   Some names may need to be spelled with casing conventions that are not
   covered by the upper-, lower-, and mixed-case transformations.
   You can arrange correct casing by placing such names in a
   *dictionary file*,
   and then supplying a ``-D`` switch.
   The casing of names from dictionary files overrides
   any ``-n`` switch.

   To handle the casing of Ada predefined names and the names from GNAT libraries,
   *gnatpp* assumes a default dictionary file.
   The name of each predefined entity is spelled with the same casing as is used
   for the entity in the :title:`Ada Reference Manual` (usually mixed case).
   The name of each entity in the GNAT libraries is spelled with the same casing
   as is used in the declaration of that entity.

   The ``-D-`` switch suppresses the use of
   the default dictionary file. Instead, the casing for predefined and
   GNAT-defined names will be established by the
   ``-n`` switch or explicit dictionary files. For
   example, by default the names `Ada.Text_IO` and
   `GNAT.OS_Lib` will appear as just shown, even in the presence of
   a ``-nU`` switch.  To ensure that even
   such names are rendered in uppercase, additionally supply the
   -D- switch (or else place these names
   in upper case in a dictionary file).

   A dictionary file is a plain text file; each line in this file can be
   either a blank line (containing only space characters), an Ada comment
   line, or the specification of exactly one *casing schema*.

   A casing schema is a string that has the following syntax:

     ::

        `casing_schema` ::= `identifier` | `simple_identifier`

        `simple_identifier` ::= `letter`{`letter_or_digit`}


   (See :title:`Ada Reference Manual`, Section 2.3) for the definition of the
   `identifier` lexical element and the `letter_or_digit` category.)

   The casing schema string can be followed by white space and/or an Ada-style
   comment; any amount of white space is allowed before the string.

   If a dictionary file is passed as
   the value of a :samp:`-D{file}` switch
   then for every
   simple name and every identifier, *gnatpp* checks if the dictionary
   defines the casing for the name or for some of its parts (the term 'subword'
   is used below to denote the part of a name which is delimited by '_' or by
   the beginning or end of the word and which does not contain any '_' inside):

   * if the whole name is in the dictionary, *gnatpp* uses for this name
     the casing defined by the dictionary; no subwords are checked for this word

   * for every subword *gnatpp* checks if the dictionary contains the
     corresponding string of the form `*`simple_identifier`*`,
     and if it does, the casing of this `simple_identifier` is used
     for this subword

   * if the whole name does not contain any '_' inside, and if for this name
     the dictionary contains two entries - one of the form `identifier`,
     and another - of the form *`simple_identifier`*, then the first one
     is applied to define the casing of this name

   * if more than one dictionary file is passed as *gnatpp* switches, each
     dictionary adds new casing exceptions and overrides all the existing casing
     exceptions set by the previous dictionaries

   * when *gnatpp* checks if the word or subword is in the dictionary,
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

   If *gnatpp* is called with the following switches:

     ::

        $ gnatpp -nM -D dict1 -D dict2 test.adb

   then we will get the following name casing in the *gnatpp* output:


     .. code-block:: ada

        procedure Test is
           NAME1             : Integer := 1;
           Name4_NAME3_Name2 : Integer := 2;
           Name2_NAME3_Name4 : Boolean;
           Name1_Var         : Float;
        begin
           Name2_NAME3_Name4 := Name4_NAME3_Name2 > NAME1;
        end Test;


.. only:: PRO or GPL

  .. _The_Body_Stub_Generator_gnatstub:

  The Body Stub Generator *gnatstub*
  ==================================

  .. index:: ! gnatstub

  *gnatstub* creates empty but compilable bodies
  for library unit declarations, and empty but compilable
  subunit for body stubs.

  *gnatstub* is a project-aware tool.
  (See :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
  the project-related switches but note that *gnatstub* does not support
  the :samp:`-U`, :samp:`-U {main_unit}`, :samp:`--subdirs={dir}`, or
  :samp:`--no_objects_dir` switches.)
  The project file package that can specify
  *gnatstub* switches is named ``gnatstub``.


  To create a body or a subunit, *gnatstub* invokes the Ada
  compiler and generates and uses the ASIS tree for the input source;
  thus the input must be legal Ada code, and the tool should have all the
  information needed to compile the input source. To provide this information,
  you may specify as a tool parameter the project file the input source belongs to
  (or you may call *gnatstub*
  through the *gnat* driver (see :ref:`The_GNAT_Driver_and_Project_Files`).
  Another possibility is to specify the source search
  path and needed configuration files in ``-cargs`` section of *gnatstub*
  call, see the description of the *gnatstub* switches below.

  If the *gnatstub* argument source contains preprocessing directives
  then the needed options should be provided to run preprocessor as a part of
  the *gnatstub* call, and the generated body stub will correspond to
  the preprocessed source.

  By default, all the program unit bodies generated by `gnatstub`
  raise the predefined `Program_Error` exception, which will catch
  accidental calls of generated stubs. This behavior can be changed with
  option ``--no-exception`` (see below).

  .. _Running_gnatstub:

  Running *gnatstub*
  ------------------

  *gnatstub* invocation has the following form:

    ::

       $ gnatstub [`switches`] `filename` [-cargs `gcc_switches`]

  where

  * *filename*
      is the name of the source file that contains a library unit declaration
      for which a body must be created or a library unit body for which subunits
      must be created for the body stubs declared in this body.
      The file name may contain the path information.
      If the name does not follow GNAT file naming conventions and a set
      of seitches does not contain a project file that defines naming
      conventions, the name of the body file must
      be provided
      explicitly as the value of the :samp:`-o{body-name}` option.
      If the file name follows the GNAT file naming
      conventions and the name of the body file is not provided,
      *gnatstub*
      takes the naming conventions for the generated source from the
      project file provided as a parameter of ``-P`` switch if any,
      or creates the name file to generate using the standard GNAT
      naming conventions.

  * *gcc_switches* is a list of switches for *gcc*.
      They will be passed on to all compiler invocations made by
      *gnatstub* to generate the ASIS trees. Here you can provide
      ``-I`` switches to form the source search path,
      use the ``-gnatec`` switch to set the configuration file,
      use the ``-gnat05`` switch if sources should be compiled in
      Ada 2005 mode etc.

  * *switches*
      is an optional sequence of switches as described in the next section


  .. _Switches_for_gnatstub:

  Switches for *gnatstub*
  -----------------------

  .. index:: --version (gnatstub)

  :samp:`--version`
    Display Copyright and version, then exit disregarding all other options.


  .. index:: --help (gnatstub)

  :samp:`--help`
    Display usage, then exit disregarding all other options.


  .. index:: -P (gnatstub)

  :samp:`-P {file}`
    Indicates the name of the project file that describes the set of sources
    to be processed.


  .. index:: -X (gnatstub)

  :samp:`-X{name}={value}`
    Indicates that external variable `name` in the argument project
    has the value `value`. Has no effect if no project is specified as
    tool argument.


  .. index:: --RTS (gnatstub)

  :samp:`--RTS={rts-path}`
    Specifies the default location of the runtime library. Same meaning as the
    equivalent *gnatmake* flag (:ref:`Switches_for_gnatmake`).


  .. index:: --subunits (gnatstub)

  :samp:`--subunits`
    Generate subunits for body stubs. If this switch is specified,
    *gnatstub* expects a library unit body as an agrument file,
    otherwise a library unit declaration is expected. If a body stub
    already has a corresponding subunit, *gnatstub* does not
    generate anything for it.


  .. index:: -f (gnatstub)

  :samp:`-f`
    If the destination directory already contains a file with the name of the
    body file
    for the argument spec file, replace it with the generated body stub.
    This switch cannot be used together with ``--subunits``.


  .. index:: -hs (gnatstub)

  :samp:`-hs`
    Put the comment header (i.e., all the comments preceding the
    compilation unit) from the source of the library unit declaration
    into the body stub.


  .. index:: -hg (gnatstub)

  :samp:`-hg`
    Put a sample comment header into the body stub.


  .. index:: --header-file (gnatstub)

  :samp:`--header-file={filename}`
    Use the content of the file as the comment header for a generated body stub.


  .. index:: -IDIR (gnatstub)
  .. index:: -I- (gnatstub)

  :samp:`-I{DIR}`, :samp:`-I-`
    These switches have  the same meaning as in calls to
    *gcc*.
    They define  the source search path in the call to
    *gcc* issued
    by *gnatstub* to compile an argument source file.


  .. index:: -gnatec (gnatstub)

  :samp:`-gnatec{PATH}`
    This switch has the same meaning as in calls to *gcc*.
    It defines the additional configuration file to be passed to the call to
    *gcc* issued
    by *gnatstub* to compile an argument source file.


  .. index:: -gnatyM (gnatstub)

  :samp:`-gnatyM{n}`
    (`n` is a non-negative integer). Set the maximum line length that is
    allowed in a source file. The default is 79. The maximum value that can be
    specified is 32767. Note that in the special case of configuration
    pragma files, the maximum is always 32767 regardless of whether or
    not this switch appears.


  .. index:: -gnaty (gnatstub)

  :samp:`-gnaty{n}`
    (`n` is a non-negative integer from 1 to 9). Set the indentation level in
    the generated body sample to `n`.
    The default indentation is 3.


  .. index:: -gnatyo (gnatstub)

  :samp:`-gnatyo`
    Order local bodies alphabetically. (By default local bodies are ordered
    in the same way as the corresponding local specs in the argument spec file.)


  .. index:: -i (gnatstub)

  :samp:`-i{n}`
    Same as :samp:`-gnaty{n}``


  .. index:: -k (gnatstub)

  :samp:`-k`
    Do not remove the tree file (i.e., the snapshot of the compiler internal
    structures used by *gnatstub*) after creating the body stub.


  .. index:: -l (gnatstub)

  :samp:`-l{n}`
    Same as ``-gnatyM`n```


  .. index:: --no-exception (gnatstub)

  :samp:`--no-exception`
    Avoid raising PROGRAM_ERROR in the generated bodies of program unit stubs.
    This is not always possible for function stubs.


  .. index:: --no-local-header (gnatstub)

  :samp:`--no-local-header`
    Do not place local comment header with unit name before body stub for a
    unit.


  .. index:: -o (gnatstub)

  :samp:`-o {body-name}`
    Body file name.  This should be set if the argument file name does not
    follow
    the GNAT file naming
    conventions. If this switch is omitted the default name for the body will be
    obtained
    from the argument file name according to the GNAT file naming conventions.


  .. index:: --dir (gnatstub)

  :samp:`--dir={dir-name}`
    The path to the directory to place the generated files into.
    If this switch is not set, the generated library unit body is
    placed in the current directory, and generated sununits -
    in the directory where the argument body is located.


  .. index:: -W (gnatstub)

  :samp:`-W{e}`
    Specify the wide character encoding method for the output body file.
    `e` is one of the following:

    ==== ==================================
    *h*  Hex encoding
    *u*  Upper half encoding
    *s*  Shift/JIS encoding
    *e*  EUC encoding
    *8*  UTF-8 encoding
    *b*  Brackets encoding (default value)
    ==== ==================================


  .. index:: -q (gnatstub)

  :samp:`-q`
    Quiet mode: do not generate a confirmation when a body is
    successfully created, and do not generate a message when a body is not
    required for an
    argument unit.


  .. index:: -r (gnatstub)

  :samp:`-r`
    Reuse the tree file (if it exists) instead of creating it.  Instead of
    creating the tree file for the library unit declaration, *gnatstub*
    tries to find it in the current directory and use it for creating
    a body. If the tree file is not found, no body is created. This option
    also implies ``-k``, whether or not
    the latter is set explicitly.


  .. index:: -t (gnatstub)

  :samp:`-t`
    Overwrite the existing tree file.  If the current directory already
    contains the file which, according to the GNAT file naming rules should
    be considered as a tree file for the argument source file,
    *gnatstub*
    will refuse to create the tree file needed to create a sample body
    unless this option is set.


  .. index:: -v (gnatstub)

  :samp:`-v`
    Verbose mode: generate version information.



.. only:: PRO or GPL

  .. _The_Unit_Test_Generator_gnattest:

  The Unit Test Generator *gnattest*
  ==================================

  .. index:: ! gnattest

  *gnattest* is an ASIS-based utility that creates unit-test skeletons
  as well as a test driver infrastructure (harness). *gnattest* creates
  a skeleton for each visible subprogram in the packages under consideration when
  they do not exist already.

  *gnattest* is a project-aware tool.
  (See :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
  the project-related switches but note that *gnattest* does not support
  the :samp:`-U`, :samp:`-eL`, :samp:`--subdirs={dir}`, or
  :samp:`--no_objects_dir` switches.)
  The project file package that can specify
  *gnattest* switches is named ``gnattest``.

  The user can choose to generate a single test driver
  that will run all individual tests, or separate test drivers for each test. The
  second option allows much greater flexibility in test execution environment,
  allows to benefit from parallel tests execution to increase performance, and
  provides stubbing support.

  *gnattest* also has a mode of operation where it acts as the test
  aggregator when multiple test executables must be run, in particular when
  the separate test drivers were generated. In this mode it handles individual
  tests execution and upon completion reports the summary results of the test
  run.

  In order to process source files from a project, *gnattest* has to
  semantically analyze the sources. Therefore, test skeletons can only be
  generated for legal Ada units. If a unit is dependent on other units,
  those units should be among the source files of the project or of other projects
  imported by this one.

  Generated skeletons and harnesses are based on the AUnit testing framework.
  AUnit is an Ada adaptation of the xxxUnit testing frameworks, similar to JUnit
  for Java or CppUnit for C++. While it is advised that gnattest users read
  the AUnit manual, deep knowledge of AUnit is not necessary for using *gnattest*.
  For correct operation of *gnattest*, AUnit should be installed and
  aunit.gpr must be on the project path. Except for some special circumstances
  (e.g. a custom run-time is used), this should normally be the case out of the box.


  .. _Running_gnattest:

  Running *gnattest*
  ------------------

  There are two ways of running *gnattest*.

  .. _Framework_Generation_Mode:

  Framework Generation Mode
  ^^^^^^^^^^^^^^^^^^^^^^^^^

  In this mode *gnattest* has the following command-line interface:

    ::

        $ gnattest `-Pprojname` [`switches`] [`filename`] [-cargs `gcc_switches`]

  where

  * :samp:`-P{projname}`
      specifies the project defining the location of source files. When no
      file names are provided on the command line, all sources in the project
      are used as input. This switch is required.

  * :samp:`{filename}`
      is the name of the source file containing the library unit package declaration
      for which a test package will be created. The file name may be given with a
      path.

  * :samp:`{switches}`
      is an optional sequence of switches as described below.

  * :samp:`{gcc_switches}`
      is a list of additional switches for
      *gcc* that will be passed to all compiler invocations
      made by *gnattest* to generate a set of ASIS trees.


  *gnattest* results can be found in two different places.

  * *automatic harness*:
      This is the harness code, which is located by default in
      "gnattest/harness" directory created in the object directory of
      the main project file. All of this code is generated completely
      automatically and can be destroyed and regenerated at will, with the
      exception of the file *gnattest_common.gpr*, which is created if absent,
      but never overwritten. It is not recommended to modify other files
      manually, since these modifications will be lost if *gnattest* is re-run.
      The entry point in the harness code is
      the project file named *test_driver.gpr*. Tests can be compiled and run
      using a command such as:

      ::

         $ gprbuild -P<harness-dir>/test_driver

      Note that if you need to adjust any options used to compile the harness,
      you can do so by editing the file *gnattest_common.gpr*.

  * *actual unit test skeletons*:
      A test skeleton for each visible subprogram is created in a separate file, if it
      doesn't exist already. By default, those separate test files are located in a
      "gnattest/tests" directory that is created in the object directory of
      corresponding project file. For example, if a source file my_unit.ads in
      directory src contains a visible subprogram Proc, then the corresponding unit
      test will be found in file src/tests/my_unit-test_data-tests.adb and will be
      called Test_Proc_<code>. <code> is a signature encoding used to differentiate
      test names in case of overloading.

      Note that if the project already has both my_unit.ads and my_unit-test_data.ads,
      this will cause a name conflict with the generated test package.


  .. _Test_Execution_Mode:

  Test Execution Mode
  ^^^^^^^^^^^^^^^^^^^

  In this  mode *gnattest* has a the following command-line interface:

    ::

        $ gnattest `test_drivers.list` [`switches`]

  where

  * :samp:`{test_drivers.list}`
       is the name of the text file containing the list of executables to treat as
       test drivers. This file is automatically generated by gnattest, but can be
       hand-edited to add or remove tests. This switch is required.


  * :samp:`{switches}`
       is an optional sequence of switches as described below.


  .. _Switches_for_gnattest_in_framework_generation_mode:

  Switches for *gnattest* in framework generation mode
  ----------------------------------------------------

    .. index:: -q (gnattest)

  :samp:`-q`
    Quiet mode: suppresses noncritical output messages.


    .. index:: -v (gnattest)

  :samp:`-v`
    Verbose mode: produces additional output about the execution of the tool.
    When specified alone on the command line, prints tool version and exits.


    .. index:: -r (gnattest)

  :samp:`-r`
    Recursively considers all sources from all projects.


    .. index:: --RTS (gnattest)

  :samp:`--RTS={rts-path}`
    Specifies the default location of the runtime library. Same meaning as the
    equivalent *gnatmake* flag (:ref:`Switches_for_gnatmake`). For restricted
    profiles, *gnattest* takes into account the run-time limitations when
    generating the harness.


    .. index:: --additional-tests (gnattest)

  :samp:`--additional-tests={projname}`
    Sources described in `projname` are considered potential additional
    manual tests to be added to the test suite.


    .. index:: --harness-only (gnattest)

  :samp:`--harness-only`
    When this option is given, *gnattest* creates a harness for all
    sources, treating them as test packages.


    .. index:: --separate-drivers (gnattest)

  :samp:`--separate-drivers[={val}]`
    Generates a separate test driver for each test or unit under test, rather
    than a single executable incorporating all tests. `val` can be "unit" or
    "test", or may be omitted, which defaults to "unit".


    .. index:: --stub (gnattest)

  :samp:`--stub`
    Generates the testing framework that uses subsystem stubbing to isolate the
    code under test.


    .. index:: --harness-dir (gnattest)

  :samp:`--harness-dir={dirname}`
    Specifies the directory that will hold the harness packages and project file
    for the test driver. If the `dirname` is a relative path, it is considered
    relative to the object directory of the project file.


    .. index:: --tests-dir (gnattest)

  :samp:`--tests-dir={dirname}`
    All test packages are placed in the `dirname` directory.
    If the `dirname` is a relative path, it is considered relative to the object
    directory of the project file. When all sources from all projects are taken
    recursively from all projects, `dirname` directories are created for each
    project in their object directories and test packages are placed accordingly.


    .. index:: --subdir (gnattest)

  :samp:`--subdir={dirname}`
    Test packages are placed in a subdirectory of the corresponding source
    directory, with the name `dirname`. Thus, each set of unit tests is located
    in a subdirectory of the code under test.  If the sources are in separate
    directories, each source directory has a test subdirectory named `dirname`.


    .. index:: --tests-root (gnattest)

  :samp:`--tests-root={dirname}`
    The hierarchy of source directories, if any, is recreated in the `dirname`
    directory, with test packages placed in directories corresponding to those
    of the sources.
    If the `dirname` is a relative path, it is considered relative to the object
    directory of the project file. When projects are considered recursively,
    directory hierarchies of tested sources are
    recreated for each project in their object directories and test packages are
    placed accordingly.


    .. index:: --stubs-dir (gnattest)

  :samp:`--stubs-dir={dirname}`
    The hierarchy of directories containing stubbed units is recreated in
    the `dirname` directory, with stubs placed in directories corresponding to
    projects they are derived from.
    If the `dirname` is a relative path, it is considered relative to the object
    directory of the project file. When projects are considered recursively,
    directory hierarchies of stubs are
    recreated for each project in their object directories and test packages are
    placed accordingly.


    .. index:: --exclude-from-stubbing (gnattest)

  :samp:`--exclude-from-stubbing={filename}`
    Disables stubbing of units listed in `filename`. The file should contain
    corresponding spec files, one per line.

  :samp:`--exclude-from-stubbing:{unit}={filename}`
    Same as above, but corresponding units will not be stubbed only when testing
    specified `unit`.

    .. index:: --validate-type-extensions (gnattest)

  :samp:`--validate-type-extensions`
    Enables substitution check: run all tests from all parents in order
    to check substitutability in accordance with the Liskov substitution principle (LSP).


    .. index:: --skeleton-default (gnattest)

  :samp:`--skeleton-default={val}`
    Specifies the default behavior of generated skeletons. `val` can be either
    "fail" or "pass", "fail" being the default.


    .. index:: --passed-tests (gnattest)

  :samp:`--passed-tests={val}`
    Specifies whether or not passed tests should be shown. `val` can be either
    "show" or "hide", "show" being the default.


    .. index:: --exit-status (gnattest)

  :samp:`--exit-status={val}`
    Specifies whether or not generated test driver should return failure exit
    status if at least one test fails or crashes. `val` can be either
    "on" or "off", "off" being the default.


    .. index:: --omit-sloc (gnattest)

  :samp:`--omit-sloc`
    Suppresses comment line containing file name and line number of corresponding
    subprograms in test skeletons.


    .. index:: --no-command-line (gnattest)

  :samp:`--no-command-line`
    Don't add command line support to test driver. Note that regardless of this
    switch, *gnattest* will automatically refrain from adding command
    line support if it detects that the selected run-time doesn't provide
    this capability.


    .. index:: --separates (gnattest)

  :samp:`--separates`
    Bodies of all test routines are generated as separates. Note that this mode is
    kept for compatibility reasons only and it is not advised to use it due to
    possible problems with hash in names of test skeletons when using an
    inconsistent casing. Separate test skeletons can be incorporated to monolith
    test package with improved hash being used by using ``--transition``
    switch.


    .. index:: --transition (gnattest)

  :samp:`--transition`
    This allows transition from separate test routines to monolith test packages.
    All matching test routines are overwritten with contents of corresponding
    separates. Note that if separate test routines had any manually added with
    clauses they will be moved to the test package body as is and have to be moved
    by hand.


    .. index:: --test-duration (gnattest)

  :samp:`--test-duration`
    Adds time measurements for each test in generated test driver.


  :samp:`--tests_root`, ``--subdir`` and ``--tests-dir`` switches are mutually exclusive.


  .. _Switches_for_gnattest_in_test_execution_mode:

  Switches for *gnattest* in test execution mode
  ----------------------------------------------


    .. index:: --passed-tests (gnattest)

  :samp:`--passed-tests={val}`
    Specifies whether or not passed tests should be shown. `val` can be either
    "show" or "hide", "show" being the default.


    .. index:: --queues (gnattest)
    .. index:: -j (gnattest)

  :samp:`--queues={n}`, :samp:`-j{n}`
    Runs `n` tests in parallel (default is 1).


  .. _Project_Attributes_for_gnattest:

  Project Attributes for *gnattest*
  ---------------------------------

  Most of the command-line options can also be passed to the tool by adding
  special attributes to the project file. Those attributes should be put in
  package **Gnattest**. Here is the list of attributes:


  * ``Tests_Root``
       is used to select the same output mode as with the ``--tests-root`` option.
       This attribute cannot be used together with ``Subdir`` or ``Tests_Dir``.

  * ``Subdir``
       is used to select the same output mode as with the ``--subdir`` option.
       This attribute cannot be used together with ``Tests_Root`` or ``Tests_Dir``.

  * ``Tests_Dir``
       is used to select the same output mode as with the ``--tests-dir`` option.
       This attribute cannot be used together with ``Subdir`` or ``Tests_Root``.

  * ``Harness_Dir``
       is used to specify the directory in which to place harness packages and project
       file for the test driver, otherwise specified by ``--harness-dir``.

  * ``Additional_Tests``
       is used to specify the project file, otherwise given by
       ``--additional-tests`` switch.

  * ``Skeletons_Default``
       is used to specify the default behavior of test skeletons, otherwise
       specified by ``--skeleton-default`` option. The value of this attribute
       should be either ``pass`` or ``fail``.

  * ``Default_Stub_Exclusion_List``
       is used to specify the file with list of units whose bodies should not
       be stubbed, otherwise specified by ``--exclude-from-stubbing=filename``.

  * ``Stub_Exclusion_List ("unit")``
       is used to specify the file with list of units whose bodies should not
       be stubbed when testing "unit", otherwise specified by
       ``--exclude-from-stubbing:unit=filename``.

  Each of those attributes can be overridden from the command line if needed.
  Other *gnattest* switches can also be passed via the project
  file as an attribute list called *Gnattest_Switches*.


  .. _Simple_gnattest_Example:

  Simple Example
  --------------

  Let's take a very simple example using the first *gnattest* example
  located in:

    ::

        <install_prefix>/share/examples/gnattest/simple

  This project contains a simple package containing one subprogram. By running *gnattest*:

    ::

        $ gnattest --harness-dir=driver -Psimple.gpr

  a test driver is created in directory ``driver``. It can be compiled and run:

    ::

       $ cd obj/driver
       $ gnatmake -Ptest_driver
       $ test_runner

  One failed test with the diagnosis "test not implemented" is reported.
  Since no special output option was specified, the test package ``Simple.Tests``
  is located in:

    ::

        <install_prefix>/share/examples/gnattest/simple/obj/gnattest/tests


  For each package containing visible subprograms, a child test package is
  generated. It contains one test routine per tested subprogram. Each
  declaration of a test subprogram has a comment specifying which tested
  subprogram it corresponds to. Bodies of test routines are placed in test package
  bodies and are surrounded by special comment sections. Those comment sections
  should not be removed or modified in order for gnattest to be able to regenerate
  test packages and keep already written tests in place.
  The test routine `Test_Inc_5eaee3` located at ``simple-test_data-tests.adb`` contains
  a single statement: a call to procedure `Assert`. It has two arguments:
  the Boolean expression we want to check and the diagnosis message to display if
  the condition is false.

  That is where actual testing code should be written after a proper setup.
  An actual check can be performed by replacing the `Assert` call with:

    ::

        Assert (Inc (1) = 2, "wrong incrementation");

  After recompiling and running the test driver, one successfully passed test
  is reported.


  .. _Setting_Up_and_Tearing_Down_the_Testing_Environment:

  Setting Up and Tearing Down the Testing Environment
  ---------------------------------------------------

  Besides test routines themselves, each test package has a parent package
  `Test_Data` that has two procedures: `Set_Up` and `Tear_Down`. This package is never
  overwritten by the tool. `Set_Up` is called before each test routine of the
  package, and `Tear_Down` is called after each test routine. Those two procedures
  can be used to perform necessary initialization and finalization,
  memory allocation, etc. Test type declared in `Test_Data` package is parent type
  for the test type of test package and can have user-defined components whose
  values can be set by `Set_Up` routine and used in test routines afterwards.


  .. _Regenerating_Tests:

  Regenerating Tests
  ------------------

  Bodies of test routines and `Test_Data` packages are never overridden after they
  have been created once. As long as the name of the subprogram, full expanded Ada
  names and order of its parameters are the same, and comment sections are
  intact, the old test routine will fit in its place and no test skeleton will be
  generated for the subprogram.

  This can be demonstrated with the previous example. By uncommenting declaration
  and body of function Dec in ``simple.ads`` and ``simple.adb``, running
  *gnattest* on the project, and then running the test driver:

    ::

        $ gnattest --harness-dir=driver -Psimple.gpr
        $ cd obj/driver
        $ gprbuild -Ptest_driver
        $ test_runner

  The old test is not replaced with a stub, nor is it lost, but a new test
  skeleton is created for function `Dec`.

  The only way of regenerating tests skeletons is to remove the previously created
  tests together with corresponding comment sections.


  .. _Default_Test_Behavior:

  Default Test Behavior
  ---------------------

  The generated test driver can treat unimplemented tests in two ways:
  either count them all as failed (this is useful to see which tests are still
  left to implement) or as passed (to sort out unimplemented ones from those
  actually failing).

  The test driver accepts a switch to specify this behavior:
  :samp:`--skeleton-default={val}`, where ``val`` is either ``pass`` or ``fail`` (exactly as for
  *gnattest*).

  The default behavior of the test driver is set with the same switch
  as passed to *gnattest* when generating the test driver.

  Passing it to the driver generated on the first example:

    ::

        $ test_runner --skeleton-default=pass

  makes both tests pass, even the unimplemented one.


  .. _Testing_Primitive_Operations_of_Tagged_Types:

  Testing Primitive Operations of Tagged Types
  --------------------------------------------

  Creation of test skeletons for primitive operations of tagged types entails
  a number of features. Test routines for all primitives of a given tagged type
  are placed in a separate child package named according to the tagged type. For
  example, if you have tagged type *T* in package *P*, all tests for primitives
  of *T* will be in *P.T_Test_Data.T_Tests*.

  Consider running *gnattest* on the second example (note: actual tests for this
  example already exist, so there's no need to worry if the tool reports that
  no new stubs were generated):

    ::

        $ cd <install_prefix>/share/examples/gnattest/tagged_rec
        $ gnattest --harness-dir=driver -Ptagged_rec.gpr

  Taking a closer look at the test type declared in the test package
  *Speed1.Controller_Test_Data* is necessary. It is declared in:

    ::

        <install_prefix>/share/examples/gnattest/tagged_rec/obj/gnattest/tests

  Test types are direct or indirect descendants of
  *AUnit.Test_Fixtures.Test_Fixture* type. In the case of non-primitive tested
  subprograms, the user doesn't need to be concerned with them. However,
  when generating test packages for primitive operations, there are some things
  the user needs to know.

  Type *Test_Controller* has components that allow assignment of various
  derivations of type *Controller*. And if you look at the specification of
  package *Speed2.Auto_Controller*, you will see that *Test_Auto_Controller*
  actually derives from *Test_Controller* rather than AUnit type *Test_Fixture*.
  Thus, test types mirror the hierarchy of tested types.

  The *Set_Up* procedure of *Test_Data* package corresponding to a test package
  of primitive operations of type *T* assigns to *Fixture* a reference to an
  object of that exact type *T*. Note, however, that if the tagged type has
  discriminants, the *Set_Up* only has a commented template for setting
  up the fixture, since filling the discriminant with actual value is up
  to the user.

  The knowledge of the structure of test types allows additional testing
  without additional effort. Those possibilities are described below.


  .. _Testing_Inheritance:

  Testing Inheritance
  -------------------

  Since the test type hierarchy mimics the hierarchy of tested types, the
  inheritance of tests takes place. An example of such inheritance can be
  seen by running the test driver generated for the second example. As previously
  mentioned, actual tests are already written for this example.

    ::

        $ cd obj/driver
        $ gprbuild -Ptest_driver
        $ test_runner

  There are 6 passed tests while there are only 5 testable subprograms. The test
  routine for function Speed has been inherited and run against objects of the
  derived type.


  .. _Tagged_Type_Substitutability_Testing:

  Tagged Type Substitutability Testing
  ------------------------------------

  *Tagged Type Substitutability Testing* is a way of verifying the global type
  consistency by testing. Global type consistency is a principle stating that if
  *S* is a subtype of *T* (in Ada, *S* is a derived type of tagged type *T*),
  then objects of type *T* may be replaced with objects of type *S* (that is,
  objects of type *S* may be substituted for objects of type *T*), without
  altering any of the desirable properties of the program. When the properties
  of the program are expressed in the form of subprogram preconditions and
  postconditions (let's call them pre and post), the principle is formulated as
  relations between the pre and post of primitive operations and the pre and post
  of their derived operations. The pre of a derived operation should not be
  stronger than the original pre, and the post of the derived operation should
  not be weaker than the original post. Those relations ensure that verifying if
  a dispatching call is safe can be done just by using the pre and post of the
  root operation.

  Verifying global type consistency by testing consists of running all the unit
  tests associated with the primitives of a given tagged type with objects of its
  derived types.

  In the example used in the previous section, there was clearly a violation of
  type consistency. The overriding primitive *Adjust_Speed* in package *Speed2*
  removes the functionality of the overridden primitive and thus doesn't respect
  the consistency principle.
  *Gnattest* has a special option to run overridden parent tests against objects
  of the type which have overriding primitives:

    ::

        $ gnattest --harness-dir=driver --validate-type-extensions -Ptagged_rec.gpr
        $ cd obj/driver
        $ gprbuild -Ptest_driver
        $ test_runner

  While all the tests pass by themselves, the parent test for *Adjust_Speed* fails
  against objects of the derived type.

  Non-overridden tests are already inherited for derived test types, so the
  ``--validate-type-extensions`` enables the application of overridden tests
  to objects of derived types.


  .. _Testing_with_Contracts:

  Testing with Contracts
  ----------------------

  *gnattest* supports pragmas *Precondition*, *Postcondition*, and *Test_Case*,
  as well as the corresponding Ada 2012 aspects.
  Test routines are generated, one per each *Test_Case* associated with a tested
  subprogram. Those test routines have special wrappers for tested functions
  that have composition of pre- and postcondition of the subprogram with
  "requires" and "ensures" of the *Test_Case* (depending on the mode, pre and post
  either count for *Nominal* mode or do **not** count for *Robustness* mode).

  The third example demonstrates how this works:

    ::

        $ cd <install_prefix>/share/examples/gnattest/contracts
        $ gnattest --harness-dir=driver -Pcontracts.gpr

  Putting actual checks within the range of the contract does not cause any
  error reports. For example, for the test routine which corresponds to
  test case 1:

    ::

        Assert (Sqrt (9.0) = 3.0, "wrong sqrt");

  and for the test routine corresponding to test case 2:

    ::

        Assert (Sqrt (-5.0) = -1.0, "wrong error indication");

  are acceptable:

    ::

        $ cd obj/driver
        $ gprbuild -Ptest_driver
        $ test_runner

  However, by changing 9.0 to 25.0 and 3.0 to 5.0, for example, you can get
  a precondition violation for test case one. Also, by using any otherwise
  correct but positive pair of numbers in the second test routine, you can also
  get a precondition violation. Postconditions are checked and reported
  the same way.


  .. _Additional_Tests:

  Additional Tests
  ----------------

  *gnattest* can add user-written tests to the main suite of the test
  driver. *gnattest* traverses the given packages and searches for test
  routines. All procedures with a single in out parameter of a type which is
  derived from *AUnit.Test_Fixtures.Test_Fixture* and that are declared in package
  specifications are added to the suites and are then executed by the test driver.
  (*Set_Up* and *Tear_Down* are filtered out.)

  An example illustrates two ways of creating test harnesses for user-written
  tests. Directory `additional_tests` contains an AUnit-based test driver written
  by hand.

    ::

        <install_prefix>/share/examples/gnattest/additional_tests/

  To create a test driver for already-written tests, use the ``--harness-only``
  option:

    ::

        gnattest -Padditional/harness/harness.gpr --harness-dir=harness_only \\
          --harness-only
        gprbuild -Pharness_only/test_driver.gpr
        harness_only/test_runner

  Additional tests can also be executed together with generated tests:

    ::

        gnattest -Psimple.gpr --additional-tests=additional/harness/harness.gpr \\
          --harness-dir=mixing
        gprbuild -Pmixing/test_driver.gpr
        mixing/test_runner


  .. _Individual_Test_Drivers:

  Individual Test Drivers
  -----------------------

  By default, *gnattest* generates a monolithic test driver that
  aggregates the individual tests into a single executable. It is also possible
  to generate separate executables for each test or each unit under test, by
  passing the switch ``--separate-drivers`` with corresponding parameter. This
  approach scales better for large testing campaigns, especially involving target
  architectures with limited resources typical for embedded development. It can
  also provide a major performance benefit on multi-core systems by allowing
  simultaneous execution of multiple tests.

  *gnattest* can take charge of executing the individual tests; for this,
  instead of passing a project file, a text file containing the list of
  executables can be passed. Such a file is automatically generated by gnattest
  under the name :file:`test_drivers.list`, but it can be
  hand-edited to add or remove tests, or replaced. The individual tests can
  also be executed standalone, or from any user-defined scripted framework.


  .. _Stubbing:

  Stubbing
  --------

  Depending on the testing campaign, it is sometimes necessary to isolate the
  part of the algorithm under test from its dependencies. This is accomplished
  via *stubbing*, i.e. replacing the subprograms that are called from the
  subprogram under test by stand-in subprograms that match the profiles of the
  original ones, but simply return predetermined values required by the test
  scenario.

  This mode of test harness generation is activated by the switch ``--stub``.

  The implementation approach chosen by *gnattest* is as follows.
  For each package under consideration all the packages it is directly depending
  on are stubbed, excluding the generic packages and package instantiations.
  The stubs are shared for each package under test. The specs of packages to stub
  remain intact, while their bodies are replaced, and hide the original bodies by
  means of extending projects. Also, for each stubbed
  package, a child package with setter routines for each subprogram declaration
  is created. These setters are meant to be used to set the behavior of
  stubbed subprograms from within test cases.

  Note that subprograms belonging to the same package as the subprogram under
  test are not stubbed. This guarantees that the sources being tested are
  exactly the sources used for production, which is an important property for
  establishing the traceability between the testing campaign and production code.

  Due to the nature of stubbing process, this mode implies the switch
  ``--separate-drivers``, i.e. an individual test driver (with the
  corresponding hierarchy of extending projects) is generated for each unit under
  test.

  .. note::

     Developing a stubs-based testing campaign requires
     good understanding of the infrastructure created by *gnattest* for
     this purpose. We recommend following the two stubbing tutorials
     `simple_stubbing` and `advanced_stubbing` provided
     under :file:`<install_prefix>/share/examples/gnattest` before
     attempting to use this powerful feature.


  .. _Putting_Tests_under_Version_Control:

  Putting Tests under Version Control
  -----------------------------------

  As has been stated earlier, *gnattest* generates two different types
  of code, test skeletons and harness. The harness is generated completely
  automatically each time, does not require manual changes and therefore should
  not be put under version control.
  It makes sense to put under version control files containing test data packages,
  both specs and bodies, and files containing bodies of test packages. Note that
  test package specs are also generated automatically each time and should not be
  put under version control.
  Option ``--omit-sloc`` may be useful when putting test packages under version control.


  .. _Current_Limitations:

  Current Limitations
  -------------------

  The tool currently has the following limitations:

  * generic tests for nested generic packages and their instantiations are
    not supported;
  * tests for protected subprograms and entries are not supported;
  * pragma *No_Run_Time* is not supported;
  * pragma *No_Secondary_Stack* is not supported;
  * if pragmas for interfacing with foreign languages are used, manual
    adjustments might be necessary to make the test harness compilable;
  * use of some constructs, such as elaboration-control pragmas, Type_Invariant
    aspects, and complex variable initializations that use Subprogram'Access,
    may result in elaboration circularities in the generated harness.

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

   :samp:`-P{project_file}`
      Indicates the name of the project file whose source files are to
      be processed. The exact set of sources depends on other options
      specified, see below.

   :samp:`-U`
      If a project file is supplied, say for project ``proj``,
      but no sources are specified for ``proj`` (either by a
      project attribute or through a tool option that provides a list
      of the files to be used), process all the source files
      from projects imported either directly or indirectly by ``proj``.
      Otherwise this option has no effect.

   :samp:`-U {main_unit}`
      Similar to :samp:`-U`, but if no sources are specified then
      process only those source files for units in the closure of
      `main_unit`.

   :samp:`-X{name}={val}`
      Indicates that the external variable ``name`` in the project has the
      value ``val``. Has no effect if no project has been specified.

   :samp:`--subdirs={dir}`
      Use the `dir` subdirectory of the project's object directory (or the `dir`
      subdirectory of the project file directory if the project does not specify
      an object directory) for tool output files. Has no effect if no project
      has been specified or if :samp:`--no_objects_dir` is specified.

   :samp:`--no_objects_dir`
      Place all the result files into the current directory (i.e., the directory
      from which the tool invocation command is issued) instead of the project's
      object directory. Has no effect if no project has been specified.

   :samp:`-eL`
      Follow all symbolic links when processing project files.

   If a project file is specified and there is neither a :samp:`-U` option,
   nor a :samp:`-U {main_unit}` option, nor some other explicit option to
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
