.. _Tools_Supporting_Project_Files:

Tools Supporting Project Files
==============================

This section describes how project files can be used in conjunction with a number of
GNAT tools.

.. _gnatmake_and_Project_Files:

gnatmake and Project Files
--------------------------

This section covers several topics related to *gnatmake* and
project files: defining switches for *gnatmake*
and for the tools that it invokes; specifying configuration pragmas;
the use of the `Main` attribute; building and rebuilding library project
files.

.. _Switches_Related_to_Project_Files:

Switches Related to Project Files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following switches are used by GNAT tools that support project files:


  .. index:: -P (any project-aware tool)

:samp:`-P{project}`
  Indicates the name of a project file. This project file will be parsed with
  the verbosity indicated by *-vP*x**,
  if any, and using the external references indicated
  by *-X* switches, if any.
  There may zero, one or more spaces between *-P* and `project`.

  There must be only one *-P* switch on the command line.

  Since the Project Manager parses the project file only after all the switches
  on the command line are checked, the order of the switches
  *-P*,
  *-vP*x**
  or *-X* is not significant.


  .. index:: -X (any project-aware tool)

:samp:`-X{name}={value}`
  Indicates that external variable `name` has the value `value`.
  The Project Manager will use this value for occurrences of
  `external(name)` when parsing the project file.

  If `name` or `value` includes a space, then `name=value` should be
  put between quotes.

  ::

      -XOS=NT
      -X"user=John Doe"
    
  Several *-X* switches can be used simultaneously.
  If several *-X* switches specify the same
  `name`, only the last one is used.

  An external variable specified with a *-X* switch
  takes precedence over the value of the same name in the environment.


  .. index:: -vP (any project-aware tool)

:samp:`-vP{x}`
  Indicates the verbosity of the parsing of GNAT project files.

  *-vP0* means Default;
  *-vP1* means Medium;
  *-vP2* means High.

  The default is Default: no output for syntactically correct
  project files.
  If several *-vP*x** switches are present,
  only the last one is used.


  .. index:: -aP (any project-aware tool)

:samp:`-aP{dir}`
  Add directory `dir` at the beginning of the project search path, in order,
  after the current working directory.


  .. index:: -eL (any project-aware tool)

:samp:`-eL`
  Follow all symbolic links when processing project files.


  .. index:: --subdirs= (gnatmake and gnatclean)

:samp:`--subdirs={subdir}`
  This switch is recognized by *gnatmake* and *gnatclean*. It
  indicate that the real directories (except the source directories) are the
  subdirectories `subdir` of the directories specified in the project files.
  This applies in particular to object directories, library directories and
  exec directories. If the subdirectories do not exist, they are created
  automatically.


.. _Switches_and_Project_Files:

Switches and Project Files
^^^^^^^^^^^^^^^^^^^^^^^^^^

For each of the packages `Builder`, `Compiler`, `Binder`, and
`Linker`, you can specify a `Default_Switches`
attribute, a `Switches` attribute, or both;
as their names imply, these switch-related
attributes affect the switches that are used for each of these GNAT
components when
*gnatmake* is invoked.  As will be explained below, these
component-specific switches precede
the switches provided on the *gnatmake* command line.

The `Default_Switches` attribute is an attribute
indexed by language name (case insensitive) whose value is a string list.
For example:

  .. code-block:: gpr

     package Compiler is
       for Default_Switches ("Ada")
           use ("-gnaty",
                "-v");
     end Compiler;

The `Switches` attribute is indexed on a file name (which may or may
not be case sensitive, depending
on the operating system) whose value is a string list.  For example:

  .. code-block:: gpr

     package Builder is
        for Switches ("main1.adb")
            use ("-O2");
        for Switches ("main2.adb")
            use ("-g");
     end Builder;

For the `Builder` package, the file names must designate source files
for main subprograms.  For the `Binder` and `Linker` packages, the
file names must designate :file:`ALI` or source files for main subprograms.
In each case just the file name without an explicit extension is acceptable.

For each tool used in a program build (*gnatmake*, the compiler, the
binder, and the linker), the corresponding package @dfn{contributes} a set of
switches for each file on which the tool is invoked, based on the
switch-related attributes defined in the package.
In particular, the switches
that each of these packages contributes for a given file `f` comprise:

* the value of attribute `Switches (`f`)`,
  if it is specified in the package for the given file,
* otherwise, the value of `Default_Switches ("Ada")`,
  if it is specified in the package.

If neither of these attributes is defined in the package, then the package does
not contribute any switches for the given file.

When *gnatmake* is invoked on a file, the switches comprise
two sets, in the following order: those contributed for the file
by the `Builder` package;
and the switches passed on the command line.

When *gnatmake* invokes a tool (compiler, binder, linker) on a file,
the switches passed to the tool comprise three sets,
in the following order:

* the applicable switches contributed for the file
  by the `Builder` package in the project file supplied on the command line;

* those contributed for the file by the package (in the relevant project file --
  see below) corresponding to the tool; and

* the applicable switches passed on the command line.

The term *applicable switches* reflects the fact that
*gnatmake* switches may or may not be passed to individual
tools, depending on the individual switch.

*gnatmake* may invoke the compiler on source files from different
projects. The Project Manager will use the appropriate project file to
determine the `Compiler` package for each source file being compiled.
Likewise for the `Binder` and `Linker` packages.

As an example, consider the following package in a project file:


  .. code-block:: gpr

     project Proj1 is
        package Compiler is
           for Default_Switches ("Ada")
               use ("-g");
           for Switches ("a.adb")
               use ("-O1");
           for Switches ("b.adb")
               use ("-O2",
                    "-gnaty");
        end Compiler;
     end Proj1;
  
If *gnatmake* is invoked with this project file, and it needs to
compile, say, the files :file:`a.adb`, :file:`b.adb`, and :file:`c.adb`, then
:file:`a.adb` will be compiled with the switch *-O1*,
:file:`b.adb` with switches *-O2* and *-gnaty*,
and :file:`c.adb` with *-g*.

The following example illustrates the ordering of the switches
contributed by different packages:

  .. code-block:: gpr

     project Proj2 is
        package Builder is
           for Switches ("main.adb")
               use ("-g",
                    "-O1",
                    "-f");
        end Builder;

        package Compiler is
           for Switches ("main.adb")
               use ("-O2");
        end Compiler;
     end Proj2;

If you issue the command:

  ::

      $ gnatmake -Pproj2 -O0 main
  
then the compiler will be invoked on :file:`main.adb` with the following
sequence of switches

  ::

      -g -O1 -O2 -O0
  
with the last *-O*
switch having precedence over the earlier ones;
several other switches
(such as *-c*) are added implicitly.

The switches *-g*
and *-O1* are contributed by package
`Builder`,  *-O2* is contributed
by the package `Compiler`
and *-O0* comes from the command line.

The *-g* switch will also be passed in the invocation of
*Gnatlink.*

A final example illustrates switch contributions from packages in different
project files:

  .. code-block:: gpr
  
     project Proj3 is
        for Source_Files use ("pack.ads", "pack.adb");
        package Compiler is
           for Default_Switches ("Ada")
               use ("-gnata");
        end Compiler;
     end Proj3;

     with "Proj3";
     project Proj4 is
        for Source_Files use ("foo_main.adb", "bar_main.adb");
        package Builder is
           for Switches ("foo_main.adb")
               use ("-s",
                    "-g");
        end Builder;
     end Proj4;

  .. code-block:: ada

     -- Ada source file:
     with Pack;
     procedure Foo_Main is
        ...
     end Foo_Main;
  
If the command is

  ::

     $ gnatmake -PProj4 foo_main.adb -cargs -gnato
  
then the switches passed to the compiler for :file:`foo_main.adb` are
*-g* (contributed by the package `Proj4.Builder`) and
*-gnato* (passed on the command line).
When the imported package `Pack` is compiled, the switches used
are *-g* from `Proj4.Builder`,
*-gnata* (contributed from package `Proj3.Compiler`,
and *-gnato* from the command line.

When using *gnatmake* with project files, some switches or
arguments may be expressed as relative paths. As the working directory where
compilation occurs may change, these relative paths are converted to absolute
paths. For the switches found in a project file, the relative paths
are relative to the project file directory, for the switches on the command
line, they are relative to the directory where *gnatmake* is invoked.
The switches for which this occurs are:
-I,
-A,
-L,
-aO,
-aL,
-aI, as well as all arguments that are not switches (arguments to
switch
-o, object files specified in package `Linker` or after
-largs on the command line). The exception to this rule is the switch
--RTS= for which a relative path argument is never converted.

.. _Specifying_Configuration_Pragmas:

Specifying Configuration Pragmas
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When using *gnatmake* with project files, if there exists a file
:file:`gnat.adc` that contains configuration pragmas, this file will be
ignored.

Configuration pragmas can be defined by means of the following attributes in
project files: `Global_Configuration_Pragmas` in package `Builder`
and `Local_Configuration_Pragmas` in package `Compiler`.

Both these attributes are single string attributes. Their values is the path
name of a file containing configuration pragmas. If a path name is relative,
then it is relative to the project directory of the project file where the
attribute is defined.

When compiling a source, the configuration pragmas used are, in order,
those listed in the file designated by attribute
`Global_Configuration_Pragmas` in package `Builder` of the main
project file, if it is specified, and those listed in the file designated by
attribute `Local_Configuration_Pragmas` in package `Compiler` of
the project file of the source, if it exists.

.. _Project_Files_and_Main_Subprograms:

Project Files and Main Subprograms
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When using a project file, you can invoke *gnatmake*
with one or several main subprograms, by specifying their source files on the
command line.

  ::

      $ gnatmake -Pprj main1.adb main2.adb main3.adb

Each of these needs to be a source file of the same project, except
when the switch `-u` is used.

When `-u` is not used, all the mains need to be sources of the
same project, one of the project in the tree rooted at the project specified
on the command line. The package `Builder` of this common project, the
"main project" is the one that is considered by *gnatmake*.

When `-u` is used, the specified source files may be in projects
imported directly or indirectly by the project specified on the command line.
Note that if such a source file is not part of the project specified on the
command line, the switches found in package `Builder` of the
project specified on the command line, if any, that are transmitted
to the compiler will still be used, not those found in the project file of
the source file.

When using a project file, you can also invoke *gnatmake* without
explicitly specifying any main, and the effect depends on whether you have
defined the `Main` attribute.  This attribute has a string list value,
where each element in the list is the name of a source file (the file
extension is optional) that contains a unit that can be a main subprogram.

If the `Main` attribute is defined in a project file as a non-empty
string list and the switch *-u* is not used on the command
line, then invoking *gnatmake* with this project file but without any
main on the command line is equivalent to invoking *gnatmake* with all
the file names in the `Main` attribute on the command line.

Example:

  .. code-block:: gpr

     project Prj is
        for Main use ("main1.adb", "main2.adb", "main3.adb");
     end Prj;

With this project file, `"gnatmake -Pprj"`
is equivalent to
`"gnatmake -Pprj main1.adb main2.adb main3.adb"`.

When the project attribute `Main` is not specified, or is specified
as an empty string list, or when the switch *-u* is used on the command
line, then invoking *gnatmake* with no main on the command line will
result in all immediate sources of the project file being checked, and
potentially recompiled. Depending on the presence of the switch *-u*,
sources from other project files on which the immediate sources of the main
project file depend are also checked and potentially recompiled. In other
words, the *-u* switch is applied to all of the immediate sources of the
main project file.

When no main is specified on the command line and attribute `Main` exists
and includes several mains, or when several mains are specified on the
command line, the default switches in package `Builder` will
be used for all mains, even if there are specific switches
specified for one or several mains.

But the switches from package `Binder` or `Linker` will be
the specific switches for each main, if they are specified.

.. _Library_Project_Files:

Library Project Files
^^^^^^^^^^^^^^^^^^^^^

When *gnatmake* is invoked with a main project file that is a library
project file, it is not allowed to specify one or more mains on the command
line.

When a library project file is specified, switches `-b` and
`-l` have special meanings.

* `-b` is only allowed for stand-alone libraries. It indicates
  to *gnatmake* that *gnatbind* should be invoked for the
  library.

* `-l` may be used for all library projects. It indicates
  to *gnatmake* that the binder generated file should be compiled
  (in the case of a stand-alone library) and that the library should be built.


.. _The_GNAT_Driver_and_Project_Files:

The GNAT Driver and Project Files
---------------------------------

A number of GNAT tools beyond *gnatmake*
can benefit from project files:

.. only:: PRO or GPL

  * *gnatbind*
  * *gnatcheck*
  * *gnatclean*
  * *gnatelim*
  * *gnatfind*
  * *gnatlink*
  * *gnatls*
  * *gnatmetric*
  * *gnatpp*
  * *gnatstub*
  * *gnatxref*

.. only:: FSF

  * *gnatbind*
  * *gnatclean*
  * *gnatfind*
  * *gnatlink*
  * *gnatls*
  * *gnatxref*

However, none of these tools can be invoked
directly with a project file switch (*-P*).
They must be invoked through the *gnat* driver.

The *gnat* driver is a wrapper that accepts a number of commands and
calls the corresponding tool. It was designed initially for VMS platforms (to
convert VMS qualifiers to Unix-style switches), but it is now available on all
GNAT platforms.

On non-VMS platforms, the *gnat* driver accepts the following commands
(case insensitive):

.. only:: PRO or GPL

  * BIND to invoke *gnatbind*
  * CHOP to invoke *gnatchop*
  * CLEAN to invoke *gnatclean*
  * COMP or COMPILE to invoke the compiler
  * ELIM to invoke *gnatelim*
  * FIND to invoke *gnatfind*
  * KR or KRUNCH to invoke *gnatkr*
  * LINK to invoke *gnatlink*
  * LS or LIST to invoke *gnatls*
  * MAKE to invoke *gnatmake*
  * METRIC to invoke *gnatmetric*
  * NAME to invoke *gnatname*
  * PP or PRETTY to invoke *gnatpp*
  * PREP or PREPROCESS to invoke *gnatprep*
  * STUB to invoke *gnatstub*
  * XREF to invoke *gnatxref*

.. only:: FSF

  * BIND to invoke *gnatbind*
  * CHOP to invoke *gnatchop*
  * CLEAN to invoke *gnatclean*
  * COMP or COMPILE to invoke the compiler
  * FIND to invoke *gnatfind*
  * KR or KRUNCH to invoke *gnatkr*
  * LINK to invoke *gnatlink*
  * LS or LIST to invoke *gnatls*
  * MAKE to invoke *gnatmake*
  * NAME to invoke *gnatname*
  * PREP or PREPROCESS to invoke *gnatprep*
  * XREF to invoke *gnatxref*

Note that the command
*gnatmake -c -f -u* is used to invoke the compiler.

On non-VMS platforms, between *gnat* and the command, two
special switches may be used:

* *-v* to display the invocation of the tool.
* *-dn* to prevent the *gnat* driver from removing
  the temporary files it has created. These temporary files are
  configuration files and temporary file list files.

The command may be followed by switches and arguments for the invoked
tool.

  ::

     $ gnat bind -C main.ali
     $ gnat ls -a main
     $ gnat chop foo.txt
  
Switches may also be put in text files, one switch per line, and the text
files may be specified with their path name preceded by '@'.

  ::

     $ gnat bind @args.txt main.ali
  
In addition, for the following commands the project file related switches
(*-P*, *-X* and *-vPx*) may be used in addition to
the switches of the invoking tool:

.. only:: PRO or GPL

   * BIND
   * COMP or COMPILE 
   * FIND
   * ELIM
   * LS or LIST
   * LINK
   * METRIC
   * PP or PRETTY
   * STUB
   * XREF

.. only:: FSF

   * BIND
   * COMP or COMPILE 
   * FIND
   * LS or LIST
   * LINK
   * XREF

.. only:: PRO or GPL

   When GNAT PP or GNAT PRETTY is used with a project file, but with no source
   specified on the command line, it invokes *gnatpp* with all
   the immediate sources of the specified project file.

   When GNAT METRIC is used with a project file, but with no source
   specified on the command line, it invokes *gnatmetric*
   with all the immediate sources of the specified project file and with
   *-d* with the parameter pointing to the object directory
   of the project.

   In addition, when GNAT PP, GNAT PRETTY or GNAT METRIC is used with
   a project file, no source is specified on the command line and
   switch -U is specified on the command line, then
   the underlying tool (gnatpp or
   gnatmetric) is invoked for all sources of all projects,
   not only for the immediate sources of the main project.
   (-U stands for Universal or Union of the project files of the project tree)

For each of the following commands, there is optionally a corresponding
package in the main project.

.. only:: PRO or GPL

   * package `Binder` for command BIND (invoking `gnatbind`)
   * package `Check` for command CHECK (invoking `gnatcheck`)
   * package `Compiler` for command COMP or COMPILE (invoking the compiler)
   * package `Cross_Reference` for command XREF (invoking `gnatxref`)
   * package `Eliminate` for command ELIM (invoking `gnatelim`)
   * package `Finder` for command FIND (invoking `gnatfind`)
   * package `Gnatls` for command LS or LIST (invoking `gnatls`)
   * package `Gnatstub` for command STUB (invoking `gnatstub`)
   * package `Linker` for command LINK (invoking `gnatlink`)
   * package `Metrics` for command METRIC (invoking `gnatmetric`)
   * package `Pretty_Printer` for command PP or PRETTY (invoking `gnatpp`)

.. only:: FSF

   * package `Binder` for command BIND (invoking `gnatbind`)
   * package `Compiler` for command COMP or COMPILE (invoking the compiler)
   * package `Cross_Reference` for command XREF (invoking `gnatxref`)
   * package `Finder` for command FIND (invoking `gnatfind`)
   * package `Gnatls` for command LS or LIST (invoking `gnatls`)
   * package `Linker` for command LINK (invoking `gnatlink`)

Package `Gnatls` has a unique attribute `Switches`,
a simple variable with a string list value. It contains switches
for the invocation of `gnatls`.

  .. code-block:: gpr

     project Proj1 is
        package gnatls is
           for Switches
               use ("-a",
                    "-v");
        end gnatls;
     end Proj1;

All other packages have two attribute `Switches` and
`Default_Switches`.

`Switches` is an indexed attribute, indexed by the
source file name, that has a string list value: the switches to be
used when the tool corresponding to the package is invoked for the specific
source file.

`Default_Switches` is an attribute,
indexed by  the programming language that has a string list value.
`Default_Switches ("Ada")` contains the
switches for the invocation of the tool corresponding
to the package, except if a specific `Switches` attribute
is specified for the source file.

  .. code-block:: gpr

     project Proj is

        for Source_Dirs use ("");

        package gnatls is
           for Switches use
               ("-a",
                "-v");
        end gnatls;

        package Compiler is
           for Default_Switches ("Ada")
               use ("-gnatv",
                    "-gnatwa");
        end Binder;

        package Binder is
           for Default_Switches ("Ada")
               use ("-C",
                    "-e");
        end Binder;

        package Linker is
           for Default_Switches ("Ada")
               use ("-C");
           for Switches ("main.adb")
               use ("-C",
                    "-v",
                    "-v");
        end Linker;

        package Finder is
           for Default_Switches ("Ada")
                use ("-a",
                     "-f");
        end Finder;

        package Cross_Reference is
           for Default_Switches ("Ada")
               use ("-a",
                    "-f",
                    "-d",
                    "-u");
        end Cross_Reference;
     end Proj;
  
With the above project file, commands such as

  ::

     $ gnat comp -Pproj main
     $ gnat ls -Pproj main
     $ gnat xref -Pproj main
     $ gnat bind -Pproj main.ali
     $ gnat link -Pproj main.ali

will set up the environment properly and invoke the tool with the switches
found in the package corresponding to the tool:
`Default_Switches ("Ada")` for all tools,
except `Switches ("main.adb")`
for `gnatlink`.

.. only:: PRO or GPL

   It is also possible to invoke some of the tools,
   (`gnatcheck`,
   `gnatmetric`,
   and `gnatpp`)
   on a set of project units thanks to the combination of the switches
   *-P*, *-U* and possibly the main unit when one is interested
   in its closure. For instance,

     ::

        $ gnat metric -Pproj
 
   will compute the metrics for all the immediate units of project `proj`.

     ::

        $ gnat metric -Pproj -U
  
   will compute the metrics for all the units of the closure of projects
   rooted at `proj`.

     ::

        $ gnat metric -Pproj -U main_unit

   will compute the metrics for the closure of units rooted at
   `main_unit`. This last possibility relies implicitly
   on *gnatbind*'s option *-R*. But if the argument files for the
   tool invoked by the *gnat* driver are explicitly  specified
   either directly or through the tool *-files* option, then the tool
   is called only for these explicitly specified files.
