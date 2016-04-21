.. |with| replace:: *with*
.. |withs| replace:: *with*\ s
.. |withed| replace:: *with*\ ed
.. |withing| replace:: *with*\ ing

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit


.. _GNAT_Project_Manager:

********************
GNAT Project Manager
********************


.. _GNAT_Project_Manager_Introduction:

Introduction
============

This chapter describes GNAT's *Project Manager*, a facility that allows
you to manage complex builds involving a number of source files, directories,
and options for different system configurations. In particular,
project files allow you to specify:

* The directory or set of directories containing the source files, and/or the
  names of the specific source files themselves
* The directory in which the compiler's output
  (:file:`ALI` files, object files, tree files, etc.) is to be placed
* The directory in which the executable programs are to be placed
* Switch settings for any of the project-enabled tools;
  you can apply these settings either globally or to individual compilation units.
* The source files containing the main subprogram(s) to be built
* The source programming language(s)
* Source file naming conventions; you can specify these either globally or for
  individual compilation units (see :ref:`Naming_Schemes`).
* Change any of the above settings depending on external values, thus enabling
  the reuse of the projects in various **scenarios** (see :ref:`Scenarios_in_Projects`).
* Automatically build libraries as part of the build process
  (see :ref:`Library_Projects`).


Project files are written in a syntax close to that of Ada, using familiar
notions such as packages, context clauses, declarations, default values,
assignments, and inheritance (see :ref:`Project_File_Reference`).

Project files can be built hierarchically from other project files, simplifying
complex system integration and project reuse (see :ref:`Organizing_Projects_into_Subsystems`).

* One project can import other projects containing needed source files.
  More generally, the Project Manager lets you structure large development
  efforts into hierarchical subsystems, where build decisions are delegated
  to the subsystem level, and thus different compilation environments
  (switch settings) used for different subsystems.
* You can organize GNAT projects in a hierarchy: a child project
  can extend a parent project, inheriting the parent's source files and
  optionally overriding any of them with alternative versions
  (see :ref:`Project_Extension`).


Several tools support project files, generally in addition to specifying
the information on the command line itself). They share common switches
to control the loading of the project (in particular
:samp:`-P{projectfile}` and
:samp:`-X{vbl}={value}`).

The Project Manager supports a wide range of development strategies,
for systems of all sizes.  Here are some typical practices that are
easily handled:

* Using a common set of source files and generating object files in different
  directories via different switch settings. It can be used for instance, for
  generating separate sets of object files for debugging and for production.
* Using a mostly-shared set of source files with different versions of
  some units or subunits. It can be used for instance, for grouping and hiding
  all OS dependencies in a small number of implementation units.

Project files can be used to achieve some of the effects of a source
versioning system (for example, defining separate projects for
the different sets of sources that comprise different releases) but the
Project Manager is independent of any source configuration management tool
that might be used by the developers.

The various sections below introduce the different concepts related to
projects. Each section starts with examples and use cases, and then goes into
the details of related project file capabilities.

.. _Building_With_Projects:

Building With Projects
======================

In its simplest form, a unique project is used to build a single executable.
This section concentrates on such a simple setup. Later sections will extend
this basic model to more complex setups.

The following concepts are the foundation of project files, and will be further
detailed later in this documentation. They are summarized here as a reference.

**Project file**:
  A text file using an Ada-like syntax, generally using the :file:`.gpr`
  extension. It defines build-related characteristics of an application.
  The characteristics include the list of sources, the location of those
  sources, the location for the generated object files, the name of
  the main program, and the options for the various tools involved in the
  build process.


**Project attribute**:
  A specific project characteristic is defined by an attribute clause. Its
  value is a string or a sequence of strings. All settings in a project
  are defined through a list of predefined attributes with precise
  semantics. See :ref:`Attributes`.


**Package in a project**:
  Global attributes are defined at the top level of a project.
  Attributes affecting specific tools are grouped in a
  package whose name is related to tool's function. The most common
  packages are `Builder`, `Compiler`, `Binder`,
  and `Linker`. See :ref:`Packages`.


**Project variables**:
  In addition to attributes, a project can use variables to store intermediate
  values and avoid duplication in complex expressions. It can be initialized
  with a value coming from the environment.
  A frequent use of variables is to define scenarios.
  See :ref:`External_Values`, :ref:`Scenarios_in_Projects`, and :ref:`Variables`.


**Source files** and **source directories**:
  A source file is associated with a language through a naming convention. For
  instance, `foo.c` is typically the name of a C source file;
  `bar.ads` or `bar.1.ada` are two common naming conventions for a
  file containing an Ada spec. A compilation unit is often composed of a main
  source file and potentially several auxiliary ones, such as header files in C.
  The naming conventions can be user defined :ref:`Naming_Schemes`, and will
  drive the builder to call the appropriate compiler for the given source file.
  Source files are searched for in the source directories associated with the
  project through the **Source_Dirs** attribute. By default, all the files (in
  these source directories) following the naming conventions associated with the
  declared languages are considered to be part of the project. It is also
  possible to limit the list of source files using the **Source_Files** or
  **Source_List_File** attributes. Note that those last two attributes only
  accept basenames with no directory information.


**Object files** and **object directory**:
  An object file is an intermediate file produced by the compiler from a
  compilation unit. It is used by post-compilation tools to produce
  final executables or libraries. Object files produced in the context of
  a given project are stored in a single directory that can be specified by the
  **Object_Dir** attribute. In order to store objects in
  two or more object directories, the system must be split into
  distinct subsystems with their own project file.


The following subsections introduce gradually all the attributes of interest
for simple build needs. Here is the simple setup that will be used in the
following examples.

The Ada source files :file:`pack.ads`, :file:`pack.adb`, and :file:`proc.adb` are in
the :file:`common/` directory. The file :file:`proc.adb` contains an Ada main
subprogram `Proc` that |withs| package `Pack`. We want to compile
these source files with the switch
*-O2*, and put the resulting files in
the directory :file:`obj/`.

::

    common/
      pack.ads
      pack.adb
      proc.adb
    common/obj/
      proc.ali, proc.o pack.ali, pack.o


Our project is to be called *Build*. The name of the
file is the name of the project (case-insensitive) with the
:file:`.gpr` extension, therefore the project file name is :file:`build.gpr`. This
is not mandatory, but a warning is issued when this convention is not followed.

This is a very simple example, and as stated above, a single project
file is enough for it. We will thus create a new file, that for now
should contain the following code:

.. code-block:: gpr

      project Build is
      end Build;


.. _Source_Files_and_Directories:

Source Files and Directories
----------------------------

When you create a new project, the first thing to describe is how to find the
corresponding source files. These are the only settings that are needed by all
the tools that will use this project (builder, compiler, binder and linker for
the compilation, IDEs to edit the source files,...).

.. index:: Source directories (GNAT Project Manager)

The first step is to declare the source directories, which are the directories
to be searched to find source files. In the case of the example,
the :file:`common` directory is the only source directory.

.. index:: Source_Dirs (GNAT Project Manager)

There are several ways of defining source directories:

* When the attribute **Source_Dirs** is not used, a project contains a
  single source directory which is the one where the project file itself
  resides. In our example, if :file:`build.gpr` is placed in the :file:`common`
  directory, the project has the needed implicit source directory.

* The attribute **Source_Dirs** can be set to a list of path names, one
  for each of the source directories. Such paths can either be absolute
  names (for instance :file:`"/usr/local/common/"` on UNIX), or relative to the
  directory in which the project file resides (for instance "." if
  :file:`build.gpr` is inside :file:`common/`, or "common" if it is one level up).
  Each of the source directories must exist and be readable.

  .. index:: portability of path names (GNAT Project Manager)

  The syntax for directories is platform specific. For portability, however,
  the project manager will always properly translate UNIX-like path names to
  the native format of the specific platform. For instance, when the same
  project file is to be used both on Unix and Windows, "/" should be used as
  the directory separator rather than "\\".

* The attribute **Source_Dirs** can automatically include subdirectories
  using a special syntax inspired by some UNIX shells. If any of the paths in
  the list ends with ":file:`**`", then that path and all its subdirectories
  (recursively) are included in the list of source directories. For instance,
  :file:`**` and :file:`./**` represent the complete directory tree rooted at
  the directory in which the project file resides.

  .. index:: Source directories (GNAT Project Manager)

  .. index:: Excluded_Source_Dirs (GNAT Project Manager)

  When using that construct, it can sometimes be convenient to also use the
  attribute **Excluded_Source_Dirs**, which is also a list of paths. Each entry
  specifies a directory whose immediate content, not including subdirs, is to
  be excluded. It is also possible to exclude a complete directory subtree
  using the "**" notation.

  .. index:: Ignore_Source_Sub_Dirs (GNAT Project Manager)

  It is often desirable to remove, from the source directories, directory
  subtrees rooted at some subdirectories. An example is the subdirectories
  created by a Version Control System such as Subversion that creates directory
  subtrees rooted at subdirectories ".svn". To do that, attribute
  **Ignore_Source_Sub_Dirs** can be used. It specifies the list of simple
  file names for the roots of these undesirable directory subtrees.


  .. code-block:: gpr

        for Source_Dirs use ("./**");
        for Ignore_Source_Sub_Dirs use (".svn");


When applied to the simple example, and because we generally prefer to have
the project file at the toplevel directory rather than mixed with the sources,
we will create the following file


.. code-block:: gpr

     build.gpr
     project Build is
        for Source_Dirs use ("common");  --  <<<<
     end Build;


Once source directories have been specified, one may need to indicate
source files of interest. By default, all source files present in the source
directories are considered by the project manager. When this is not desired,
it is possible to specify the list of sources to consider explicitly.
In such a case, only source file base names are indicated and not
their absolute or relative path names. The project manager is in charge of
locating the specified source files in the specified source directories.

* By default, the project manager searches for all source files of all
  specified languages in all the source directories.

  Since the project manager was initially developed for Ada environments, the
  default language is usually Ada and the above project file is complete: it
  defines without ambiguity the sources composing the project: that is to say,
  all the sources in subdirectory "common" for the default language (Ada) using
  the default naming convention.

  .. index:: Languages (GNAT Project Manager)

  However, when compiling a multi-language application, or a pure C
  application, the project manager must be told which languages are of
  interest, which is done by setting the **Languages** attribute to a list of
  strings, each of which is the name of a language.

  .. index:: Naming scheme (GNAT Project Manager)

  Even when using only Ada, the default naming might not be suitable. Indeed,
  how does the project manager recognizes an "Ada file" from any other
  file? Project files can describe the naming scheme used for source files,
  and override the default (see :ref:`Naming_Schemes`). The default is the
  standard GNAT extension (:file:`.adb` for bodies and :file:`.ads` for
  specs), which is what is used in our example, explaining why no naming scheme
  is explicitly specified.
  See :ref:`Naming_Schemes`.

  .. index:: Source_Files (GNAT Project Manager)

* `Source_Files`.
  In some cases, source directories might contain files that should not be
  included in a project. One can specify the explicit list of file names to
  be considered through the **Source_Files** attribute.
  When this attribute is defined, instead of looking at every file in the
  source directories, the project manager takes only those names into
  consideration  reports  errors if they cannot be found in the source
  directories or does not correspond to the naming scheme.

* For various reasons, it is sometimes useful to have a project with no
  sources (most of the time because the attributes defined in the project
  file will be reused in other projects, as explained in
  :ref:`Organizing_Projects_into_Subsystems`. To do this, the attribute
  *Source_Files* is set to the empty list, i.e. `()`. Alternatively,
  *Source_Dirs* can be set to the empty list, with the same
  result.

  .. index:: Source_List_File (GNAT Project Manager)

* `Source_List_File`.
  If there is a great number of files, it might be more convenient to use
  the attribute **Source_List_File**, which specifies the full path of a file.
  This file must contain a list of source file names (one per line, no
  directory information) that are searched as if they had been defined
  through *Source_Files*. Such a file can easily be created through
  external tools.

  A warning is issued if both attributes `Source_Files` and
  `Source_List_File` are given explicit values. In this case, the
  attribute `Source_Files` prevails.

  .. index:: Excluded_Source_Files (GNAT Project Manager)
  .. index:: Locally_Removed_Files (GNAT Project Manager)
  .. index:: Excluded_Source_List_File (GNAT Project Manager)

* `Excluded_Source_Files`.
  Specifying an explicit list of files is not always convenient.It might be
  more convenient to use the default search rules with specific exceptions.
  This can be done thanks to the attribute **Excluded_Source_Files**
  (or its synonym **Locally_Removed_Files**).
  Its value is the list of file names that should not be taken into account.
  This attribute is often used when extending a project,
  see :ref:`Project_Extension`. A similar attribute
  **Excluded_Source_List_File** plays the same
  role but takes the name of file containing file names similarly to
  `Source_List_File`.


In most simple cases, such as the above example, the default source file search
behavior provides the expected result, and we do not need to add anything after
setting `Source_Dirs`. The project manager automatically finds
:file:`pack.ads`, :file:`pack.adb`, and :file:`proc.adb` as source files of the
project.

Note that by default a warning is issued when a project has no sources attached
to it and this is not explicitly indicated in the project file.

.. _Duplicate_Sources_in_Projects:

Duplicate Sources in Projects
-----------------------------

If the order of the source directories is known statically, that is if
`"/**"` is not used in the string list `Source_Dirs`, then there may
be several files with the same name sitting in different directories of the
project. In this case, only the file in the first directory is considered as a
source of the project and the others are hidden. If `"/**"` is used in the
string list `Source_Dirs`, it is an error to have several files with the
same name in the same directory `"/**"` subtree, since there would be an
ambiguity as to which one should be used. However, two files with the same name
may exist in two single directories or directory subtrees. In this case, the
one in the first directory or directory subtree is a source of the project.

If there are two sources in different directories of the same `"/**"`
subtree, one way to resolve the problem is to exclude the directory of the
file that should not be used as a source of the project.

.. _Object_and_Exec_Directory:

Object and Exec Directory
-------------------------

The next step when writing a project is to indicate where the compiler should
put the object files. In fact, the compiler and other tools might create
several different kind of files (for GNAT, there is the object file and the ALI
file for instance). One of the important concepts in projects is that most
tools may consider source directories as read-only and do not attempt to create
new or temporary files there. Instead, all files are created in the object
directory. It is of course not true for project-aware IDEs, whose purpose it is
to create the source files.

.. index:: Object_Dir (GNAT Project Manager)

The object directory is specified through the **Object_Dir** attribute.
Its value is the path to the object directory, either absolute or
relative to the directory containing the project file. This
directory must already exist and be readable and writable, although
some tools have a switch to create the directory if needed (See
the switch `-p` for *gprbuild*).

If the attribute `Object_Dir` is not specified, it defaults to
the project directory, that is the directory containing the project file.

For our example, we can specify the object dir in this way:

.. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "obj";   --  <<<<
       end Build;

As mentioned earlier, there is a single object directory per project. As a
result, if you have an existing system where the object files are spread across
several directories, you can either move all of them into the same directory if
you want to build it with a single project file, or study the section on
subsystems (see :ref:`Organizing_Projects_into_Subsystems`) to see how each
separate object directory can be associated with one of the subsystems
constituting the application.

When the *linker* is called, it usually creates an executable. By
default, this executable is placed in the object directory of the project. It
might be convenient to store it in its own directory.

.. index:: Exec_Dir (GNAT Project Manager)

This can be done through the `Exec_Dir` attribute, which, like
*Object_Dir* contains a single absolute or relative path and must point to
an existing and writable directory, unless you ask the tool to create it on
your behalf. When not specified, It defaults to the object directory and
therefore to the project file's directory if neither *Object_Dir* nor
*Exec_Dir* was specified.

In the case of the example, let's place the executable in the root
of the hierarchy, ie the same directory as :file:`build.gpr`. Hence
the project file is now

.. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "obj";
          for Exec_Dir use ".";  --   <<<<
       end Build;


.. _Main_Subprograms:

Main Subprograms
----------------

In the previous section, executables were mentioned. The project manager needs
to be taught what they are. In a project file, an executable is indicated by
pointing to the source file of a main subprogram. In C this is the file that
contains the `main` function, and in Ada the file that contains the main
unit.

There can be any number of such main files within a given project, and thus
several executables can be built in the context of a single project file. Of
course, one given executable might not (and in fact will not) need all the
source files referenced by the project. As opposed to other build environments
such as *makefile*, one does not need to specify the list of
dependencies of each executable, the project-aware builder knows enough of the
semantics of the languages to build and link only the necessary elements.

.. index:: Main (GNAT Project Manager)

The list of main files is specified via the **Main** attribute. It contains
a list of file names (no directories). If a project defines this
attribute, it is not necessary to identify  main files on the
command line when invoking a builder, and editors like
*GPS* will be able to create extra menus to spawn or debug the
corresponding executables.

.. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "obj";
          for Exec_Dir use ".";
          for Main use ("proc.adb");  --   <<<<
       end Build;


If this attribute is defined in the project, then spawning the builder
with a command such as

.. code-block:: sh

     gprbuild -Pbuild


automatically builds all the executables corresponding to the files
listed in the *Main* attribute. It is possible to specify one
or more executables on the command line to build a subset of them.

.. _Tools_Options_in_Project_Files:

Tools Options in Project Files
------------------------------

We now have a project file that fully describes our environment, and can be
used to build the application with a simple *gprbuild* command as seen
in the previous section. In fact, the empty project we showed immediately at
the beginning (with no attribute at all) could already fulfill that need if it
was put in the :file:`common` directory.

Of course, we might want more control. This section shows you how to specify
the compilation switches that the various tools involved in the building of the
executable should use.

.. index:: command line length (GNAT Project Manager)

Since source names and locations are described in the project file, it is not
necessary to use switches on the command line for this purpose (switches such
as -I for gcc). This removes a major source of command line length overflow.
Clearly, the builders will have to communicate this information one way or
another to the underlying compilers and tools they call but they usually use
response files for this and thus are not subject to command line overflows.

Several tools participate to the creation of an executable: the compiler
produces object files from the source files; the binder (in the Ada case)
creates a "source" file that takes care, among other things, of elaboration
issues and global variable initialization; and the linker gathers everything
into a single executable that users can execute. All these tools are known to
the project manager and will be called with user defined switches from the
project files. However, we need to introduce a new project file concept to
express the switches to be used for any of the tools involved in the build.

.. index:: project file packages (GNAT Project Manager)

A project file is subdivided into zero or more **packages**, each of which
contains the attributes specific to one tool (or one set of tools). Project
files use an Ada-like syntax for packages. Package names permitted in project
files are restricted to a predefined set (see :ref:`Packages`), and the contents
of packages are limited to a small set of constructs and attributes
(see :ref:`Attributes`).

Our example project file can be extended with the following empty packages. At
this stage, they could all be omitted since they are empty, but they show which
packages would be involved in the build process.

.. code-block:: gpr

       project Build is
          for Source_Dirs use ("common");
          for Object_Dir use "obj";
          for Exec_Dir use ".";
          for Main use ("proc.adb");

          package Builder is  --<<<  for gprbuild
          end Builder;

          package Compiler is --<<<  for the compiler
          end Compiler;

          package Binder is   --<<<  for the binder
          end Binder;

          package Linker is   --<<<  for the linker
          end Linker;
       end Build;

Let's first examine the compiler switches. As stated in the initial description
of the example, we want to compile all files with *-O2*. This is a
compiler switch, although it is usual, on the command line, to pass it to the
builder which then passes it to the compiler. It is recommended to use directly
the right package, which will make the setup easier to understand for other
people.

Several attributes can be used to specify the switches:

.. index:: Default_Switches (GNAT Project Manager)

**Default_Switches**:

  This is the first mention in this manual of an **indexed attribute**. When
  this attribute is defined, one must supply an *index* in the form of a
  literal string.
  In the case of *Default_Switches*, the index is the name of the
  language to which the switches apply (since a different compiler will
  likely be used for each language, and each compiler has its own set of
  switches). The value of the attribute is a list of switches.

  In this example, we want to compile all Ada source files with the switch
  *-O2*, and the resulting project file is as follows
  (only the `Compiler` package is shown):

  .. code-block:: gpr

       package Compiler is
         for Default_Switches ("Ada") use ("-O2");
       end Compiler;

.. index:: Switches (GNAT Project Manager)

**Switches**:

  In some cases, we might want to use specific switches
  for one or more files. For instance, compiling :file:`proc.adb` might not be
  possible at high level of optimization because of a compiler issue.
  In such a case, the *Switches*
  attribute (indexed on the file name) can be used and will override the
  switches defined by *Default_Switches*. Our project file would
  become:

  .. code-block:: gpr


      package Compiler is
         for Default_Switches ("Ada")
             use ("-O2");
         for Switches ("proc.adb")
             use ("-O0");
      end Compiler;


  `Switches` may take a pattern as an index, such as in:

  .. code-block:: gpr

      package Compiler is
        for Default_Switches ("Ada")
            use ("-O2");
        for Switches ("pkg*")
            use ("-O0");
      end Compiler;

  Sources :file:`pkg.adb` and :file:`pkg-child.adb` would be compiled with -O0,
  not -O2.

  `Switches` can also be given a language name as index instead of a file
  name in which case it has the same semantics as *Default_Switches*.
  However, indexes with wild cards are never valid for language name.


.. index:: Local_Configuration_Pragmas (GNAT Project Manager)

**Local_Configuration_Pragmas**:

  This attribute may specify the path
  of a file containing configuration pragmas for use by the Ada compiler,
  such as `pragma Restrictions (No_Tasking)`. These pragmas will be
  used for all the sources of the project.


The switches for the other tools are defined in a similar manner through the
**Default_Switches** and **Switches** attributes, respectively in the
*Builder* package (for *gprbuild*),
the *Binder* package (binding Ada executables) and the *Linker*
package (for linking executables).


.. _Compiling_with_Project_Files:

Compiling with Project Files
----------------------------

Now that our project files are written, let's build our executable.
Here is the command we would use from the command line:

.. code-block:: sh

     gprbuild -Pbuild

This will automatically build the executables specified through the
*Main* attribute: for each, it will compile or recompile the
sources for which the object file does not exist or is not up-to-date; it
will then run the binder; and finally run the linker to create the
executable itself.

The *gprbuild* builder, can automatically manage C files the
same way: create the file :file:`utils.c` in the :file:`common` directory,
set the attribute *Languages* to `"(Ada, C)"`, and re-run

.. code-block:: sh

     gprbuild -Pbuild

Gprbuild knows how to recompile the C files and will
recompile them only if one of their dependencies has changed. No direct
indication on how to build the various elements is given in the
project file, which describes the project properties rather than a
set of actions to be executed. Here is the invocation of
*gprbuild* when building a multi-language program:

.. code-block:: sh

    $ gprbuild -Pbuild
    gcc -c proc.adb
    gcc -c pack.adb
    gcc -c utils.c
    gprbind proc
    ...
    gcc proc.o -o proc

Notice the three steps described earlier:

* The first three gcc commands correspond to the compilation phase.
* The gprbind command corresponds to the post-compilation phase.
* The last gcc command corresponds to the final link.


.. index:: -v option (for GPRbuild)

The default output of GPRbuild's execution is kept reasonably simple and easy
to understand. In particular, some of the less frequently used commands are not
shown, and some parameters are abbreviated. So it is not possible to rerun the
effect of the *gprbuild* command by cut-and-pasting its output.
GPRbuild's option `-v` provides a much more verbose output which includes,
among other information, more complete compilation, post-compilation and link
commands.


.. _Executable_File_Names:

Executable File Names
---------------------

.. index:: Executable (GNAT Project Manager)

By default, the executable name corresponding to a main file is
computed from the main source file name. Through the attribute
**Builder.Executable**, it is possible to change this default.

For instance, instead of building *proc* (or *proc.exe*
on Windows), we could configure our project file to build "proc1"
(resp proc1.exe) with the following addition:

.. code-block:: gpr

       project Build is
          ...  --  same as before
          package Builder is
             for Executable ("proc.adb") use "proc1";
          end Builder
       end Build;

.. index:: Executable_Suffix (GNAT Project Manager)

Attribute **Executable_Suffix**, when specified, may change the suffix
of the executable files, when no attribute `Executable` applies:
its value replaces the platform-specific executable suffix.
The default executable suffix is empty on UNIX and ".exe" on Windows.

It is also possible to change the name of the produced executable by using the
command line switch *-o*. When several mains are defined in the project,
it is not possible to use the *-o* switch and the only way to change the
names of the executable is provided by Attributes `Executable` and
`Executable_Suffix`.


.. _Avoid_Duplication_With_Variables:

Avoid Duplication With Variables
--------------------------------

To illustrate some other project capabilities, here is a slightly more complex
project using similar sources and a main program in C:


.. code-block:: gpr

    project C_Main is
       for Languages    use ("Ada", "C");
       for Source_Dirs  use ("common");
       for Object_Dir   use  "obj";
       for Main         use ("main.c");
       package Compiler is
          C_Switches := ("-pedantic");
          for Default_Switches ("C")   use C_Switches;
          for Default_Switches ("Ada") use ("-gnaty");
          for Switches ("main.c") use C_Switches & ("-g");
       end Compiler;
    end C_Main;

This project has many similarities with the previous one.
As expected, its `Main` attribute now refers to a C source.
The attribute *Exec_Dir* is now omitted, thus the resulting
executable will be put in the directory :file:`obj`.

The most noticeable difference is the use of a variable in the
*Compiler* package to store settings used in several attributes.
This avoids text duplication, and eases maintenance (a single place to
modify if we want to add new switches for C files). We will revisit
the use of variables in the context of scenarios (see :ref:`Scenarios_in_Projects`).

In this example, we see how the file :file:`main.c` can be compiled with
the switches used for all the other C files, plus *-g*.
In this specific situation the use of a variable could have been
replaced by a reference to the `Default_Switches` attribute:

.. code-block:: gpr

       for Switches ("c_main.c") use Compiler'Default_Switches ("C") & ("-g");

Note the tick (*'*) used to refer to attributes defined in a package.

Here is the output of the GPRbuild command using this project:

.. code-block:: sh

    $ gprbuild -Pc_main
    gcc -c -pedantic -g main.c
    gcc -c -gnaty proc.adb
    gcc -c -gnaty pack.adb
    gcc -c -pedantic utils.c
    gprbind main.bexch
    ...
    gcc main.o -o main

The default switches for Ada sources,
the default switches for C sources (in the compilation of :file:`lib.c`),
and the specific switches for :file:`main.c` have all been taken into
account.


.. _Naming_Schemes:

Naming Schemes
--------------

Sometimes an Ada software system is ported from one compilation environment to
another (say GNAT), and the file are not named using the default GNAT
conventions. Instead of changing all the file names, which for a variety of
reasons might not be possible, you can define the relevant file naming scheme
in the **Naming** package of your project file.

The naming scheme has two distinct goals for the project manager: it
allows finding of source files when searching in the source
directories, and given a source file name it makes it possible to guess
the associated language, and thus the compiler to use.

Note that the use by the Ada compiler of pragmas Source_File_Name is not
supported when using project files. You must use the features described in this
paragraph. You can however specify other configuration pragmas.

The following attributes can be defined in package `Naming`:

.. index:: Casing (GNAT Project Manager)

**Casing**:

  Its value must be one of `"lowercase"` (the default if
  unspecified), `"uppercase"` or `"mixedcase"`. It describes the
  casing of file names with regards to the Ada unit name. Given an Ada unit
  My_Unit, the file name will respectively be :file:`my_unit.adb` (lowercase),
  :file:`MY_UNIT.ADB` (uppercase) or :file:`My_Unit.adb` (mixedcase).
  On Windows, file names are case insensitive, so this attribute is
  irrelevant.


.. index:: Dot_Replacement (GNAT Project Manager)

**Dot_Replacement**:

  This attribute specifies the string that should replace the "." in unit
  names. Its default value is `"-"` so that a unit
  `Parent.Child` is expected to be found in the file
  :file:`parent-child.adb`. The replacement string must satisfy the following
  requirements to avoid ambiguities in the naming scheme:

  * It must not be empty

  * It cannot start or end with an alphanumeric character

  * It cannot be a single underscore

  * It cannot start with an underscore followed by an alphanumeric

  * It cannot contain a dot `'.'` except if the entire string is `"."`

.. index:: Spec_Suffix (GNAT Project Manager)
.. index:: Specification_Suffix (GNAT Project Manager)

**Spec_Suffix** and **Specification_Suffix**:

  For Ada, these attributes give the suffix used in file names that contain
  specifications. For other languages, they give the extension for files
  that contain declaration (header files in C for instance). The attribute
  is indexed on the language.
  The two attributes are equivalent, but the latter is obsolescent.

  If the value of the attribute is the empty string, it indicates to the
  Project Manager that the only specifications/header files for the language
  are those specified with attributes `Spec` or
  `Specification_Exceptions`.

  If `Spec_Suffix ("Ada")` is not specified, then the default is
  `".ads"`.

  A non empty value must satisfy the following requirements:

  * It must include at least one dot

  * If `Dot_Replacement` is a single dot, then it cannot include
    more than one dot.

.. index:: Body_Suffix (GNAT Project Manager)
.. index:: Implementation_Suffix (GNAT Project Manager)

**Body_Suffix** and **Implementation_Suffix**:

  These attributes give the extension used for file names that contain
  code (bodies in Ada). They are indexed on the language. The second
  version is obsolescent and fully replaced by the first attribute.

  For each language of a project, one of these two attributes need to be
  specified, either in the project itself or in the configuration project file.

  If the value of the attribute is the empty string, it indicates to the
  Project Manager that the only source files for the language
  are those specified with attributes `Body` or
  `Implementation_Exceptions`.

  These attributes must satisfy the same requirements as `Spec_Suffix`.
  In addition, they must be different from any of the values in
  `Spec_Suffix`.
  If `Body_Suffix ("Ada")` is not specified, then the default is
  `".adb"`.

  If `Body_Suffix ("Ada")` and `Spec_Suffix ("Ada")` end with the
  same string, then a file name that ends with the longest of these two
  suffixes will be a body if the longest suffix is `Body_Suffix ("Ada")`
  or a spec if the longest suffix is `Spec_Suffix ("Ada")`.

  If the suffix does not start with a '.', a file with a name exactly equal to
  the suffix will also be part of the project (for instance if you define the
  suffix as `Makefile.in`, a file called :file:`Makefile.in` will be part
  of the project. This capability is usually not interesting when building.
  However, it might become useful when a project is also used to
  find the list of source files in an editor, like the GNAT Programming System
  (GPS).

.. index:: Separate_Suffix (GNAT Project Manager)

**Separate_Suffix**:

  This attribute is specific to Ada. It denotes the suffix used in file names
  that contain separate bodies. If it is not specified, then it defaults to
  same value as `Body_Suffix ("Ada")`.

  The value of this attribute cannot be the empty string.

  Otherwise, the same rules apply as for the
  `Body_Suffix` attribute. The only accepted index is "Ada".


**Spec** or **Specification**:

  .. index:: Spec (GNAT Project Manager)

  .. index:: Specification (GNAT Project Manager)

  This attribute `Spec` can be used to define the source file name for a
  given Ada compilation unit's spec. The index is the literal name of the Ada
  unit (case insensitive). The value is the literal base name of the file that
  contains this unit's spec (case sensitive or insensitive depending on the
  operating system). This attribute allows the definition of exceptions to the
  general naming scheme, in case some files do not follow the usual
  convention.

  When a source file contains several units, the relative position of the unit
  can be indicated. The first unit in the file is at position 1


  .. code-block:: gpr

       for Spec ("MyPack.MyChild") use "mypack.mychild.spec";
       for Spec ("top") use "foo.a" at 1;
       for Spec ("foo") use "foo.a" at 2;


.. index:: Body (GNAT Project Manager)

.. index:: Implementation (GNAT Project Manager)

**Body** or **Implementation**:

  These attribute play the same role as *Spec* for Ada bodies.


.. index:: Specification_Exceptions (GNAT Project Manager)

.. index:: Implementation_Exceptions (GNAT Project Manager)

**Specification_Exceptions** and **Implementation_Exceptions**:

  These attributes define exceptions to the naming scheme for languages
  other than Ada. They are indexed on the language name, and contain
  a list of file names respectively for headers and source code.


For example, the following package models the Apex file naming rules:

.. code-block:: gpr

     package Naming is
       for Casing               use "lowercase";
       for Dot_Replacement      use ".";
       for Spec_Suffix ("Ada")  use ".1.ada";
       for Body_Suffix ("Ada")  use ".2.ada";
     end Naming;


.. _Installation:

Installation
------------

After building an application or a library it is often required to
install it into the development environment. For instance this step is
required if the library is to be used by another application.
The *gprinstall* tool provides an easy way to install
libraries, executable or object code generated during the build. The
**Install** package can be used to change the default locations.

The following attributes can be defined in package `Install`:

.. index:: Active (GNAT Project Manager)

**Active**
  Whether the project is to be installed, values are `true`
  (default) or `false`.


.. index:: Artifacts (GNAT Project Manager)

**Artifacts**

  An array attribute to declare a set of files not part of the sources
  to be installed. The array discriminant is the directory where the
  file is to be installed. If a relative directory then Prefix (see
  below) is prepended. Note also that if the same file name occurs
  multiple time in the attribute list, the last one will be the one
  installed.


.. index:: Prefix (GNAT Project Manager)

**Prefix**:

  Root directory for the installation.


**Exec_Subdir**

  Subdirectory of **Prefix** where executables are to be
  installed. Default is **bin**.


**Lib_Subdir**

  Subdirectory of **Prefix** where directory with the library or object
  files is to be installed. Default is **lib**.


**Sources_Subdir**

  Subdirectory of **Prefix** where directory with sources is to be
  installed. Default is **include**.


**Project_Subdir**

  Subdirectory of **Prefix** where the generated project file is to be
  installed. Default is **share/gpr**.


**Mode**

  The installation mode, it is either **dev** (default) or **usage**.
  See **gprbuild** user's guide for details.


**Install_Name**

  Specify the name to use for recording the installation. The default is
  the project name without the extension.


.. _Distributed_support:

Distributed support
-------------------

For large projects the compilation time can become a limitation in
the development cycle. To cope with that, GPRbuild supports
distributed compilation.

The following attributes can be defined in package `Remote`:

.. index:: Root_Dir (GNAT Project Manager)

**Root_Dir**:

  Root directory of the project's sources. The default value is the
  project's directory.


.. _Organizing_Projects_into_Subsystems:

Organizing Projects into Subsystems
===================================

A **subsystem** is a coherent part of the complete system to be built. It is
represented by a set of sources and one single object directory. A system can
be composed of a single subsystem when it is simple as we have seen in the
first section. Complex systems are usually composed of several interdependent
subsystems. A subsystem is dependent on another subsystem if knowledge of the
other one is required to build it, and in particular if visibility on some of
the sources of this other subsystem is required. Each subsystem is usually
represented by its own project file.

In this section, the previous example is being extended. Let's assume some
sources of our `Build` project depend on other sources.
For instance, when building a graphical interface, it is usual to depend upon
a graphical library toolkit such as GtkAda. Furthermore, we also need
sources from a logging module we had previously written.

.. _Project_Dependencies:

Project Dependencies
--------------------

GtkAda comes with its own project file (appropriately called
:file:`gtkada.gpr`), and we will assume we have already built a project
called :file:`logging.gpr` for the logging module. With the information provided
so far in :file:`build.gpr`, building the application would fail with an error
indicating that the gtkada and logging units that are relied upon by the sources
of this project cannot be found.

This is solved by adding the following **with** clauses at the beginning of our
project:

.. code-block:: gpr

     with "gtkada.gpr";
     with "a/b/logging.gpr";
     project Build is
       ...  --  as before
     end Build;


.. index:: Externally_Built (GNAT Project Manager)

When such a project is compiled, *gprbuild* will automatically check
the other projects and recompile their sources when needed. It will also
recompile the sources from `Build` when needed, and finally create the
executable. In some cases, the implementation units needed to recompile a
project are not available, or come from some third party and you do not want to
recompile it yourself. In this case, set the attribute **Externally_Built** to
"true", indicating to the builder that this project can be assumed to be
up-to-date, and should not be considered for recompilation. In Ada, if the
sources of this externally built project were compiled with another version of
the compiler or with incompatible options, the binder will issue an error.

The project's |with| clause has several effects. It provides source
visibility between projects during the compilation process. It also guarantees
that the necessary object files from `Logging` and `GtkAda` are
available when linking `Build`.

As can be seen in this example, the syntax for importing projects is similar
to the syntax for importing compilation units in Ada. However, project files
use literal strings instead of names, and the |with| clause identifies
project files rather than packages.

Each literal string after |with| is the path
(absolute or relative) to a project file. The `.gpr` extension is
optional, although we recommend adding it. If no extension is specified,
and no project file with the :file:`.gpr` extension is found, then
the file is searched for exactly as written in the |with| clause,
that is with no extension.

As mentioned above, the path after a |with| has to be a literal
string, and you cannot use concatenation, or lookup the value of external
variables to change the directories from which a project is loaded.
A solution if you need something like this is to use aggregate projects
(see :ref:`Aggregate_Projects`).

.. index:: project path (GNAT Project Manager)

When a relative path or a base name is used, the
project files are searched relative to each of the directories in the
**project path**. This path includes all the directories found with the
following algorithm, in this order; the first matching file is used:

* First, the file is searched relative to the directory that contains the
  current project file.

  .. index:: GPR_PROJECT_PATH_FILE (GNAT Project Manager)
  .. index:: GPR_PROJECT_PATH (GNAT Project Manager)
  .. index:: ADA_PROJECT_PATH (GNAT Project Manager)

* Then it is searched relative to all the directories specified in the
  environment variables **GPR_PROJECT_PATH_FILE**,
  **GPR_PROJECT_PATH** and **ADA_PROJECT_PATH** (in that order) if they exist.
  The value of **GPR_PROJECT_PATH_FILE**, when defined, is the path name of
  a text file that contains project directory path names, one per line.
  **GPR_PROJECT_PATH** and **ADA_PROJECT_PATH**, when defined, contain
  project directory path names separated by directory separators.
  **ADA_PROJECT_PATH** is used for compatibility, it is recommended to
  use **GPR_PROJECT_PATH_FILE** or **GPR_PROJECT_PATH**.

* Finally, it is searched relative to the default project directories.
  Such directories depend on the tool used. The locations searched in the
  specified order are:

  * :file:`<prefix>/<target>/lib/gnat` if option *--target* is specified
  * :file:`<prefix>/<target>/share/gpr` if option *--target* is specified
  * :file:`<prefix>/share/gpr/`
  * :file:`<prefix>/lib/gnat/`

  In our example, :file:`gtkada.gpr` is found in the predefined directory if
  it was installed at the same root as GNAT.

Some tools also support extending the project path from the command line,
generally through the *-aP*. You can see the value of the project
path by using the *gnatls -v* command.

Any symbolic link will be fully resolved in the directory of the
importing project file before the imported project file is examined.

Any source file in the imported project can be used by the sources of the
importing project, transitively.
Thus if `A` imports `B`, which imports `C`, the sources of
`A` may depend on the sources of `C`, even if `A` does not
import `C` explicitly. However, this is not recommended, because if
and when `B` ceases to import `C`, some sources in `A` will
no longer compile. *gprbuild* has a switch *--no-indirect-imports*
that will report such indirect dependencies.

.. note::

   One very important aspect of a project hierarchy is that
   **a given source can only belong to one project** (otherwise the project manager
   would not know which settings apply to it and when to recompile it). It means
   that different project files do not usually share source directories or
   when they do, they need to specify precisely which project owns which sources
   using attribute `Source_Files` or equivalent. By contrast, 2 projects
   can each own a source with the same base file name as long as they live in
   different directories. The latter is not true for Ada Sources because of the
   correlation between source files and Ada units.

.. _Cyclic_Project_Dependencies:

Cyclic Project Dependencies
---------------------------

Cyclic dependencies are mostly forbidden:
if `A` imports `B` (directly or indirectly) then `B`
is not allowed to import `A`. However, there are cases when cyclic
dependencies would be beneficial. For these cases, another form of import
between projects exists: the **limited with**.  A project `A` that
imports a project `B` with a straight |with| may also be imported,
directly or indirectly, by `B` through a `limited with`.

The difference between straight |with| and `limited with` is that
the name of a project imported with a `limited with` cannot be used in the
project importing it. In particular, its packages cannot be renamed and
its variables cannot be referred to.

.. code-block:: gpr

     with "b.gpr";
     with "c.gpr";
     project A is
         for Exec_Dir use B'Exec_Dir; -- ok
     end A;

     limited with "a.gpr";   --  Cyclic dependency: A -> B -> A
     project B is
        for Exec_Dir use A'Exec_Dir; -- not ok
     end B;

     with "d.gpr";
     project C is
     end C;

     limited with "a.gpr";  --  Cyclic dependency: A -> C -> D -> A
     project D is
        for Exec_Dir use A'Exec_Dir; -- not ok
     end D;


.. _Sharing_Between_Projects:

Sharing Between Projects
------------------------

When building an application, it is common to have similar needs in several of
the projects corresponding to the subsystems under construction. For instance,
they will all have the same compilation switches.

As seen before (see :ref:`Tools_Options_in_Project_Files`), setting compilation
switches for all sources of a subsystem is simple: it is just a matter of
adding a `Compiler.Default_Switches` attribute to each project files with
the same value. Of course, that means duplication of data, and both places need
to be changed in order to recompile the whole application with different
switches. It can become a real problem if there are many subsystems and thus
many project files to edit.

There are two main approaches to avoiding this duplication:

* Since :file:`build.gpr` imports :file:`logging.gpr`, we could change it
  to reference the attribute in Logging, either through a package renaming,
  or by referencing the attribute. The following example shows both cases:

  .. code-block:: gpr

      project Logging is
         package Compiler is
            for Switches ("Ada")
                use ("-O2");
         end Compiler;
         package Binder is
            for Switches ("Ada")
                use ("-E");
         end Binder;
      end Logging;

      with "logging.gpr";
      project Build is
         package Compiler renames Logging.Compiler;
         package Binder is
            for Switches ("Ada") use Logging.Binder'Switches ("Ada");
         end Binder;
      end Build;

  The solution used for `Compiler` gets the same value for all
  attributes of the package, but you cannot modify anything from the
  package (adding extra switches or some exceptions). The second
  version is more flexible, but more verbose.

  If you need to refer to the value of a variable in an imported
  project, rather than an attribute, the syntax is similar but uses
  a "." rather than an apostrophe. For instance:

  .. code-block:: gpr

      with "imported";
      project Main is
         Var1 := Imported.Var;
      end Main;

* The second approach is to define the switches in a third project.
  That project is set up without any sources (so that, as opposed to
  the first example, none of the project plays a special role), and
  will only be used to define the attributes. Such a project is
  typically called :file:`shared.gpr`.

  .. code-block:: gpr

      abstract project Shared is
         for Source_Files use ();   --  no sources
         package Compiler is
            for Switches ("Ada")
                use ("-O2");
         end Compiler;
      end Shared;

      with "shared.gpr";
      project Logging is
         package Compiler renames Shared.Compiler;
      end Logging;

      with "shared.gpr";
      project Build is
         package Compiler renames Shared.Compiler;
      end Build;

  As for the first example, we could have chosen to set the attributes
  one by one rather than to rename a package. The reason we explicitly
  indicate that `Shared` has no sources is so that it can be created
  in any directory and we are sure it shares no sources with `Build`
  or `Logging`, which of course would be invalid.

  .. index:: project qualifier (GNAT Project Manager)

  Note the additional use of the **abstract** qualifier in :file:`shared.gpr`.
  This qualifier is optional, but helps convey the message that we do not
  intend this project to have sources (see :ref:`Qualified_Projects` for
  more qualifiers).


.. _Global_Attributes:

Global Attributes
-----------------

We have already seen many examples of attributes used to specify a special
option of one of the tools involved in the build process. Most of those
attributes are project specific. That it to say, they only affect the invocation
of tools on the sources of the project where they are defined.

There are a few additional attributes that apply to all projects in a
hierarchy as long as they are defined on the "main" project.
The main project is the project explicitly mentioned on the command-line.
The project hierarchy is the "with"-closure of the main project.

Here is a list of commonly used global attributes:

.. index:: Global_Configuration_Pragmas (GNAT Project Manager)

**Builder.Global_Configuration_Pragmas**:

  This attribute points to a file that contains configuration pragmas
  to use when building executables. These pragmas apply for all
  executables built from this project hierarchy. As we have seen before,
  additional pragmas can be specified on a per-project basis by setting the
  `Compiler.Local_Configuration_Pragmas` attribute.

.. index:: Global_Compilation_Switches (GNAT Project Manager)

**Builder.Global_Compilation_Switches**:

  This attribute is a list of compiler switches to use when compiling any
  source file in the project hierarchy. These switches are used in addition
  to the ones defined in the `Compiler` package, which only apply to
  the sources of the corresponding project. This attribute is indexed on
  the name of the language.

Using such global capabilities is convenient. It can also lead to unexpected
behavior. Especially when several subsystems are shared among different main
projects and the different global attributes are not
compatible. Note that using aggregate projects can be a safer and more powerful
replacement to global attributes.

.. _Scenarios_in_Projects:

Scenarios in Projects
=====================

Various aspects of the projects can be modified based on **scenarios**. These
are user-defined modes that change the behavior of a project. Typical
examples are the setup of platform-specific compiler options, or the use of
a debug and a release mode (the former would activate the generation of debug
information, while the second will focus on improving code optimization).

Let's enhance our example to support debug and release modes. The issue is to
let the user choose what kind of system he is building: use *-g* as
compiler switches in debug mode and *-O2* in release mode. We will also
set up the projects so that we do not share the same object directory in both
modes; otherwise switching from one to the other might trigger more
recompilations than needed or mix objects from the two modes.

One naive approach is to create two different project files, say
:file:`build_debug.gpr` and :file:`build_release.gpr`, that set the appropriate
attributes as explained in previous sections. This solution does not scale
well, because in the presence of multiple projects depending on each other, you
will also have to duplicate the complete hierarchy and adapt the project files
to point to the right copies.

.. index:: scenarios (GNAT Project Manager)

Instead, project files support the notion of scenarios controlled
by external values. Such values can come from several sources (in decreasing
order of priority):

.. index:: -X (usage with GNAT Project Manager)

**Command line**:
  When launching *gprbuild*, the user can pass
  extra *-X* switches to define the external value. In
  our case, the command line might look like

  .. code-block:: sh

         gprbuild -Pbuild.gpr -Xmode=release


**Environment variables**:
  When the external value does not come from the command line, it can come from
  the value of environment variables of the appropriate name.
  In our case, if an environment variable called "mode"
  exists, its value will be taken into account.



.. index:: external (GNAT Project Manager)

**External function second parameter**.

We now need to get that value in the project. The general form is to use
the predefined function **external** which returns the current value of
the external. For instance, we could set up the object directory to point to
either :file:`obj/debug` or :file:`obj/release` by changing our project to

.. code-block:: gpr

     project Build is
         for Object_Dir use "obj/" & external ("mode", "debug");
         ... --  as before
     end Build;

The second parameter to `external` is optional, and is the default
value to use if "mode" is not set from the command line or the environment.

In order to set the switches according to the different scenarios, other
constructs have to be introduced such as typed variables and case constructions.

.. index:: typed variable (GNAT Project Manager)
.. index:: case construction (GNAT Project Manager)

A **typed variable** is a variable that
can take only a limited number of values, similar to an enumeration in Ada.
Such a variable can then be used in a **case construction** and create conditional
sections in the project. The following example shows how this can be done:

.. code-block:: gpr

     project Build is
        type Mode_Type is ("debug", "release");  --  all possible values
        Mode : Mode_Type := external ("mode", "debug"); -- a typed variable

        package Compiler is
           case Mode is
              when "debug" =>
                 for Switches ("Ada")
                     use ("-g");
              when "release" =>
                 for Switches ("Ada")
                     use ("-O2");
           end case;
        end Compiler;
     end Build;

The project has suddenly grown in size, but has become much more flexible.
`Mode_Type` defines the only valid values for the `mode` variable. If
any other value is read from the environment, an error is reported and the
project is considered as invalid.

The `Mode` variable is initialized with an external value
defaulting to `"debug"`. This default could be omitted and that would
force the user to define the value. Finally, we can use a case construction to set the
switches depending on the scenario the user has chosen.

Most aspects of the projects can depend on scenarios. The notable exception
are project dependencies (|with| clauses), which cannot depend on a scenario.

Scenarios work the same way with **project hierarchies**: you can either
duplicate a variable similar to `Mode` in each of the project (as long
as the first argument to `external` is always the same and the type is
the same), or simply set the variable in the :file:`shared.gpr` project
(see :ref:`Sharing_Between_Projects`).


.. _Library_Projects:

Library Projects
================

So far, we have seen examples of projects that create executables. However,
it is also possible to create libraries instead. A **library** is a specific
type of subsystem where, for convenience, objects are grouped together
using system-specific means such as archives or windows DLLs.

Library projects provide a system- and language-independent way of building
both **static** and **dynamic** libraries. They also support the concept of
**standalone libraries** (SAL) which offer two significant properties: the
elaboration (e.g. initialization) of the library is either automatic or
very simple; a change in the
implementation part of the library implies minimal post-compilation actions on
the complete system and potentially no action at all for the rest of the
system in the case of dynamic SALs.

There is a restriction on shared library projects: by default, they are only
allowed to import other shared library projects. They are not allowed to
import non library projects or static library projects.

The GNAT Project Manager takes complete care of the library build, rebuild and
installation tasks, including recompilation of the source files for which
objects do not exist or are not up to date, assembly of the library archive, and
installation of the library (i.e., copying associated source, object and
:file:`ALI` files to the specified location).


.. _Building_Libraries:

Building Libraries
------------------

Let's enhance our example and transform the `logging` subsystem into a
library.  In order to do so, a few changes need to be made to
:file:`logging.gpr`.  Some attributes need to be defined: at least
`Library_Name` and `Library_Dir`; in addition, some other attributes
can be used to specify specific aspects of the library. For readability, it is
also recommended (although not mandatory), to use the qualifier `library`
in front of the `project` keyword.

.. index:: Library_Name (GNAT Project Manager)

**Library_Name**:

  This attribute is the name of the library to be built. There is no
  restriction on the name of a library imposed by the project manager, except
  for stand-alone libraries whose names must follow the syntax of Ada
  identifiers; however, there may be system-specific restrictions on the name.
  In general, it is recommended to stick to alphanumeric characters (and
  possibly single underscores) to help portability.

.. index:: Library_Dir (GNAT Project Manager)

**Library_Dir**:

  This attribute  is the path (absolute or relative) of the directory where
  the library is to be installed. In the process of building a library,
  the sources are compiled, the object files end up  in the explicit or
  implicit `Object_Dir` directory. When all sources of a library
  are compiled, some of the compilation artifacts, including the library itself,
  are copied to the library_dir directory. This directory must exist and be
  writable. It must also be different from the object directory so that cleanup
  activities in the Library_Dir do not affect recompilation needs.

Here is the new version of :file:`logging.gpr` that makes it a library:

.. code-block:: gpr

     library project Logging is          --  "library" is optional
        for Library_Name use "logging";  --  will create "liblogging.a" on Unix
        for Object_Dir   use "obj";
        for Library_Dir  use "lib";      --  different from object_dir
     end Logging;

Once the above two attributes are defined, the library project is valid and
is enough for building a library with default characteristics.
Other library-related attributes can be used to change the defaults:

.. index:: Library_Kind (GNAT Project Manager)

**Library_Kind**:

  The value of this attribute must be either `"static"`, `"dynamic"` or
  `"relocatable"` (the latter is a synonym for dynamic). It indicates
  which kind of library should be built (the default is to build a
  static library, that is an archive of object files that can potentially
  be linked into a static executable). When the library is set to be dynamic,
  a separate image is created that will be loaded independently, usually
  at the start of the main program execution. Support for dynamic libraries is
  very platform specific, for instance on Windows it takes the form of a DLL
  while on GNU/Linux, it is a dynamic elf image whose suffix is usually
  :file:`.so`. Library project files, on the other hand, can be written in
  a platform independent way so that the same project file can be used to build
  a library on different operating systems.

  If you need to build both a static and a dynamic library, it is recommended
  to use two different object directories, since in some cases some extra code
  needs to be generated for the latter. For such cases, one can either define
  two different project files, or a single one that uses scenarios to indicate
  the various kinds of library to be built and their corresponding object_dir.

.. index:: Library_ALI_Dir (GNAT Project Manager)

**Library_ALI_Dir**:

  This attribute may be specified to indicate the directory where the ALI
  files of the library are installed. By default, they are copied into the
  `Library_Dir` directory, but as for the executables where we have a
  separate `Exec_Dir` attribute, you might want to put them in a separate
  directory since there can be hundreds of them. The same restrictions as for
  the `Library_Dir` attribute apply.

.. index:: Library_Version (GNAT Project Manager)

**Library_Version**:

  This attribute is platform dependent, and has no effect on Windows.
  On Unix, it is used only for dynamic libraries as the internal
  name of the library (the `"soname"`). If the library file name (built
  from the `Library_Name`) is different from the `Library_Version`,
  then the library file will be a symbolic link to the actual file whose name
  will be `Library_Version`. This follows the usual installation schemes
  for dynamic libraries on many Unix systems.

  .. code-block:: gpr

      project Logging is
         Version := "1";
         for Library_Dir use "lib";
         for Library_Name use "logging";
         for Library_Kind use "dynamic";
         for Library_Version use "liblogging.so." & Version;
      end Logging;


  After the compilation, the directory :file:`lib` will contain both a
  :file:`libdummy.so.1` library and a symbolic link to it called
  :file:`libdummy.so`.

.. index:: Library_GCC (GNAT Project Manager)

**Library_GCC**:

  This attribute is the name of the tool to use instead of "gcc" to link shared
  libraries. A common use of this attribute is to define a wrapper script that
  accomplishes specific actions before calling gcc (which itself calls the
  linker to build the library image).

.. index:: Library_Options (GNAT Project Manager)

**Library_Options**:

  This attribute may be used to specify additional switches (last switches)
  when linking a shared library.

  It may also be used to add foreign object files to a static library.
  Each string in Library_Options is an absolute or relative path of an object
  file. When a relative path, it is relative to the object directory.

.. index:: Leading_Library_Options (GNAT Project Manager)

**Leading_Library_Options**:

  This attribute, that is taken into account only by *gprbuild*, may be
  used to specified leading options (first switches) when linking a shared
  library.

.. index:: Linker_Options (GNAT Project Manager)

**Linker.Linker_Options**:

  This attribute specifies additional switches to be given to the linker when
  linking an executable. It is ignored when defined in the main project and
  taken into account in all other projects that are imported directly or
  indirectly. These switches complement the `Linker.Switches`
  defined in the main project. This is useful when a particular subsystem
  depends on an external library: adding this dependency as a
  `Linker_Options` in the project of the subsystem is more convenient than
  adding it to all the `Linker.Switches` of the main projects that depend
  upon this subsystem.


.. _Using_Library_Projects:

Using Library Projects
----------------------

When the builder detects that a project file is a library project file, it
recompiles all sources of the project that need recompilation and rebuild the
library if any of the sources have been recompiled. It then groups all object
files into a single file, which is a shared or a static library. This library
can later on be linked with multiple executables. Note that the use
of shard libraries reduces the size of the final executable and can also reduce
the memory footprint at execution time when the library is shared among several
executables.

*gprbuild also allows to build **multi-language libraries** when specifying
sources from multiple languages.

A non-library project can import a library project. When the builder is invoked
on the former, the library of the latter is only rebuilt when absolutely
necessary. For instance, if a unit of the library is not up-to-date but none of
the executables need this unit, then the unit is not recompiled and the library
is not reassembled.  For instance, let's assume in our example that logging has
the following sources: :file:`log1.ads`, :file:`log1.adb`, :file:`log2.ads` and
:file:`log2.adb`. If :file:`log1.adb` has been modified, then the library
:file:`liblogging` will be rebuilt when compiling all the sources of
`Build` only if :file:`proc.ads`, :file:`pack.ads` or :file:`pack.adb`
include a `"with Log1"`.

To ensure that all the sources in the `Logging` library are
up to date, and that all the sources of `Build` are also up to date,
the following two commands need to be used:

.. code-block:: sh

     gprbuild -Plogging.gpr
     gprbuild -Pbuild.gpr

All :file:`ALI` files will also be copied from the object directory to the
library directory. To build executables, *gprbuild* will use the
library rather than the individual object files.

Library projects can also be useful to describe a library that needs to be used
but, for some reason, cannot be rebuilt. For instance, it is the case when some
of the library sources are not available. Such library projects need to use the
`Externally_Built` attribute as in the example below:

.. code-block:: gpr

     library project Extern_Lib is
        for Languages    use ("Ada", "C");
        for Source_Dirs  use ("lib_src");
        for Library_Dir  use "lib2";
        for Library_Kind use "dynamic";
        for Library_Name use "l2";
        for Externally_Built use "true";  --  <<<<
     end Extern_Lib;

In the case of externally built libraries, the `Object_Dir`
attribute does not need to be specified because it will never be
used.

The main effect of using such an externally built library project is mostly to
affect the linker command in order to reference the desired library. It can
also be achieved by using `Linker.Linker_Options` or `Linker.Switches`
in the project corresponding to the subsystem needing this external library.
This latter method is more straightforward in simple cases but when several
subsystems depend upon the same external library, finding the proper place
for the `Linker.Linker_Options` might not be easy and if it is
not placed properly, the final link command is likely to present ordering issues.
In such a situation, it is better to use the externally built library project
so that all other subsystems depending on it can declare this dependency thanks
to a project |with| clause, which in turn will trigger the builder to find
the proper order of libraries in the final link command.


.. _Stand-alone_Library_Projects:

Stand-alone Library Projects
----------------------------

.. index:: standalone libraries (usage with GNAT Project Manager)

A **stand-alone library** is a library that contains the necessary code to
elaborate the Ada units that are included in the library. A stand-alone
library is a convenient way to add an Ada subsystem to a more global system
whose main is not in Ada since it makes the elaboration of the Ada part mostly
transparent. However, stand-alone libraries are also useful when the main is in
Ada: they provide a means for minimizing relinking & redeployment of complex
systems when localized changes are made.

The name of a stand-alone library, specified with attribute
`Library_Name`, must have the syntax of an Ada identifier.

The most prominent characteristic of a stand-alone library is that it offers a
distinction between interface units and implementation units. Only the former
are visible to units outside the library. A stand-alone library project is thus
characterised by a third attribute, usually **Library_Interface**, in addition
to the two attributes that make a project a Library Project
(`Library_Name` and `Library_Dir`). This third attribute may also be
**Interfaces**. **Library_Interface** only works when the interface is in Ada
and takes a list of units as parameter. **Interfaces** works for any supported
language and takes a list of sources as parameter.

.. index:: Library_Interface (GNAT Project Manager)

**Library_Interface**:

  This attribute defines an explicit subset of the units of the project. Units
  from projects importing this library project may only "with" units whose
  sources are listed in the `Library_Interface`. Other sources are
  considered implementation units.

  .. code-block:: gpr

     for Library_Dir use "lib";
     for Library_Name use "logging";
     for Library_Interface use ("lib1", "lib2");  --  unit names

**Interfaces**

  This attribute defines an explicit subset of the source files of a project.
  Sources from projects importing this project, can only depend on sources from
  this subset. This attribute can be used on non library projects. It can also
  be used as a replacement for attribute `Library_Interface`, in which
  case, units have to be replaced by source files. For multi-language library
  projects, it is the only way to make the project a Stand-Alone Library project
  whose interface is not purely Ada.


.. index:: Library_Standalone (GNAT Project Manager)

**Library_Standalone**:

  This attribute defines the kind of standalone library to
  build. Values are either `standard` (the default), `no` or
  `encapsulated`. When `standard` is used the code to elaborate and
  finalize the library is embedded, when `encapsulated` is used the
  library can furthermore depend only on static libraries (including
  the GNAT runtime). This attribute can be set to `no` to make it clear
  that the library should not be standalone in which case the
  `Library_Interface` should not defined. Note that this attribute
  only applies to shared libraries, so `Library_Kind` must be set
  to `dynamic`.

  .. code-block:: gpr

     for Library_Dir use "lib";
     for Library_Name use "logging";
     for Library_Kind use "dynamic";
     for Library_Interface use ("lib1", "lib2");  --  unit names
     for Library_Standalone use "encapsulated";

In order to include the elaboration code in the stand-alone library, the binder
is invoked on the closure of the library units creating a package whose name
depends on the library name (b~logging.ads/b in the example).
This binder-generated package includes **initialization** and **finalization**
procedures whose names depend on the library name (`logginginit` and
`loggingfinal` in the example). The object corresponding to this package is
included in the library.

.. index:: Library_Auto_Init (GNAT Project Manager)

**Library_Auto_Init**:

  A dynamic stand-alone Library is automatically initialized
  if automatic initialization of Stand-alone Libraries is supported on the
  platform and if attribute **Library_Auto_Init** is not specified or
  is specified with the value "true". A static Stand-alone Library is never
  automatically initialized. Specifying "false" for this attribute
  prevents automatic initialization.

  When a non-automatically initialized stand-alone library is used in an
  executable, its initialization procedure must be called before any service of
  the library is used. When the main subprogram is in Ada, it may mean that the
  initialization procedure has to be called during elaboration of another
  package.


.. index:: Library_Dir (GNAT Project Manager)

**Library_Dir**:

  For a stand-alone library, only the :file:`ALI` files of the interface units
  (those that are listed in attribute `Library_Interface`) are copied to
  the library directory. As a consequence, only the interface units may be
  imported from Ada units outside of the library. If other units are imported,
  the binding phase will fail.


**Binder.Default_Switches**:

  When a stand-alone library is bound, the switches that are specified in
  the attribute **Binder.Default_Switches ("Ada")** are
  used in the call to *gnatbind*.


.. index:: Library_Src_Dir (GNAT Project Manager)

**Library_Src_Dir**:

  This attribute defines the location (absolute or relative to the project
  directory) where the sources of the interface units are copied at
  installation time.
  These sources includes the specs of the interface units along with the
  closure of sources necessary to compile them successfully. That may include
  bodies and subunits, when pragmas `Inline` are used, or when there are
  generic units in specs. This directory cannot point to the object directory
  or one of the source directories, but it can point to the library directory,
  which is the default value for this attribute.


.. index:: Library_Symbol_Policy (GNAT Project Manager)

**Library_Symbol_Policy**:

  This attribute controls the export of symbols and, on some platforms (like
  VMS) that have the notions of major and minor IDs built in the library
  files, it controls the setting of these IDs. It is not supported on all
  platforms (where it will just have no effect). It may have one of the
  following values:

  *  `"autonomous"` or `"default"`: exported symbols are not controlled

  * `"compliant"`: if attribute **Library_Reference_Symbol_File**
    is not defined, then it is equivalent to policy "autonomous". If there
    are exported symbols in the reference symbol file that are not in the
    object files of the interfaces, the major ID of the library is increased.
    If there are symbols in the object files of the interfaces that are not
    in the reference symbol file, these symbols are put at the end of the list
    in the newly created symbol file and the minor ID is increased.

  * `"controlled"`: the attribute **Library_Reference_Symbol_File** must be
    defined. The library will fail to build if the exported symbols in the
    object files of the interfaces do not match exactly the symbol in the
    symbol file.

  * `"restricted"`: The attribute **Library_Symbol_File** must be defined.
    The library will fail to build if there are symbols in the symbol file that
    are not in the exported symbols of the object files of the interfaces.
    Additional symbols in the object files are not added to the symbol file.

  * `"direct"`: The attribute **Library_Symbol_File** must be defined and
    must designate an existing file in the object directory. This symbol file
    is passed directly to the underlying linker without any symbol processing.


.. index:: Library_Reference_Symbol_File (GNAT Project Manager)

**Library_Reference_Symbol_File**

  This attribute may define the path name of a reference symbol file that is
  read when the symbol policy is either "compliant" or "controlled", on
  platforms that support symbol control, such as VMS, when building a
  stand-alone library. The path may be an absolute path or a path relative
  to the project directory.


.. index:: Library_Symbol_File (GNAT Project Manager)

**Library_Symbol_File**

  This attribute may define the name of the symbol file to be created when
  building a stand-alone library when the symbol policy is either "compliant",
  "controlled" or "restricted", on platforms that support symbol control,
  such as VMS. When symbol policy is "direct", then a file with this name
  must exist in the object directory.


.. _Installing_a_library_with_project_files:

Installing a library with project files
---------------------------------------

When using project files, a usable version of the library is created in the
directory specified by the `Library_Dir` attribute of the library
project file. Thus no further action is needed in order to make use of
the libraries that are built as part of the general application build.

You may want to install a library in a context different from where the library
is built. This situation arises with third party suppliers, who may want
to distribute a library in binary form where the user is not expected to be
able to recompile the library. The simplest option in this case is to provide
a project file slightly different from the one used to build the library, by
using the `externally_built` attribute. See :ref:`Using_Library_Projects`

Another option is to use *gprinstall* to install the library in a
different context than the build location. *gprinstall* automatically
generates a project to use this library, and also copies the minimum set of
sources needed to use the library to the install location.
:ref:`Installation`


.. _Project_Extension:

Project Extension
=================

During development of a large system, it is sometimes necessary to use
modified versions of some of the source files, without changing the original
sources. This can be achieved through the **project extension** facility.

Suppose for instance that our example `Build` project is built every night
for the whole team, in some shared directory. A developer usually needs to work
on a small part of the system, and might not want to have a copy of all the
sources and all the object files (mostly because that would require too much
disk space, time to recompile everything). He prefers to be able to override
some of the source files in his directory, while taking advantage of all the
object files generated at night.

Another example can be taken from large software systems, where it is common to have
multiple implementations of a common interface; in Ada terms, multiple
versions of a package body for the same spec.  For example, one implementation
might be safe for use in tasking programs, while another might be used only
in sequential applications.  This can be modeled in GNAT using the concept
of *project extension*.  If one project (the 'child') *extends*
another project (the 'parent') then by default all source files of the
parent project are inherited by the child, but the child project can
override any of the parent's source files with new versions, and can also
add new files or remove unnecessary ones.
This facility is the project analog of a type extension in
object-oriented programming.  Project hierarchies are permitted (an extending
project may itself be extended), and a project that
extends a project can also import other projects.

A third example is that of using project extensions to provide different
versions of the same system. For instance, assume that a `Common`
project is used by two development branches. One of the branches has now
been frozen, and no further change can be done to it or to `Common`.
However, the other development branch still needs evolution of `Common`.
Project extensions provide a flexible solution to create a new version
of a subsystem while sharing and reusing as much as possible from the original
one.

A project extension implicitly inherits all the sources and objects from the
project it extends. It is possible to create a new version of some of the
sources in one of the additional source directories of the extending
project. Those new versions hide the original versions. Adding new sources or
removing existing ones is also possible. Here is an example on how to extend
the project `Build` from previous examples:

.. code-block:: gpr

     project Work extends "../bld/build.gpr" is
     end Work;

The project after **extends** is the one being extended. As usual, it can be
specified using an absolute path, or a path relative to any of the directories
in the project path (see :ref:`Project_Dependencies`). This project does not
specify source or object directories, so the default values for these
attributes will be used that is to say the current directory (where project
`Work` is placed). We can compile that project with

.. code-block:: sh

     gprbuild -Pwork

If no sources have been placed in the current directory, this command
won't do anything, since this project does not change the
sources it inherited from `Build`, therefore all the object files
in `Build` and its dependencies are still valid and are reused
automatically.

Suppose we now want to supply an alternate version of :file:`pack.adb` but use
the existing versions of :file:`pack.ads` and :file:`proc.adb`.  We can create
the new file in Work's current directory (likely by copying the one from the
`Build` project and making changes to it. If new packages are needed at
the same time, we simply create new files in the source directory of the
extending project.

When we recompile, *gprbuild* will now automatically recompile
this file (thus creating :file:`pack.o` in the current directory) and
any file that depends on it (thus creating :file:`proc.o`). Finally, the
executable is also linked locally.

Note that we could have obtained the desired behavior using project import
rather than project inheritance. A `base` project would contain the
sources for :file:`pack.ads` and :file:`proc.adb`, and `Work` would
import `base` and add :file:`pack.adb`. In this scenario,  `base`
cannot contain the original version of :file:`pack.adb` otherwise there would be
2 versions of the same unit in the closure of the project and this is not
allowed. Generally speaking, it is not recommended to put the spec and the
body of a unit in different projects since this affects their autonomy and
reusability.

In a project file that extends another project, it is possible to
indicate that an inherited source is **not part** of the sources of the
extending project. This is necessary sometimes when a package spec has
been overridden and no longer requires a body: in this case, it is
necessary to indicate that the inherited body is not part of the sources
of the project, otherwise there will be a compilation error
when compiling the spec.

.. index:: Excluded_Source_Files (GNAT Project Manager)

.. index:: Excluded_Source_List_File (GNAT Project Manager)

For that purpose, the attribute **Excluded_Source_Files** is used.
Its value is a list of file names.
It is also possible to use attribute `Excluded_Source_List_File`.
Its value is the path of a text file containing one file name per
line.

.. code-block:: gpr

     project Work extends "../bld/build.gpr" is
        for Source_Files use ("pack.ads");
        --  New spec of Pkg does not need a completion
        for Excluded_Source_Files use ("pack.adb");
     end Work;


All packages that are not declared in the extending project are inherited from
the project being extended, with their attributes, with the exception of
`Linker'Linker_Options` which is never inherited. In particular, an
extending project retains all the switches specified in the project being
extended.

At the project level, if they are not declared in the extending project, some
attributes are inherited from the project being extended. They are:
`Languages`, `Main` (for a root non library project) and
`Library_Name` (for a project extending a library project).

.. _Project_Hierarchy_Extension:

Project Hierarchy Extension
---------------------------

One of the fundamental restrictions in project extension is the following:
**A project is not allowed to import directly or indirectly at the same time an extending project and one of its ancestors**.

For example, consider the following hierarchy of projects.

::

     a.gpr  contains package A1
     b.gpr, imports a.gpr and contains B1, which depends on A1
     c.gpr, imports b.gpr and contains C1, which depends on B1

If we want to locally extend the packages `A1` and `C1`, we need to
create several extending projects:

::

     a_ext.gpr which extends a.gpr, and overrides A1
     b_ext.gpr which extends b.gpr and imports a_ext.gpr
     c_ext.gpr which extends c.gpr, imports b_ext.gpr and overrides C1

.. code-block:: gpr

     project A_Ext extends "a.gpr" is
        for Source_Files use ("a1.adb", "a1.ads");
     end A_Ext;

     with "a_ext.gpr";
     project B_Ext extends "b.gpr" is
     end B_Ext;

     with "b_ext.gpr";
     project C_Ext extends "c.gpr" is
        for Source_Files use ("c1.adb");
     end C_Ext;

The extension :file:`b_ext.gpr` is required, even though we are not overriding
any of the sources of :file:`b.gpr` because otherwise :file:`c_expr.gpr` would
import :file:`b.gpr` which itself knows nothing about :file:`a_ext.gpr`.

.. index:: extends all (GNAT Project Manager)

When extending a large system spanning multiple projects, it is often
inconvenient to extend every project in the hierarchy that is impacted by a
small change introduced in a low layer. In such cases, it is possible to create
an **implicit extension** of an entire hierarchy using **extends all**
relationship.

When the project is extended using `extends all` inheritance, all projects
that are imported by it, both directly and indirectly, are considered virtually
extended. That is, the project manager creates implicit projects
that extend every project in the hierarchy; all these implicit projects do not
control sources on their own and use the object directory of
the "extending all" project.

It is possible to explicitly extend one or more projects in the hierarchy
in order to modify the sources. These extending projects must be imported by
the "extending all" project, which will replace the corresponding virtual
projects with the explicit ones.

When building such a project hierarchy extension, the project manager will
ensure that both modified sources and sources in implicit extending projects
that depend on them are recompiled.

Thus, in our example we could create the following projects instead:

::

     a_ext.gpr, extends a.gpr and overrides A1
     c_ext.gpr, "extends all" c.gpr, imports a_ext.gpr and overrides C1

.. code-block:: gpr

     project A_Ext extends "a.gpr" is
        for Source_Files use ("a1.adb", "a1.ads");
     end A_Ext;

     with "a_ext.gpr";
     project C_Ext extends all "c.gpr" is
       for Source_Files use ("c1.adb");
     end C_Ext;


When building project :file:`c_ext.gpr`, the entire modified project space is
considered for recompilation, including the sources of :file:`b.gpr` that are
impacted by the changes in `A1` and `C1`.


.. _Aggregate_Projects:

Aggregate Projects
==================

Aggregate projects are an extension of the project paradigm, and are
meant to solve a few specific use cases that cannot be solved directly
using standard projects. This section will go over a few of these use
cases to try to explain what you can use aggregate projects for.


.. _Building_all_main_programs_from_a_single_project_tree:

Building all main programs from a single project tree
-----------------------------------------------------

Most often, an application is organized into modules and submodules,
which are very conveniently represented as a project tree or graph
(the root project A |withs| the projects for each modules (say B and C),
which in turn |with| projects for submodules.

Very often, modules will build their own executables (for testing
purposes for instance), or libraries (for easier reuse in various
contexts).

However, if you build your project through *gprbuild*, using a syntax similar to

::

     gprbuild -PA.gpr

this will only rebuild the main programs of project A, not those of the
imported projects B and C. Therefore you have to spawn several
*gprbuild* commands, one per project, to build all executables.
This is a little inconvenient, but more importantly is inefficient
because *gprbuild* needs to do duplicate work to ensure that sources are
up-to-date, and cannot easily compile things in parallel when using
the -j switch.

Also libraries are always rebuilt when building a project.

You could therefore define an aggregate project Agg that groups A, B
and C. Then, when you build with

::

      gprbuild -PAgg.gpr

this will build all mains from A, B and C.

.. code-block:: gpr

     aggregate project Agg is
        for Project_Files use ("a.gpr", "b.gpr", "c.gpr");
     end Agg;

If B or C do not define any main program (through their Main
attribute), all their sources are built. When you do not group them
in the aggregate project, only those sources that are needed by A
will be built.

If you add a main to a project P not already explicitly referenced in the
aggregate project, you will need to add "p.gpr" in the list of project
files for the aggregate project, or the main will not be built when
building the aggregate project.

.. _Building_a_set_of_projects_with_a_single_command:

Building a set of projects with a single command
------------------------------------------------

One other case is when you have multiple applications and libraries
that are built independently from each other (but can be built in
parallel). For instance, you have a project tree rooted at A, and
another one (which might share some subprojects) rooted at B.

Using only *gprbuild*, you could do

.. code-block:: sh

    gprbuild -PA.gpr
    gprbuild -PB.gpr

to build both. But again, *gprbuild* has to do some duplicate work for
those files that are shared between the two, and cannot truly build
things in parallel efficiently.

If the two projects are really independent, share no sources other
than through a common subproject, and have no source files with a
common basename, you could create a project C that imports A and
B. But these restrictions are often too strong, and one has to build
them independently. An aggregate project does not have these
limitations and can aggregate two project trees that have common
sources.

This scenario is particularly useful in environments like VxWorks 653
where the applications running in the multiple partitions can be built
in parallel through a single *gprbuild* command. This also works nicely
with Annex E.


.. _Define_a_build_environment:

Define a build environment
--------------------------

The environment variables at the time you launch *gprbuild*
will influence the view these tools have of the project
(PATH to find the compiler, ADA_PROJECT_PATH or GPR_PROJECT_PATH to find the
projects, environment variables that are referenced in project files
through the "external" built-in function, ...). Several command line switches
can be used to override those (-X or -aP), but on some systems and
with some projects, this might make the command line too long, and on
all systems often make it hard to read.

An aggregate project can be used to set the environment for all
projects built through that aggregate. One of the nice aspects is that
you can put the aggregate project under configuration management, and
make sure all your user have a consistent environment when
building. The syntax looks like

.. code-block:: gpr

     aggregate project Agg is
        for Project_Files use ("A.gpr", "B.gpr");
        for Project_Path use ("../dir1", "../dir1/dir2");
        for External ("BUILD") use "PRODUCTION";

        package Builder is
           for Global_Compilation_Switches ("Ada") use ("-g");
        end Builder;
     end Agg;

One of the often requested features in projects is to be able to
reference external variables in |with| declarations, as in

.. code-block:: gpr

     with external("SETUP") & "path/prj.gpr";   --  ILLEGAL
     project MyProject is
        ...
     end MyProject;

For various reasons, this is not allowed. But using aggregate projects provide
an elegant solution. For instance, you could use a project file like:

.. code-block:: gpr

     aggregate project Agg is
         for Project_Path use (external("SETUP") & "path");
         for Project_Files use ("myproject.gpr");
     end Agg;

     with "prj.gpr";  --  searched on Agg'Project_Path
     project MyProject is
        ...
     end MyProject;


.. _Performance_improvements_in_builder:

Performance improvements in builder
-----------------------------------

The loading of aggregate projects is optimized in *gprbuild*,
so that all files are searched for only once on the disk
(thus reducing the number of system calls and contributing to faster
compilation times, especially on systems with sources on remote
servers). As part of the loading, *gprbuild*
computes how and where a source file should be compiled, and even if it is
found several times in the aggregated projects it will be compiled only
once.

Since there is no ambiguity as to which switches should be used, files
can be compiled in parallel (through the usual -j switch) and this can
be done while maximizing the use of CPUs (compared to launching
multiple *gprbuild* commands in parallel).


.. _Syntax_of_aggregate_projects:

Syntax of aggregate projects
----------------------------

An aggregate project follows the general syntax of project files. The
recommended extension is still :file:`.gpr`. However, a special
`aggregate` qualifier must be put before the keyword
`project`.

An aggregate project cannot |with| any other project (standard or
aggregate), except an abstract project which can be used to share attribute
values. Also, aggregate projects cannot be extended or imported though a
|with| clause by any other project. Building other aggregate projects from
an aggregate project is done through the Project_Files attribute (see below).

An aggregate project does not have any source files directly (only
through other standard projects). Therefore a number of the standard
attributes and packages are forbidden in an aggregate project. Here is the
(non exhaustive) list:

* Languages
* Source_Files, Source_List_File and other attributes dealing with
  list of sources.
* Source_Dirs, Exec_Dir and Object_Dir
* Library_Dir, Library_Name and other library-related attributes
* Main
* Roots
* Externally_Built
* Inherit_Source_Path
* Excluded_Source_Dirs
* Locally_Removed_Files
* Excluded_Source_Files
* Excluded_Source_List_File
* Interfaces

The only package that is authorized (albeit optional) is
Builder. Other packages (in particular Compiler, Binder and Linker)
are forbidden.

The following three attributes can be used only in an aggregate project:

.. index:: Project_Files (GNAT Project Manager)

**Project_Files**:

  This attribute is compulsory (or else we are not aggregating any project,
  and thus not doing anything). It specifies a list of :file:`.gpr` files
  that are grouped in the aggregate. The list may be empty. The project
  files can be either other aggregate projects, or standard projects. When
  grouping standard projects, you can have both the root of a project tree
  (and you do not need to specify all its imported projects), and any project
  within the tree.

  Basically, the idea is to specify all those projects that have
  main programs you want to build and link, or libraries you want to
  build. You can even specify projects that do not use the Main
  attribute nor the `Library_*` attributes, and the result will be to
  build all their source files (not just the ones needed by other
  projects).

  The file can include paths (absolute or relative). Paths are relative to
  the location of the aggregate project file itself (if you use a base name,
  we expect to find the .gpr file in the same directory as the aggregate
  project file). The environment variables `ADA_PROJECT_PATH`,
  `GPR_PROJECT_PATH` and `GPR_PROJECT_PATH_FILE` are not used to find
  the project files. The extension :file:`.gpr` is mandatory, since this attribute
  contains file names, not project names.

  Paths can also include the `"*"` and `"**"` globbing patterns. The
  latter indicates that any subdirectory (recursively) will be
  searched for matching files. The latter (`"**"`) can only occur at the
  last position in the directory part (ie `"a/**/*.gpr"` is supported, but
  not `"**/a/*.gpr"`). Starting the pattern with `"**"` is equivalent
  to starting with `"./**"`.

  For now, the pattern `"*"` is only allowed in the filename part, not
  in the directory part. This is mostly for efficiency reasons to limit the
  number of system calls that are needed.

  Here are a few valid examples:

  .. code-block:: gpr

     for Project_Files use ("a.gpr", "subdir/b.gpr");
     --  two specific projects relative to the directory of agg.gpr

     for Project_Files use ("/.gpr");
     --  all projects recursively


.. index:: Project_Path (GNAT Project Manager)

**Project_Path**:

  This attribute can be used to specify a list of directories in
  which to look for project files in |with| declarations.

  When you specify a project in Project_Files (say `x/y/a.gpr`), and
  `a.gpr` imports a project `b.gpr`, only `b.gpr` is searched in
  the project path. `a.gpr` must be exactly at
  `<dir of the aggregate>/x/y/a.gpr`.

  This attribute, however, does not affect the search for the aggregated
  project files specified with `Project_Files`.

  Each aggregate project has its own `Project_Path` (that is if
  `agg1.gpr` includes `agg2.gpr`, they can potentially both have a
  different `Project_Path`).

  This project path is defined as the concatenation, in that order, of:

  * the current directory;

  * followed by the command line -aP switches;

  * then the directories from the GPR_PROJECT_PATH and ADA_PROJECT_PATH environment
    variables;

  * then the directories from the Project_Path attribute;

  * and finally the predefined directories.

  In the example above, agg2.gpr's project path is not influenced by
  the attribute agg1'Project_Path, nor is agg1 influenced by
  agg2'Project_Path.

  This can potentially lead to errors. Consider the following example::

     --
     --  +---------------+                  +----------------+
     --  | Agg1.gpr      |-=--includes--=-->| Agg2.gpr       |
     --  |  'project_path|                  |  'project_path |
     --  |               |                  |                |
     --  +---------------+                  +----------------+
     --        :                                   :
     --        includes                        includes
     --        :                                   :
     --        v                                   v
     --    +-------+                          +---------+
     --    | P.gpr |<---------- withs --------|  Q.gpr  |
     --    +-------+---------\                +---------+
     --        |             |
     --        withs         |
     --        |             |
     --        v             v
     --    +-------+      +---------+
     --    | R.gpr |      | R'.gpr  |
     --    +-------+      +---------+

  When looking for p.gpr, both aggregates find the same physical file on
  the disk. However, it might happen that with their different project
  paths, both aggregate projects would in fact find a different r.gpr.
  Since we have a common project (p.gpr) "with"ing two different r.gpr,
  this will be reported as an error by the builder.

  Directories are relative to the location of the aggregate project file.

  Example:

  .. code-block:: gpr

     for Project_Path use ("/usr/local/gpr", "gpr/");

.. index:: External (GNAT Project Manager)

**External**:

  This attribute can be used to set the value of environment
  variables as retrieved through the `external` function
  in projects. It does not affect the environment variables
  themselves (so for instance you cannot use it to change the value
  of your PATH as seen from the spawned compiler).

  This attribute affects the external values as seen in the rest of
  the aggregate project, and in the aggregated projects.

  The exact value of external a variable comes from one of three
  sources (each level overrides the previous levels):

  * An External attribute in aggregate project, for instance
    `for External ("BUILD_MODE") use "DEBUG"`;

  * Environment variables.
    These override the value given by the attribute, so that
    users can override the value set in the (presumably shared
    with others team members) aggregate project.

  * The -X command line switch to *gprbuild*.
    This always takes precedence.

  This attribute is only taken into account in the main aggregate
  project (i.e. the one specified on the command line to *gprbuild*),
  and ignored in other aggregate projects. It is invalid
  in standard projects.
  The goal is to have a consistent value in all
  projects that are built through the aggregate, which would not
  be the case in the diamond case: A groups the aggregate
  projects B and C, which both (either directly or indirectly)
  build the project P. If B and C could set different values for
  the environment variables, we would have two different views of
  P, which in particular might impact the list of source files in P.


.. _package_Builder_in_aggregate_projects:

package Builder in aggregate projects
-------------------------------------

As mentioned above, only the package Builder can be specified in
an aggregate project. In this package, only the following attributes
are valid:

.. index:: Switches (GNAT Project Manager)

**Switches**:

  This attribute gives the list of switches to use for *gprbuild*.
  Because no mains can be specified for aggregate projects, the only possible
  index for attribute `Switches` is `others`. All other indexes will
  be ignored.

  Example:

  .. code-block:: gpr

     for Switches (others) use ("-v", "-k", "-j8");

  These switches are only read from the main aggregate project (the
  one passed on the command line), and ignored in all other aggregate
  projects or projects.

  It can only contain builder switches, not compiler switches.

.. index:: Global_Compilation_Switches (GNAT Project Manager)

**Global_Compilation_Switches**

  This attribute gives the list of compiler switches for the various
  languages. For instance,

  .. code-block:: gpr

    for Global_Compilation_Switches ("Ada") use ("O1", "-g");
    for Global_Compilation_Switches ("C")   use ("-O2");

  This attribute is only taken into account in the aggregate project
  specified on the command line, not in other aggregate projects.

  In the projects grouped by that aggregate, the attribute
  Builder.Global_Compilation_Switches is also ignored. However, the
  attribute Compiler.Default_Switches will be taken into account (but
  that of the aggregate have higher priority). The attribute
  Compiler.Switches is also taken into account and can be used to
  override the switches for a specific file. As a result, it always
  has priority.

  The rules are meant to avoid ambiguities when compiling. For
  instance, aggregate project Agg groups the projects A and B, that
  both depend on C. Here is an extra for all of these projects:


  .. code-block:: gpr

     aggregate project Agg is
         for Project_Files use ("a.gpr", "b.gpr");
         package Builder is
            for Global_Compilation_Switches ("Ada") use ("-O2");
         end Builder;
     end Agg;

     with "c.gpr";
     project A is
         package Builder is
            for Global_Compilation_Switches ("Ada") use ("-O1");
            --  ignored
         end Builder;

         package Compiler is
            for Default_Switches ("Ada")
                use ("-O1", "-g");
            for Switches ("a_file1.adb")
                use ("-O0");
         end Compiler;
     end A;

     with "c.gpr";
     project B is
         package Compiler is
            for Default_Switches ("Ada") use ("-O0");
         end Compiler;
     end B;

     project C is
         package Compiler is
            for Default_Switches ("Ada")
                use ("-O3",
                     "-gnatn");
            for Switches ("c_file1.adb")
                use ("-O0", "-g");
         end Compiler;
     end C;


  then the following switches are used:

  * all files from project A except a_file1.adb are compiled
    with "-O2 -g", since the aggregate project has priority.

  * the file a_file1.adb is compiled with
    "-O0", since the Compiler.Switches has priority

  * all files from project B are compiled with
    "-O2", since the aggregate project has priority

  * all files from C are compiled with "-O2 -gnatn", except for
    c_file1.adb which is compiled with "-O0 -g"

  Even though C is seen through two paths (through A and through
  B), the switches used by the compiler are unambiguous.


.. index:: Global_Configuration_Pragmas (GNAT Project Manager)

**Global_Configuration_Pragmas**

  This attribute can be used to specify a file containing
  configuration pragmas, to be passed to the Ada compiler.  Since we
  ignore the package Builder in other aggregate projects and projects,
  only those pragmas defined in the main aggregate project will be
  taken into account.

  Projects can locally add to those by using the
  `Compiler.Local_Configuration_Pragmas` attribute if they need.


.. index:: Global_Config_File (GNAT Project Manager)

**Global_Config_File**

  This attribute, indexed with a language name, can be used to specify a config
  when compiling sources of the language. For Ada, these files are configuration
  pragmas files.

For projects that are built through the aggregate, the package Builder
is ignored, except for the Executable attribute which specifies the
name of the executables resulting from the link of the main programs, and
for the Executable_Suffix.


.. _Aggregate_Library_Projects:

Aggregate Library Projects
==========================

Aggregate library projects make it possible to build a single library
using object files built using other standard or library
projects. This gives the flexibility to describe an application as
having multiple modules (a GUI, database access, ...) using different
project files (so possibly built with different compiler options) and
yet create a single library (static or relocatable) out of the
corresponding object files.

.. _Building_aggregate_library_projects:

Building aggregate library projects
-----------------------------------

For example, we can define an aggregate project Agg that groups A, B
and C:

.. code-block:: gpr

     aggregate library project Agg is
        for Project_Files use ("a.gpr", "b.gpr", "c.gpr");
        for Library_Name use ("agg");
        for Library_Dir use ("lagg");
     end Agg;

Then, when you build with:

.. code-block:: sh

      gprbuild agg.gpr

This will build all units from projects A, B and C and will create a
static library named :file:`libagg.a` in the :file:`lagg`
directory. An aggregate library project has the same set of
restriction as a standard library project.

Note that a shared aggregate library project cannot aggregate a
static library project. In platforms where a compiler option is
required to create relocatable object files, a Builder package in the
aggregate library project may be used:

.. code-block:: gpr

     aggregate library project Agg is
        for Project_Files use ("a.gpr", "b.gpr", "c.gpr");
        for Library_Name use ("agg");
        for Library_Dir use ("lagg");
        for Library_Kind use "relocatable";

        package Builder is
           for Global_Compilation_Switches ("Ada") use ("-fPIC");
        end Builder;
     end Agg;

With the above aggregate library Builder package, the `-fPIC`
option will be passed to the compiler when building any source code
from projects :file:`a.gpr`, :file:`b.gpr` and :file:`c.gpr`.


.. _Syntax_of_aggregate_library_projects:

Syntax of aggregate library projects
------------------------------------

An aggregate library project follows the general syntax of project
files. The recommended extension is still :file:`.gpr`. However, a special
`aggregate library` qualifier must be put before the keyword
`project`.

An aggregate library project cannot |with| any other project
(standard or aggregate), except an abstract project which can be used
to share attribute values.

An aggregate library project does not have any source files directly (only
through other standard projects). Therefore a number of the standard
attributes and packages are forbidden in an aggregate library
project. Here is the (non exhaustive) list:

* Languages
* Source_Files, Source_List_File and other attributes dealing with
  list of sources.
* Source_Dirs, Exec_Dir and Object_Dir
* Main
* Roots
* Externally_Built
* Inherit_Source_Path
* Excluded_Source_Dirs
* Locally_Removed_Files
* Excluded_Source_Files
* Excluded_Source_List_File
* Interfaces

The only package that is authorized (albeit optional) is Builder.

The Project_Files attribute (See :ref:`Aggregate_Projects`) is used to
described the aggregated projects whose object files have to be
included into the aggregate library. The environment variables
`ADA_PROJECT_PATH`, `GPR_PROJECT_PATH` and
`GPR_PROJECT_PATH_FILE` are not used to find the project files.


.. _Project_File_Reference:

Project File Reference
======================

This section describes the syntactic structure of project files, the various
constructs that can be used. Finally, it ends with a summary of all available
attributes.


.. _Project_Declaration:

Project Declaration
-------------------

Project files have an Ada-like syntax. The minimal project file is:

.. code-block:: gpr

     project Empty is
     end Empty;

The identifier `Empty` is the name of the project.
This project name must be present after the reserved
word `end` at the end of the project file, followed by a semi-colon.

**Identifiers** (i.e., the user-defined names such as project or variable names)
have the same syntax as Ada identifiers: they must start with a letter,
and be followed by zero or more letters, digits or underscore characters;
it is also illegal to have two underscores next to each other. Identifiers
are always case-insensitive ("Name" is the same as "name").

::

    simple_name ::= identifier
    name        ::= simple_name { . simple_name }

**Strings** are used for values of attributes or as indexes for these
attributes. They are in general case sensitive, except when noted
otherwise (in particular, strings representing file names will be case
insensitive on some systems, so that "file.adb" and "File.adb" both
represent the same file).

**Reserved words** are the same as for standard Ada 95, and cannot
be used for identifiers. In particular, the following words are currently
used in project files, but others could be added later on. In bold are the
extra reserved words in project files:
``all``, ``at``, ``case``, ``end``, ``for``, ``is``, ``limited``,
``null``, ``others``, ``package``, ``renames``, ``type``, ``use``, ``when``,
``with``, **extends**, **external**, **project**.

**Comments** in project files have the same syntax as in Ada, two consecutive
hyphens through the end of the line.

A project may be an **independent project**, entirely defined by a single
project file. Any source file in an independent project depends only
on the predefined library and other source files in the same project.
But a project may also depend on other projects, either by importing them
through **with clauses**, or by **extending** at most one other project. Both
types of dependency can be used in the same project.

A path name denotes a project file. It can be absolute or relative.
An absolute path name includes a sequence of directories, in the syntax of
the host operating system, that identifies uniquely the project file in the
file system. A relative path name identifies the project file, relative
to the directory that contains the current project, or relative to a
directory listed in the environment variables ADA_PROJECT_PATH and
GPR_PROJECT_PATH. Path names are case sensitive if file names in the host
operating system are case sensitive. As a special case, the directory
separator can always be "/" even on Windows systems, so that project files
can be made portable across architectures.
The syntax of the environment variables ADA_PROJECT_PATH and
GPR_PROJECT_PATH is a list of directory names separated by colons on UNIX and
semicolons on Windows.

A given project name can appear only once in a context clause.

It is illegal for a project imported by a context clause to refer, directly
or indirectly, to the project in which this context clause appears (the
dependency graph cannot contain cycles), except when one of the with clauses
in the cycle is a **limited with**.

.. code-block:: gpr

     with "other_project.gpr";
     project My_Project extends "extended.gpr" is
     end My_Project;

These dependencies form a **directed graph**, potentially cyclic when using
**limited with**. The subgraph reflecting the **extends** relations is a tree.

A project's **immediate sources** are the source files directly defined by
that project, either implicitly by residing in the project source directories,
or explicitly through any of the source-related attributes.
More generally, a project's **sources** are the immediate sources of the
project together with the immediate sources (unless overridden) of any project
on which it depends directly or indirectly.

A **project hierarchy** can be created, where projects are children of
other projects. The name of such a child project must be `Parent.Child`,
where `Parent` is the name of the parent project. In particular, this
makes all |with| clauses of the parent project automatically visible
in the child project.

::

      project        ::= context_clause project_declaration

      context_clause ::= {with_clause}
      with_clause    ::= *with* path_name { , path_name } ;
      path_name      ::= string_literal

      project_declaration ::= simple_project_declaration | project_extension
      simple_project_declaration ::=
        project <project_>name is
          {declarative_item}
        end <project_>simple_name;


.. _Qualified_Projects:

Qualified Projects
------------------

Before the reserved `project`, there may be one or two **qualifiers**, that
is identifiers or reserved words, to qualify the project.
The current list of qualifiers is:

**abstract**:
  Qualifies a project with no sources.
  Such a   project must either have no declaration of attributes `Source_Dirs`,
  `Source_Files`, `Languages` or `Source_List_File`, or one of
  `Source_Dirs`, `Source_Files`, or `Languages` must be declared
  as empty. If it extends another project, the project it extends must also be a
  qualified abstract project.

**standard**:
  A standard project is a non library project with sources.
  This is the default (implicit) qualifier.

**aggregate**:
  A project whose sources are aggregated from other project files.

**aggregate library**:
  A library whose sources are aggregated from other project
  or library project files.

**library**:
  A library project must declare both attributes
  Library_Name` and `Library_Dir`.

**configuration**:
  A configuration project cannot be in a project tree.
  It describes compilers and other tools to *gprbuild*.


.. _Declarations:

Declarations
------------

Declarations introduce new entities that denote types, variables, attributes,
and packages. Some declarations can only appear immediately within a project
declaration. Others can appear within a project or within a package.

::

    declarative_item ::= simple_declarative_item
      | typed_string_declaration
      | package_declaration

    simple_declarative_item ::= variable_declaration
      | typed_variable_declaration
      | attribute_declaration
      | case_construction
      | empty_declaration

    empty_declaration ::= *null* ;

An empty declaration is allowed anywhere a declaration is allowed. It has
no effect.


.. _Packages:

Packages
--------

A project file may contain **packages**, that group attributes (typically
all the attributes that are used by one of the GNAT tools).

A package with a given name may only appear once in a project file.
The following packages are currently supported in project files
(See :ref:`Attributes` for the list of attributes that each can contain).

*Binder*
  This package specifies characteristics useful when invoking the binder either
  directly via the *gnat* driver or when using *gprbuild*.
  See :ref:`Main_Subprograms`.

*Builder*
  This package specifies the compilation options used when building an
  executable or a library for a project. Most of the options should be
  set in one of `Compiler`, `Binder` or `Linker` packages,
  but there are some general options that should be defined in this
  package. See :ref:`Main_Subprograms`, and :ref:`Executable_File_Names` in
  particular.

.. only:: PRO or GPL

  *Check*
    This package specifies the options used when calling the checking tool
    *gnatcheck* via the *gnat* driver. Its attribute
    **Default_Switches** has the same semantics as for the package
    `Builder`. The first string should always be `-rules` to specify
    that all the other options belong to the `-rules` section of the
    parameters to *gnatcheck*.

*Clean*
  This package specifies the options used when cleaning a project or a project
  tree using the tools *gnatclean* or *gprclean*.

*Compiler*
  This package specifies the compilation options used by the compiler for
  each languages. See :ref:`Tools_Options_in_Project_Files`.

*Cross_Reference*
  This package specifies the options used when calling the library tool
  *gnatxref* via the *gnat* driver. Its attributes
  **Default_Switches** and **Switches** have the same semantics as for the
  package `Builder`.

.. only:: PRO or GPL

  *Eliminate*
    This package specifies the options used when calling the tool
    *gnatelim* via the *gnat* driver. Its attributes
    **Default_Switches** and **Switches** have the same semantics as for the
    package `Builder`.

*Finder*
  This package specifies the options used when calling the search tool
  *gnatfind* via the *gnat* driver. Its attributes
  **Default_Switches** and **Switches** have the same semantics as for the
  package `Builder`.

*Gnatls*
  This package specifies the options to use when invoking *gnatls*
  via the *gnat* driver.

.. only:: PRO or GPL

  *Gnatstub*
    This package specifies the options used when calling the tool
    *gnatstub* via the *gnat* driver. Its attributes
    **Default_Switches** and **Switches** have the same semantics as for the
    package `Builder`.

*IDE*
  This package specifies the options used when starting an integrated
  development environment, for instance *GPS* or *Gnatbench*.

*Install*
  This package specifies the options used when installing a project
  with *gprinstall*. See :ref:`Installation`.

*Linker*
  This package specifies the options used by the linker.
  See :ref:`Main_Subprograms`.

.. only:: PRO or GPL

  *Metrics*
    This package specifies the options used when calling the tool
    *gnatmetric* via the *gnat* driver. Its attributes
    **Default_Switches** and **Switches** have the same semantics as for the
    package `Builder`.

*Naming*
  This package specifies the naming conventions that apply
  to the source files in a project. In particular, these conventions are
  used to automatically find all source files in the source directories,
  or given a file name to find out its language for proper processing.
  See :ref:`Naming_Schemes`.

 .. only:: PRO or GPL

  *Pretty_Printer*
    This package specifies the options used when calling the formatting tool
    *gnatpp* via the *gnat* driver. Its attributes
    **Default_Switches** and **Switches** have the same semantics as for the
    package `Builder`.

*Remote*
  This package is used by *gprbuild* to describe how distributed
  compilation should be done.

*Stack*
  This package specifies the options used when calling the tool
  *gnatstack* via the *gnat* driver. Its attributes
  **Default_Switches** and **Switches** have the same semantics as for the
  package `Builder`.

*Synchronize*
  This package specifies the options used when calling the tool
  *gnatsync* via the *gnat* driver.

In its simplest form, a package may be empty:

.. code-block:: gpr

     project Simple is
       package Builder is
       end Builder;
     end Simple;

A package may contain **attribute declarations**,
**variable declarations** and **case constructions**, as will be
described below.

When there is ambiguity between a project name and a package name,
the name always designates the project. To avoid possible confusion, it is
always a good idea to avoid naming a project with one of the
names allowed for packages or any name that starts with `gnat`.

A package can also be defined by a **renaming declaration**. The new package
renames a package declared in a different project file, and has the same
attributes as the package it renames. The name of the renamed package
must be the same as the name of the renaming package. The project must
contain a package declaration with this name, and the project
must appear in the context clause of the current project, or be its parent
project. It is not possible to add or override attributes to the renaming
project. If you need to do so, you should use an **extending declaration**
(see below).

Packages that are renamed in other project files often come from project files
that have no sources: they are just used as templates. Any modification in the
template will be reflected automatically in all the project files that rename
a package from the template. This is a very common way to share settings
between projects.

Finally, a package can also be defined by an **extending declaration**. This is
similar to a **renaming declaration**, except that it is possible to add or
override attributes.

::

      package_declaration ::= package_spec | package_renaming | package_extension
      package_spec ::=
        package <package_>simple_name is
          {simple_declarative_item}
        end package_identifier ;
      package_renaming ::==
        package <package_>simple_name renames <project_>simple_name.package_identifier ;
      package_extension ::==
        package <package_>simple_name extends <project_>simple_name.package_identifier is
          {simple_declarative_item}
        end package_identifier ;


.. _Expressions:

Expressions
-----------

An expression is any value that can be assigned to an attribute or a
variable. It is either a literal value, or a construct requiring runtime
computation by the project manager. In a project file, the computed value of
an expression is either a string or a list of strings.

A string value is one of:

* A literal string, for instance `"comm/my_proj.gpr"`
* The name of a variable that evaluates to a string (see :ref:`Variables`)
* The name of an attribute that evaluates to a string (see :ref:`Attributes`)
* An external reference (see :ref:`External_Values`)
* A concatenation of the above, as in `"prefix_" & Var`.

A list of strings is one of the following:

* A parenthesized comma-separated list of zero or more string expressions, for
  instance `(File_Name, "gnat.adc", File_Name & ".orig")` or `()`.
* The name of a variable that evaluates to a list of strings
* The name of an attribute that evaluates to a list of strings
* A concatenation of a list of strings and a string (as defined above), for
  instance `("A", "B") & "C"`
* A concatenation of two lists of strings

The following is the grammar for expressions

::

      string_literal ::= "{string_element}"  --  Same as Ada
      string_expression ::= string_literal
          | *variable_*name
          | external_value
          | attribute_reference
          | ( string_expression { & string_expression } )
      string_list  ::= ( string_expression { , string_expression } )
         | *string_variable*_name
         | *string_*attribute_reference
      term ::= string_expression | string_list
      expression ::= term { & term }     --  Concatenation

Concatenation involves strings and list of strings. As soon as a list of
strings is involved, the result of the concatenation is a list of strings. The
following Ada declarations show the existing operators:

.. code-block:: ada

     function "&" (X : String;      Y : String)      return String;
     function "&" (X : String_List; Y : String)      return String_List;
     function "&" (X : String_List; Y : String_List) return String_List;


Here are some specific examples:

.. code-block:: ada

     List := () & File_Name; --  One string in this list
     List2 := List & (File_Name & ".orig"); -- Two strings
     Big_List := List & Lists2;  --  Three strings
     Illegal := "gnat.adc" & List2;  --  Illegal, must start with list


.. _External_Values:

External Values
---------------

An external value is an expression whose value is obtained from the command
that invoked the processing of the current project file (typically a
*gprbuild* command).

There are two kinds of external values, one that returns a single string, and
one that returns a string list.

The syntax of a single string external value is::

    external_value ::= *external* ( string_literal [, string_literal] )


The first string_literal is the string to be used on the command line or
in the environment to specify the external value. The second string_literal,
if present, is the default to use if there is no specification for this
external value either on the command line or in the environment.

Typically, the external value will either exist in the
environment variables
or be specified on the command line through the
:samp:`-X{vbl}={value}` switch. If both
are specified, then the command line value is used, so that a user can more
easily override the value.

The function `external` always returns a string. It is an error if the
value was not found in the environment and no default was specified in the
call to `external`.

An external reference may be part of a string expression or of a string
list expression, and can therefore appear in a variable declaration or
an attribute declaration.

Most of the time, this construct is used to initialize typed variables, which
are then used in **case** constructions to control the value assigned to
attributes in various scenarios. Thus such variables are often called
**scenario variables**.

The syntax for a string list external value is::

    external_value ::= *external_as_list* ( string_literal , string_literal )


The first string_literal is the string to be used on the command line or
in the environment to specify the external value. The second string_literal is
the separator between each component of the string list.

If the external value does not exist in the environment or on the command line,
the result is an empty list. This is also the case, if the separator is an
empty string or if the external value is only one separator.

Any separator at the beginning or at the end of the external value is
discarded. Then, if there is no separator in the external value, the result is
a string list with only one string. Otherwise, any string between the beginning
and the first separator, between two consecutive separators and between the
last separator and the end are components of the string list.

::

     *external_as_list* ("SWITCHES", ",")

If the external value is "-O2,-g",
the result is ("-O2", "-g").

If the external value is ",-O2,-g,",
the result is also ("-O2", "-g").

if the external value is "-gnatv",
the result is ("-gnatv").

If the external value is ",,", the result is ("").

If the external value is ",", the result is (), the empty string list.


.. _Typed_String_Declaration:

Typed String Declaration
------------------------

A **type declaration** introduces a discrete set of string literals.
If a string variable is declared to have this type, its value
is restricted to the given set of literals. These are the only named
types in project files. A string type may only be declared at the project
level, not inside a package.

::

     typed_string_declaration ::=
       *type* *<typed_string_>*_simple_name *is* ( string_literal {, string_literal} );

The string literals in the list are case sensitive and must all be different.
They may include any graphic characters allowed in Ada, including spaces.
Here is an example of a string type declaration:

.. code-block:: ada

     type OS is ("NT", "nt", "Unix", "GNU/Linux", "other OS");

Variables of a string type are called **typed variables**; all other
variables are called **untyped variables**. Typed variables are
particularly useful in `case` constructions, to support conditional
attribute declarations. (See :ref:`Case_Constructions`).

A string type may be referenced by its name if it has been declared in the same
project file, or by an expanded name whose prefix is the name of the project
in which it is declared.


.. _Variables:

Variables
---------

**Variables** store values (strings or list of strings) and can appear
as part of an expression. The declaration of a variable creates the
variable and assigns the value of the expression to it. The name of the
variable is available immediately after the assignment symbol, if you
need to reuse its old value to compute the new value. Before the completion
of its first declaration, the value of a variable defaults to the empty
string ("").

A **typed** variable can be used as part of a **case** expression to
compute the value, but it can only be declared once in the project file,
so that all case constructions see the same value for the variable. This
provides more consistency and makes the project easier to understand.
The syntax for its declaration is identical to the Ada syntax for an
object declaration. In effect, a typed variable acts as a constant.

An **untyped** variable can be declared and overridden multiple times
within the same project. It is declared implicitly through an Ada
assignment. The first declaration establishes the kind of the variable
(string or list of strings) and successive declarations must respect
the initial kind. Assignments are executed in the order in which they
appear, so the new value replaces the old one and any subsequent reference
to the variable uses the new value.

A variable may be declared at the project file level, or within a package.

::

     typed_variable_declaration ::=
       *<typed_variable_>*simple_name : *<typed_string_>*name := string_expression;

     variable_declaration ::= *<variable_>*simple_name := expression;

Here are some examples of variable declarations:

.. code-block:: gpr

     This_OS : OS := external ("OS"); --  a typed variable declaration
     That_OS := "GNU/Linux";          --  an untyped variable declaration

     Name      := "readme.txt";
     Save_Name := Name & ".saved";

     Empty_List := ();
     List_With_One_Element := ("-gnaty");
     List_With_Two_Elements := List_With_One_Element & "-gnatg";
     Long_List := ("main.ada", "pack1_.ada", "pack1.ada", "pack2_.ada");

A **variable reference** may take several forms:

* The simple variable name, for a variable in the current package (if any)
  or in the current project
* An expanded name, whose prefix is a context name.

A **context** may be one of the following:

* The name of an existing package in the current project
* The name of an imported project of the current project
* The name of an ancestor project (i.e., a project extended by the current
  project, either directly or indirectly)
* An expanded name whose prefix is an imported/parent project name, and
  whose selector is a package name in that project.


.. _Case_Constructions:

Case Constructions
------------------

A **case** construction is used in a project file to effect conditional
behavior. Through this construction, you can set the value of attributes
and variables depending on the value previously assigned to a typed
variable.

All choices in a choice list must be distinct. Unlike Ada, the choice
lists of all alternatives do not need to include all values of the type.
An `others` choice must appear last in the list of alternatives.

The syntax of a `case` construction is based on the Ada case construction
(although the `null` declaration for empty alternatives is optional).

The case expression must be a string variable, either typed or not, whose value
is often given by an external reference (see :ref:`External_Values`).

Each alternative starts with the reserved word `when`, either a list of
literal strings separated by the `"|"` character or the reserved word
`others`, and the `"=>"` token.
When the case expression is a typed string variable, each literal string must
belong to the string type that is the type of the case variable.
After each `=>`, there are zero or more declarations.  The only
declarations allowed in a case construction are other case constructions,
attribute declarations and variable declarations. String type declarations and
package declarations are not allowed. Variable declarations are restricted to
variables that have already been declared before the case construction.

::

     case_construction ::=
       *case* *<variable_>*name *is* {case_item} *end case* ;

     case_item ::=
       *when* discrete_choice_list =>
         {case_declaration
           | attribute_declaration
           | variable_declaration
           | empty_declaration}

     discrete_choice_list ::= string_literal {| string_literal} | *others*

Here is a typical example, with a typed string variable:

.. code-block:: gpr

     project MyProj is
        type OS_Type is ("GNU/Linux", "Unix", "NT", "VMS");
        OS : OS_Type := external ("OS", "GNU/Linux");

        package Compiler is
          case OS is
            when "GNU/Linux" | "Unix" =>
              for Switches ("Ada")
                  use ("-gnath");
            when "NT" =>
              for Switches ("Ada")
                  use ("-gnatP");
            when others =>
              null;
          end case;
        end Compiler;
     end MyProj;


.. _Attributes:

Attributes
----------

A project (and its packages) may have **attributes** that define
the project's properties.  Some attributes have values that are strings;
others have values that are string lists.

::

     attribute_declaration ::=
        simple_attribute_declaration | indexed_attribute_declaration

     simple_attribute_declaration ::= *for* attribute_designator *use* expression ;

     indexed_attribute_declaration ::=
       *for* *<indexed_attribute_>*simple_name ( string_literal) *use* expression ;

     attribute_designator ::=
       *<simple_attribute_>*simple_name
       | *<indexed_attribute_>*simple_name ( string_literal )

There are two categories of attributes: **simple attributes**
and **indexed attributes**.
Each simple attribute has a default value: the empty string (for string
attributes) and the empty list (for string list attributes).
An attribute declaration defines a new value for an attribute, and overrides
the previous value. The syntax of a simple attribute declaration is similar to
that of an attribute definition clause in Ada.

Some attributes are indexed. These attributes are mappings whose
domain is a set of strings. They are declared one association
at a time, by specifying a point in the domain and the corresponding image
of the attribute.
Like untyped variables and simple attributes, indexed attributes
may be declared several times. Each declaration supplies a new value for the
attribute, and replaces the previous setting.

Here are some examples of attribute declarations:

.. code-block:: gpr

     --  simple attributes
     for Object_Dir use "objects";
     for Source_Dirs use ("units", "test/drivers");

     --  indexed attributes
     for Body ("main") use "Main.ada";
     for Switches ("main.ada")
         use ("-v", "-gnatv");
     for Switches ("main.ada") use Builder'Switches ("main.ada") & "-g";

     --  indexed attributes copy (from package Builder in project Default)
     --  The package name must always be specified, even if it is the current
     --  package.
     for Default_Switches use Default.Builder'Default_Switches;

Attributes references may appear anywhere in expressions, and are used
to retrieve the value previously assigned to the attribute. If an attribute
has not been set in a given package or project, its value defaults to the
empty string or the empty list, with some exceptions.

::

    attribute_reference ::=
      attribute_prefix ' *<simple_attribute>_*simple_name [ (string_literal) ]
    attribute_prefix ::= *project*
      | *<project_>*simple_name
      | package_identifier
      | *<project_>*simple_name . package_identifier

Examples are::

     <project>'Object_Dir
     Naming'Dot_Replacement
     Imported_Project'Source_Dirs
     Imported_Project.Naming'Casing
     Builder'Default_Switches ("Ada")

The exceptions to the empty defaults are:

* Object_Dir: default is "."
* Exec_Dir: default is 'Object_Dir, that is the value of attribute
  Object_Dir in the same project, declared or defaulted.
* Source_Dirs: default is (".")

The prefix of an attribute may be:

* `project` for an attribute of the current project
* The name of an existing package of the current project
* The name of an imported project
* The name of a parent project that is extended by the current project
* An expanded name whose prefix is imported/parent project name,
  and whose selector is a package name

In the following sections, all predefined attributes are succinctly described,
first the project level attributes, that is those attributes that are not in a
package, then the attributes in the different packages.

It is possible for different tools to dynamically create new packages with
attributes, or new attributes in predefined packages. These attributes are
not documented here.

The attributes under Configuration headings are usually found only in
configuration project files.

The characteristics of each attribute are indicated as follows:

* **Type of value**

  The value of an attribute may be a single string, indicated by the word
  "single", or a string list, indicated by the word "list".

* **Read-only**

  When the attribute is read-only, that is when it is not allowed to declare
  the attribute, this is indicated by the words "read-only".

* **Optional index**

  If it is allowed in the value of the attribute (both single and list) to have
  an optional index, this is indicated by the words "optional index".

* **Indexed attribute**

  When it is an indexed attribute, this is indicated by the word "indexed".

* **Case-sensitivity of the index**

  For an indexed attribute, if the index is case-insensitive, this is indicated
  by the words "case-insensitive index".

* **File name index**

  For an indexed attribute, when the index is a file name, this is indicated by
  the words "file name index". The index may or may not be case-sensitive,
  depending on the platform.

* **others allowed in index**

  For an indexed attribute, if it is allowed to use **others** as the index,
  this is indicated by the words "others allowed".

  When **others** is used as the index of an indexed attribute, the value of
  the attribute indexed by **others** is used when no other index would apply.


.. _Project_Level_Attributes:

Project Level Attributes
^^^^^^^^^^^^^^^^^^^^^^^^


* **General**

  * **Name**: single, read-only

    The name of the project.

  * **Project_Dir**: single, read-only

    The path name of the project directory.

  * **Main**: list, optional index

    The list of main sources for the executables.

  * **Languages**: list

    The list of languages of the sources of the project.

  * **Roots**: list, indexed, file name index

    The index is the file name of an executable source. Indicates the list of units
    from the main project that need to be bound and linked with their closures
    with the executable. The index is either a file name, a language name or "*".
    The roots for an executable source are those in **Roots** with an index that
    is the executable source file name, if declared. Otherwise, they are those in
    **Roots** with an index that is the language name of the executable source,
    if present. Otherwise, they are those in **Roots ("*")**, if declared. If none
    of these three possibilities are declared, then there are no roots for the
    executable source.

  * **Externally_Built**: single

    Indicates if the project is externally built.
    Only case-insensitive values allowed are "true" and "false", the default.

* **Directories**

  * **Object_Dir**: single

    Indicates the object directory for the project.

  * **Exec_Dir**: single

    Indicates the exec directory for the project, that is the directory where the
    executables are.

  * **Source_Dirs**: list

    The list of source directories of the project.

  * **Inherit_Source_Path**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of language names. Indicates that
    in the source search path of the index language the source directories of
    the languages in the list should be included.

    Example:

    .. code-block:: gpr

       for Inherit_Source_Path ("C++") use ("C");

  * **Exclude_Source_Dirs**: list

    The list of directories that are included in Source_Dirs but are not source
    directories of the project.

  * **Ignore_Source_Sub_Dirs**: list

    Value is a list of simple names for subdirectories that are removed from the
    list of source directories, including theur subdirectories.

* **Source Files**

  * **Source_Files**: list

    Value is a list of source file simple names.

  * **Locally_Removed_Files**: list

    Obsolescent. Equivalent to Excluded_Source_Files.

  * **Excluded_Source_Files**: list

    Value is a list of simple file names that are not sources of the project.
    Allows to remove sources that are inherited or found in the source directories
    and that match the naming scheme.

  * **Source_List_File**: single

    Value is a text file name that contains a list of source file simple names,
    one on each line.

  * **Excluded_Source_List_File**: single

    Value is a text file name that contains a list of file simple names that
    are not sources of the project.

  * **Interfaces**: list

    Value is a list of file names that constitutes the interfaces of the project.

* **Aggregate Projects**

  * **Project_Files**: list

    Value is the list of aggregated projects.

  * **Project_Path**: list

    Value is a list of directories that are added to the project search path when
    looking for the aggregated projects.

  * **External**: single, indexed

    Index is the name of an external reference. Value is the value of the
    external reference to be used when parsing the aggregated projects.

* **Libraries**

  * **Library_Dir**: single

    Value is the name of the library directory. This attribute needs to be
    declared for each library project.

  * **Library_Name**: single

    Value is the name of the library. This attribute needs to be declared or
    inherited for each library project.

  * **Library_Kind**: single

    Specifies the kind of library: static library (archive) or shared library.
    Case-insensitive values must be one of "static" for archives (the default) or
    "dynamic" or "relocatable" for shared libraries.

  * **Library_Version**: single

    Value is the name of the library file.

  * **Library_Interface**: list

    Value is the list of unit names that constitutes the interfaces
    of a Stand-Alone Library project.

  * **Library_Standalone**: single

    Specifies if a Stand-Alone Library (SAL) is encapsulated or not.
    Only authorized case-insensitive values are "standard" for non encapsulated
    SALs, "encapsulated" for encapsulated SALs or "no" for non SAL library project.

  * **Library_Encapsulated_Options**: list

    Value is a list of options that need to be used when linking an encapsulated
    Stand-Alone Library.

  * **Library_Encapsulated_Supported**: single

    Indicates if encapsulated Stand-Alone Libraries are supported. Only
    authorized case-insensitive values are "true" and "false" (the default).

  * **Library_Auto_Init**: single

    Indicates if a Stand-Alone Library is auto-initialized. Only authorized
    case-insentive values are "true" and "false".

  * **Leading_Library_Options**: list

    Value is a list of options that are to be used at the beginning of
    the command line when linking a shared library.

  * **Library_Options**: list

    Value is a list of options that are to be used when linking a shared library.

  * **Library_Rpath_Options**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of options for an invocation of the
    compiler of the language. This invocation is done for a shared library project
    with sources of the language. The output of the invocation is the path name
    of a shared library file. The directory name is to be put in the run path
    option switch when linking the shared library for the project.

  * **Library_Src_Dir**: single

    Value is the name of the directory where copies of the sources of the
    interfaces of a Stand-Alone Library are to be copied.

  * **Library_ALI_Dir**: single

    Value is the name of the directory where the ALI files of the interfaces
    of a Stand-Alone Library are to be copied. When this attribute is not declared,
    the directory is the library directory.

  * **Library_gcc**: single

    Obsolescent attribute. Specify the linker driver used to link a shared library.
    Use instead attribute Linker'Driver.

  * **Library_Symbol_File**: single

    Value is the name of the library symbol file.

  * **Library_Symbol_Policy**: single

    Indicates the symbol policy kind. Only authorized case-insensitive values are
    "autonomous", "default", "compliant", "controlled" or "direct".

  * **Library_Reference_Symbol_File**: single

    Value is the name of the reference symbol file.

* **Configuration - General**

  * **Default_Language**: single

    Value is the case-insensitive name of the language of a project when attribute
    Languages is not specified.

  * **Run_Path_Option**: list

    Value is the list of switches to be used when specifying the run path option
    in an executable.

  * **Run_Path_Origin**: single

    Value is the the string that may replace the path name of the executable
    directory in the run path options.

  * **Separate_Run_Path_Options**: single

    Indicates if there may be several run path options specified when linking an
    executable. Only authorized case-insensitive values are "true" or "false" (the
    default).

  * **Toolchain_Version**: single, indexed, case-insensitive index

    Index is a language name. Specify the version of a toolchain for a language.

  * **Toolchain_Description**: single, indexed, case-insensitive index

    Obsolescent. No longer used.

  * **Object_Generated**: single, indexed, case-insensitive index

    Index is a language name. Indicates if invoking the compiler for a language
    produces an object file. Only authorized case-insensitive values are "false"
    and "true" (the default).

  * **Objects_Linked**: single, indexed, case-insensitive index

    Index is a language name. Indicates if the object files created by the compiler
    for a language need to be linked in the executable. Only authorized
    case-insensitive values are "false" and "true" (the default).

  * **Target**: single

    Value is the name of the target platform. Taken into account only in the main
    project.

    Note that when the target is specified on the command line (usually with
    a switch --target=), the value of attribute reference 'Target is the one
    specified on the command line.

  * **Runtime**: single, indexed, case-insensitive index

    Index is a language name. Indicates the runtime directory that is to be used
    when using the compiler of the language. Taken into account only in the main
    project.

    Note that when the runtime is specified for a language on the command line
    (usually with a switch --RTS), the value of attribute reference 'Runtime
    for this language is the one specified on the command line.

* **Configuration - Libraries**

  * **Library_Builder**: single

    Value is the path name of the application that is to be used to build
    libraries. Usually the path name of "gprlib".

  * **Library_Support**: single

    Indicates the level of support of libraries. Only authorized case-insensitive
    values are "static_only", "full" or "none" (the default).

* **Configuration - Archives**

  * **Archive_Builder**: list

    Value is the name of the application to be used to create a static library
    (archive), followed by the options to be used.

  * **Archive_Builder_Append_Option**: list

    Value is the list of options to be used when invoking the archive builder
    to add project files into an archive.

  * **Archive_Indexer**: list

    Value is the name of the archive indexer, followed by the required options.

  * **Archive_Suffix**: single

    Value is the extension of archives. When not declared, the extension is ".a".

  * **Library_Partial_Linker**: list

    Value is the name of the partial linker executable, followed by the required
    options.

* **Configuration - Shared Libraries**

  * **Shared_Library_Prefix**: single

    Value is the prefix in the name of shared library files. When not declared,
    the prefix is "lib".

  * **Shared_Library_Suffix**: single

    Value is the the extension of the name of shared library files. When not
    declared, the extension is ".so".

  * **Symbolic_Link_Supported**: single

    Indicates if symbolic links are supported on the platform. Only authorized
    case-insensitive values are "true" and "false" (the default).

  * **Library_Major_Minor_Id_Supported**: single

    Indicates if major and minor ids for shared library names are supported on
    the platform. Only authorized case-insensitive values are "true" and "false"
    (the default).

  * **Library_Auto_Init_Supported**: single

    Indicates if auto-initialization of Stand-Alone Libraries is supported. Only
    authorized case-insensitive values are "true" and "false" (the default).

  * **Shared_Library_Minimum_Switches**: list

    Value is the list of required switches when linking a shared library.

  * **Library_Version_Switches**: list

    Value is the list of switches to specify a internal name for a shared library.

  * **Library_Install_Name_Option**: single

    Value is the name of the option that needs to be used, concatenated with the
    path name of the library file, when linking a shared library.

  * **Runtime_Library_Dir**: single, indexed, case-insensitive index

    Index is a language name. Value is the path name of the directory where the
    runtime libraries are located.

  * **Runtime_Source_Dir**: single, indexed, case-insensitive index

    Index is a language name. Value is the path name of the directory where the
    sources of runtime libraries are located.


.. _Package_Binder_Attributes:

Package Binder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  * **Default_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to be used when binding
    code of the language, if there is no applicable attribute Switches.

  * **Switches**: list, optional index, indexed,
    case-insensitive index, others allowed

    Index is either a language name or a source file name. Value is the list of
    switches to be used when binding code. Index is either the source file name
    of the executable to be bound or the language name of the code to be bound.

* **Configuration - Binding**

  * **Driver**: single, indexed, case-insensitive index

    Index is a language name. Value is the name of the application to be used when
    binding code of the language.

  * **Required_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of the required switches to be
    used when binding code of the language.

  * **Prefix**: single, indexed, case-insensitive index

    Index is a language name. Value is a prefix to be used for the binder exchange
    file name for the language. Used to have different binder exchange file names
    when binding different languages.

  * **Objects_Path**: single,indexed, case-insensitive index

    Index is a language name. Value is the name of the environment variable that
    contains the path for the object directories.

  * **Object_Path_File**: single,indexed, case-insensitive index

    Index is a language name. Value is the name of the environment variable. The
    value of the environment variable is the path name of a text file that
    contains the list of object directories.


.. _Package_Builder_Attributes:

Package Builder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

* **Default_Switches**: list, indexed, case-insensitive index

  Index is a language name. Value is the list of builder switches to be used when
  building an executable of the language, if there is no applicable attribute
  Switches.

* **Switches**: list, optional index, indexed, case-insensitive index,
  others allowed

  Index is either a language name or a source file name. Value is the list of
  builder switches to be used when building an executable. Index is either the
  source file name of the executable to be built or its language name.

* **Global_Compilation_Switches**: list, optional index, indexed,
  case-insensitive index

  Index is a language name. Value is the list of compilation switches to be
  used when building an executable. Index is either the source file name of
  the executable to be built or its language name.

* **Executable**: single, indexed, case-insensitive index

  Index is an executable source file name. Value is the simple file name of the
  executable to be built.

* **Executable_Suffix**: single

  Value is the extension of the file names of executable. When not specified,
  the extension is the default extension of executables on the platform.

* **Global_Configuration_Pragmas**: single

  Value is the file name of a configuration pragmas file that is specified to
  the Ada compiler when compiling any Ada source in the project tree.

* **Global_Config_File**: single, indexed, case-insensitive index

  Index is a language name. Value is the file name of a configuration file that
  is specified to the compiler when compiling any source of the language in the
  project tree.


.. only:: PRO and GPL

   .. _Package_Check_Attributes:

   Package Check Attributes
   ^^^^^^^^^^^^^^^^^^^^^^^^

   * **Default_Switches**: list, indexed, case-insensitive index

     Index is a language name. Value is a list of switches to be used when invoking
     `gnatcheck` for a source of the language, if there is no applicable
     attribute Switches.

   * **Switches**: list, optional index, indexed, case-insensitive index,
     others allowed

     Index is a source file name. Value is the list of switches to be used when
     invoking `gnatcheck` for the source.

.. _Package_Clean_Attributes:

Package Clean Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

* **Switches**: list

  Value is a list of switches to be used by the cleaning application.

* **Source_Artifact_Extensions**: list, indexed, case-insensitive index

  Index is a language names. Value is the list of extensions for file names
  derived from object file names that need to be cleaned in the object
  directory of the project.

* **Object_Artifact_Extensions**: list, indexed, case-insensitive index

  Index is a language names. Value is the list of extensions for file names
  derived from source file names that need to be cleaned in the object
  directory of the project.

* **Artifacts_In_Object_Dir**: single

  Value is a list of file names expressed as regular expressions that are to be
  deleted by gprclean in the object directory of the project.

* **Artifacts_In_Exec_Dir**: single

  Value is list of file names expressed as regular expressions that are to be
  deleted by gprclean in the exec directory of the main project.

.. _Package_Compiler_Attributes:

Package Compiler Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  * **Default_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of switches to be used when invoking
    the compiler for the language for a source of the project, if there is no
    applicable attribute Switches.

  * **Switches**: list, optional index, indexed, case-insensitive index,
    others allowed

    Index is a source file name or a language name. Value is the list of switches
    to be used when invoking the compiler for the source or for its language.

  * **Local_Configuration_Pragmas**: single

    Value is the file name of a configuration pragmas file that is specified to
    the Ada compiler when compiling any Ada source in the project.

  * **Local_Config_File**: single, indexed, case-insensitive index

    Index is a language name. Value is the file name of a configuration file that
    is specified to the compiler when compiling any source of the language in the
    project.

* **Configuration - Compiling**

  * **Driver**: single, indexed, case-insensitive index

    Index is a language name. Value is the name of the executable for the compiler
    of the language.

  * **Language_Kind**: single, indexed, case-insensitive index

    Index is a language name. Indicates the kind of the language, either file based
    or unit based. Only authorized case-insensitive values are "unit_based" and
    "file_based" (the default).

  * **Dependency_Kind**: single, indexed, case-insensitive index

    Index is a language name. Indicates how the dependencies are handled for the
    language. Only authorized case-insensitive values are "makefile", "ali_file",
    "ali_closure" or "none" (the default).

  * **Required_Switches**: list, indexed, case-insensitive index

    Equivalent to attribute Leading_Required_Switches.

  * **Leading_Required_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of the minimum switches to be used
    at the beginning of the command line when invoking the compiler for the
    language.

  * **Trailing_Required_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of the minimum switches to be used
    at the end of the command line when invoking the compiler for the language.

  * **PIC_Option**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to be used when
    compiling a source of the language when the project is a shared library
    project.

  * **Path_Syntax**: single, indexed, case-insensitive index

    Index is a language name. Value is the kind of path syntax to be used when
    invoking the compiler for the language. Only authorized case-insensitive
    values are "canonical" and "host" (the default).

  * **Source_File_Switches**: single, indexed, case-insensitive index

    Index is a language name. Value is a list of switches to be used just before
    the path name of the source to compile when invoking the compiler for a source
    of the language.

  * **Object_File_Suffix**: single, indexed, case-insensitive index

    Index is a language name. Value is the extension of the object files created
    by the compiler of the language. When not specified, the extension is the
    default one for the platform.

  * **Object_File_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to be used by the
    compiler of the language to specify the path name of the object file. When not
    specified, the switch used is "-o".

  * **Multi_Unit_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to be used to compile
    a unit in a multi unit source of the language. The index of the unit in the
    source is concatenated with the last switches in the list.

  * **Multi_Unit_Object_Separator**: single, indexed, case-insensitive index

    Index is a language name. Value is the string to be used in the object file
    name before the index of the unit, when compiling a unit in a multi unit source
    of the language.

* **Configuration - Mapping Files**

  * **Mapping_File_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to be used to specify
    a mapping file when invoking the compiler for a source of the language.

  * **Mapping_Spec_Suffix**: single, indexed, case-insensitive index

    Index is a language name. Value is the suffix to be used in a mapping file
    to indicate that the source is a spec.

  * **Mapping_Body_Suffix**: single, indexed, case-insensitive index

    Index is a language name. Value is the suffix to be used in a mapping file
    to indicate that the source is a body.

* **Configuration - Config Files**

  * **Config_File_Switches**: list: single, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language a configuration file.

  * **Config_Body_File_Name**: single, indexed, case-insensitive index

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to a body of the language in a configuration
    file.

  * **Config_Body_File_Name_Index**: single, indexed, case-insensitive index

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to the body a unit in a multi unit source of the
    language in a configuration file.

  * **Config_Body_File_Name_Pattern**: single, indexed,
    case-insensitive index

    Index is a language name. Value is the template to be used to indicate a
    configuration for all bodies of the languages in a configuration file.

  * **Config_Spec_File_Name**: single, indexed, case-insensitive index

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to a spec of the language in a configuration
    file.

  * **Config_Spec_File_Name_Index**: single, indexed, case-insensitive index

    Index is a language name. Value is the template to be used to indicate a
    configuration specific to the spec a unit in a multi unit source of the
    language in a configuration file.

  * **Config_Spec_File_Name_Pattern**: single, indexed,
    case-insensitive index

    Index is a language name. Value is the template to be used to indicate a
    configuration for all specs of the languages in a configuration file.

  * **Config_File_Unique**: single, indexed, case-insensitive index

    Index is a language name. Indicates if there should be only one configuration
    file specified to the compiler of the language. Only authorized
    case-insensitive values are "true" and "false" (the default).

* **Configuration - Dependencies**

  * **Dependency_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to be used to specify
    to the compiler the dependency file when the dependency kind of the language is
    file based, and when Dependency_Driver is not specified for the language.

  * **Dependency_Driver**: list, indexed, case-insensitive index

    Index is a language name. Value is the name of the executable to be used to
    create the dependency file for a source of the language, followed by the
    required switches.

* **Configuration - Search Paths**

  * **Include_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language to indicate a directory to look for sources.

  * **Include_Path**: single, indexed, case-insensitive index

    Index is a language name. Value is the name of an environment variable that
    contains the path of all the directories that the compiler of the language
    may search for sources.

  * **Include_Path_File**: single, indexed, case-insensitive index

    Index is a language name. Value is the name of an environment variable the
    value of which is the path name of a text file that contains the directories
    that the compiler of the language may search for sources.

  * **Object_Path_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is the list of switches to specify to the
    compiler of the language the name of a text file that contains the list of
    object directories. When this attribute is not declared, the text file is
    not created.


.. _Package_Cross_Reference_Attributes:

Package Cross_Reference Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* **Default_Switches**: list, indexed, case-insensitive index

  Index is a language name. Value is a list of switches to be used when invoking
  `gnatxref` for a source of the language, if there is no applicable
  attribute Switches.

* **Switches**: list, optional index, indexed, case-insensitive index,
  others allowed

  Index is a source file name. Value is the list of switches to be used when
  invoking `gnatxref` for the source.


.. only:: PRO or GPL

  .. _Package_Eliminate_Attributes:

  Package Eliminate Attributes
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  * **Default_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of switches to be used when invoking
    `gnatelim` for a source of the language, if there is no applicable
    attribute Switches.

  * **Switches**: list, optional index, indexed, case-insensitive index,
    others allowed

    Index is a source file name. Value is the list of switches to be used when
    invoking `gnatelim` for the source.

.. _Package_Finder_Attributes:

Package Finder Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **Default_Switches**: list, indexed, case-insensitive index

  Index is a language name. Value is a list of switches to be used when invoking
  `gnatfind` for a source of the language, if there is no applicable
  attribute Switches.

* **Switches**: list, optional index, indexed, case-insensitive index,
  others allowed

  Index is a source file name. Value is the list of switches to be used when
  invoking `gnatfind` for the source.


.. _Package_gnatls_Attributes:

Package gnatls Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **Switches**: list

  Value is a list of switches to be used when invoking `gnatls`.


.. only:: PRO or GPL

  Package gnatstub Attributes
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^

  * **Default_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of switches to be used when invoking
    `gnatstub` for a source of the language, if there is no applicable
    attribute Switches.

  * **Switches**: list, optional index, indexed, case-insensitive index,
    others allowed

    Index is a source file name. Value is the list of switches to be used when
    invoking `gnatstub` for the source.


.. _Package_IDE_Attributes:

Package IDE Attributes
^^^^^^^^^^^^^^^^^^^^^^

* **Default_Switches**: list, indexed

  Index is the name of an external tool that the GNAT Programming System (GPS)
  is supporting. Value is a list of switches to use when invoking that tool.

* **Remote_Host**: single

  Value is a string that designates the remote host in a cross-compilation
  environment, to be used for remote compilation and debugging. This attribute
  should not be specified when running on the local machine.

* **Program_Host**: single

  Value is a string that specifies the name of IP address of the embedded target
  in a cross-compilation environment, on which the program should execute.

* **Communication_Protocol**: single

  Value is the name of the protocol to use to communicate with the target
  in a cross-compilation environment, for example `"wtx"` or
  `"vxworks"`.

* **Compiler_Command**: single, indexed, case-insensitive index

  Index is a language Name. Value is a string that denotes the command to be
  used to invoke the compiler. For historical reasons, the value of
  `Compiler_Command ("Ada")` is expected to be a reference to *gnatmake* or
  *cross-gnatmake*.

* **Debugger_Command**: single

  Value is a string that specifies the name of the debugger to be used, such as
  gdb, powerpc-wrs-vxworks-gdb or gdb-4.

* **gnatlist**: single

  Value is a string that specifies the name of the *gnatls* utility
  to be used to retrieve information about the predefined path; for example,
  `"gnatls"`, `"powerpc-wrs-vxworks-gnatls"`.

* **VCS_Kind**: single

  Value is a string used to specify the Version Control System (VCS) to be used
  for this project, for example "Subversion", "ClearCase". If the
  value is set to "Auto", the IDE will try to detect the actual VCS used
  on the list of supported ones.

* **VCS_File_Check**: single

  Value is a string that specifies the command used by the VCS to check
  the validity of a file, either when the user explicitly asks for a check,
  or as a sanity check before doing the check-in.

* **VCS_Log_Check**: single

  Value is a string that specifies the command used by the VCS to check
  the validity of a log file.

* **Documentation_Dir**: single

  Value is the directory used to generate the documentation of source code.


.. _Package_Install_Attributes:

Package Install Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

* **Artifacts**: list, indexed

  An array attribute to declare a set of files not part of the sources
  to be installed. The array discriminant is the directory where the
  file is to be installed. If a relative directory then Prefix (see
  below) is prepended. Note also that if the same file name occurs
  multiple time in the attribute list, the last one will be the one
  installed.

* **Prefix**: single

  Value is the install destination directory.

* **Sources_Subdir**: single

  Value is the sources directory or subdirectory of Prefix.

* **Exec_Subdir**: single

  Value is the executables directory or subdirectory of Prefix.

* **Lib_Subdir**: single

  Value is library directory or subdirectory of Prefix.

* **Project_Subdir**: single

  Value is the project directory or subdirectory of Prefix.

* **Active**: single

  Indicates that the project is to be installed or not. Case-insensitive value
  "false" means that the project is not to be installed, all other values mean
  that the project is to be installed.

* **Mode**: single

  Value is the installation mode, it is either **dev** (default) or **usage**.

* **Install_Name**: single

  Specify the name to use for recording the installation. The default is
  the project name without the extension.


.. _Package_Linker_Attributes:

Package Linker Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **General**

  * **Required_Switches**: list

    Value is a list of switches that are required when invoking the linker to link
    an executable.

  * **Default_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of switches for the linker when
    linking an executable for a main source of the language, when there is no
    applicable Switches.

  * **Leading_Switches**: list, optional index, indexed,
    case-insensitive index, others allowed

    Index is a source file name or a language name. Value is the list of switches
    to be used at the beginning of the command line when invoking the linker to
    build an executable for the source or for its language.

  * **Switches**: list, optional index, indexed, case-insensitive index,
    others allowed

    Index is a source file name or a language name. Value is the list of switches
    to be used when invoking the linker to build an executable for the source or
    for its language.

  * **Trailing_Switches**: list, optional index, indexed,
    case-insensitive index, others allowed

    Index is a source file name or a language name. Value is the list of switches
    to be used at the end of the command line when invoking the linker to
    build an executable for the source or for its language. These switches may
    override the Required_Switches.

  * **Linker_Options**: list

    Value is a list of switches/options that are to be added when linking an
    executable from a project importing the current project directly or indirectly.
    Linker_Options are not used when linking an executable from the current
    project.

  * **Map_File_Option**: single

    Value is the switch to specify the map file name that the linker needs to
    create.

* **Configuration - Linking**

  * **Driver**: single

    Value is the name of the linker executable.

* **Configuration - Response Files**

  * **Max_Command_Line_Length**: single

    Value is the maximum number of character in the command line when invoking
    the linker to link an executable.

  * **Response_File_Format**: single

    Indicates the kind of response file to create when the length of the linking
    command line is too large. Only authorized case-insensitive values are "none",
    "gnu", "object_list", "gcc_gnu", "gcc_option_list" and "gcc_object_list".

  * **Response_File_Switches**: list

    Value is the list of switches to specify a response file to the linker.



.. only PRO or GPL

  .. _Package_Metrics_Attribute:

  Package Metrics Attribute
  ^^^^^^^^^^^^^^^^^^^^^^^^^

  * **Default_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of switches to be used when invoking
    `gnatmetric` for a source of the language, if there is no applicable
    attribute Switches.

  * **Switches**: list, optional index, indexed, case-insensitive index,
    others allowed

    Index is a source file name. Value is the list of switches to be used when
    invoking `gnatmetric` for the source.


.. _Package_Naming_Attributes:

Package Naming Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **Specification_Suffix**: single, indexed, case-insensitive index

  Equivalent to attribute Spec_Suffix.

* **Spec_Suffix**: single, indexed, case-insensitive index

  Index is a language name. Value is the extension of file names for specs of
  the language.

* **Implementation_Suffix**: single, indexed, case-insensitive index

  Equivalent to attribute Body_Suffix.

* **Body_Suffix**: single, indexed, case-insensitive index

  Index is a language name. Value is the extension of file names for bodies of
  the language.

* **Separate_Suffix**: single

  Value is the extension of file names for subunits of Ada.

* **Casing**: single

  Indicates the casing of sources of the Ada language. Only authorized
  case-insensitive values are "lowercase", "uppercase" and "mixedcase".

* **Dot_Replacement**: single

  Value is the string that replace the dot of unit names in the source file names
  of the Ada language.

* **Specification**: single, optional index, indexed,
  case-insensitive index

  Equivalent to attribute Spec.

* **Spec**: single, optional index, indexed, case-insensitive index

  Index is a unit name. Value is the file name of the spec of the unit.

* **Implementation**: single, optional index, indexed,
  case-insensitive index

  Equivalent to attribute Body.

* **Body**: single, optional index, indexed, case-insensitive index

  Index is a unit name. Value is the file name of the body of the unit.

* **Specification_Exceptions**: list, indexed, case-insensitive index

  Index is a language name. Value is a list of specs for the language that do not
  necessarily follow the naming scheme for the language and that may or may not
  be found in the source directories of the project.

* **Implementation_Exceptions**: list, indexed, case-insensitive index

  Index is a language name. Value is a list of bodies for the language that do not
  necessarily follow the naming scheme for the language and that may or may not
  be found in the source directories of the project.


.. only:: PRO or GPL

  .. _Package_Pretty_Printer_Attributes:

  Package Pretty_Printer Attributes
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  * **Default_Switches**: list, indexed, case-insensitive index

    Index is a language name. Value is a list of switches to be used when invoking
    `gnatpp` for a source of the language, if there is no applicable
    attribute Switches.

  * **Switches**: list, optional index, indexed, case-insensitive index,
    others allowed

    Index is a source file name. Value is the list of switches to be used when
    invoking `gnatpp` for the source.


.. _Package_Remote_Attributes:

Package Remote Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

* **Included_Patterns**: list

  If this attribute is defined it sets the patterns to
  synchronized from the master to the slaves. It is exclusive
  with Excluded_Patterns, that is it is an error to define
  both.

* **Included_Artifact_Patterns**: list

  If this attribute is defined it sets the patterns of compilation
  artifacts to synchronized from the slaves to the build master.
  This attribute replace the default hard-coded patterns.

* **Excluded_Patterns**: list

  Set of patterns to ignore when synchronizing sources from the build
  master to the slaves. A set of predefined patterns are supported
  (e.g. \*.o, \*.ali, \*.exe, etc.), this attributes make it possible to
  add some more patterns.

* **Root_Dir**: single

  Value is the root directory used by the slave machines.


.. _Package_Stack_Attributes:

Package Stack Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

* **Switches**: list

  Value is the list of switches to be used when invoking `gnatstack`.


Package Synchronize Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* **Default_Switches**: list, indexed, case-insensitive index

  Index is a language name. Value is a list of switches to be used when invoking
  `gnatsync` for a source of the language, if there is no applicable
  attribute Switches.

* **Switches**: list, optional index, indexed, case-insensitive index,
  others allowed

  Index is a source file name. Value is the list of switches to be used when
  invoking `gnatsync` for the source.
