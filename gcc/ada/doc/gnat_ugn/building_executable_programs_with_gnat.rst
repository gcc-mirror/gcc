.. |with| replace:: *with*
.. |withs| replace:: *with*\ s
.. |withed| replace:: *with*\ ed
.. |withing| replace:: *with*\ ing

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit

.. role:: switch(samp)

.. _Building_Executable_Programs_With_GNAT:

**************************************
Building Executable Programs with GNAT
**************************************

This chapter describes first the gnatmake tool
(:ref:`The_GNAT_Make_Program_gnatmake`),
which automatically determines the set of sources
needed by an Ada compilation unit and executes the necessary
(re)compilations, binding and linking.
It also explains how to use each tool individually: the
compiler (gcc, see :ref:`Compiling_with_gcc`),
binder (gnatbind, see :ref:`Binding_with_gnatbind`),
and linker (gnatlink, see :ref:`Linking_with_gnatlink`)
to build executable programs.
Finally, this chapter provides examples of
how to make use of the general GNU make mechanism
in a GNAT context (see :ref:`Using_the_GNU_make_Utility`).

.. only:: PRO or GPL

   For building large systems with components possibly written
   in different languages (such as Ada, C, C++ and Fortran)
   and organized into subsystems and libraries, the GPRbuild
   tool can be used. This tool, and the Project Manager
   facility that it is based upon, is described in
   *GPRbuild and GPR Companion Tools User's Guide*.


.. _The_GNAT_Make_Program_gnatmake:

Building with ``gnatmake``
==========================

.. index:: gnatmake

A typical development cycle when working on an Ada program consists of
the following steps:

#. Edit some sources to fix bugs;

#. Add enhancements;

#. Compile all sources affected;

#. Rebind and relink; and

#. Test.

.. index:: Dependency rules (compilation)

The third step in particular can be tricky, because not only do the modified
files have to be compiled, but any files depending on these files must also be
recompiled. The dependency rules in Ada can be quite complex, especially
in the presence of overloading, ``use`` clauses, generics and inlined
subprograms.

``gnatmake`` automatically takes care of the third and fourth steps
of this process. It determines which sources need to be compiled,
compiles them, and binds and links the resulting object files.

Unlike some other Ada make programs, the dependencies are always
accurately recomputed from the new sources. The source based approach of
the GNAT compilation model makes this possible. This means that if
changes to the source program cause corresponding changes in
dependencies, they will always be tracked exactly correctly by
``gnatmake``.

Note that for advanced forms of project structure, we recommend creating
a project file as explained in the *GNAT_Project_Manager* chapter in the
*GPRbuild User's Guide*, and using the
``gprbuild`` tool which supports building with project files and works similarly
to ``gnatmake``.

.. _Running_gnatmake:

Running ``gnatmake``
--------------------

The usual form of the ``gnatmake`` command is

.. code-block:: sh

     $ gnatmake [<switches>] <file_name> [<file_names>] [<mode_switches>]

The only required argument is one ``file_name``, which specifies
a compilation unit that is a main program. Several ``file_names`` can be
specified: this will result in several executables being built.
If ``switches`` are present, they can be placed before the first
``file_name``, between ``file_names`` or after the last ``file_name``.
If ``mode_switches`` are present, they must always be placed after
the last ``file_name`` and all ``switches``.

If you are using standard file extensions (:file:`.adb` and
:file:`.ads`), then the
extension may be omitted from the ``file_name`` arguments. However, if
you are using non-standard extensions, then it is required that the
extension be given. A relative or absolute directory path can be
specified in a ``file_name``, in which case, the input source file will
be searched for in the specified directory only. Otherwise, the input
source file will first be searched in the directory where
``gnatmake`` was invoked and if it is not found, it will be search on
the source path of the compiler as described in
:ref:`Search_Paths_and_the_Run-Time_Library_RTL`.

All ``gnatmake`` output (except when you specify :switch:`-M`) is sent to
:file:`stderr`. The output produced by the
:switch:`-M` switch is sent to :file:`stdout`.


.. _Switches_for_gnatmake:

Switches for ``gnatmake``
-------------------------

You may specify any of the following switches to ``gnatmake``:


.. index:: --version  (gnatmake)

:switch:`--version`
  Display Copyright and version, then exit disregarding all other options.


.. index:: --help  (gnatmake)

:switch:`--help`
  If ``--version`` was not used, display usage, then exit disregarding
  all other options.


.. index:: --GCC=compiler_name  (gnatmake)

:switch:`--GCC={compiler_name}`
  Program used for compiling. The default is ``gcc``. You need to use
  quotes around ``compiler_name`` if ``compiler_name`` contains
  spaces or other separator characters.
  As an example ``--GCC="foo -x  -y"``
  will instruct ``gnatmake`` to use ``foo -x -y`` as your
  compiler. A limitation of this syntax is that the name and path name of
  the executable itself must not include any embedded spaces. Note that
  switch :switch:`-c` is always inserted after your command name. Thus in the
  above example the compiler command that will be used by ``gnatmake``
  will be ``foo -c -x -y``. If several ``--GCC=compiler_name`` are
  used, only the last ``compiler_name`` is taken into account. However,
  all the additional switches are also taken into account. Thus,
  ``--GCC="foo -x -y" --GCC="bar -z -t"`` is equivalent to
  ``--GCC="bar -x -y -z -t"``.


.. index:: --GNATBIND=binder_name  (gnatmake)

:switch:`--GNATBIND={binder_name}`
  Program used for binding. The default is ``gnatbind``. You need to
  use quotes around ``binder_name`` if ``binder_name`` contains spaces
  or other separator characters.
  As an example ``--GNATBIND="bar -x  -y"``
  will instruct ``gnatmake`` to use ``bar -x -y`` as your
  binder. Binder switches that are normally appended by ``gnatmake``
  to ``gnatbind`` are now appended to the end of ``bar -x -y``.
  A limitation of this syntax is that the name and path name of the executable
  itself must not include any embedded spaces.

.. index:: --GNATLINK=linker_name  (gnatmake)

:switch:`--GNATLINK={linker_name}`
  Program used for linking. The default is ``gnatlink``. You need to
  use quotes around ``linker_name`` if ``linker_name`` contains spaces
  or other separator characters.
  As an example ``--GNATLINK="lan -x  -y"``
  will instruct ``gnatmake`` to use ``lan -x -y`` as your
  linker. Linker switches that are normally appended by ``gnatmake`` to
  ``gnatlink`` are now appended to the end of ``lan -x -y``.
  A limitation of this syntax is that the name and path name of the executable
  itself must not include any embedded spaces.

:switch:`--create-map-file`
  When linking an executable, create a map file. The name of the map file
  has the same name as the executable with extension ".map".

:switch:`--create-map-file={mapfile}`
  When linking an executable, create a map file with the specified name.

.. index:: --create-missing-dirs  (gnatmake)

:switch:`--create-missing-dirs`
  When using project files (:switch:`-P{project}`), automatically create
  missing object directories, library directories and exec
  directories.

:switch:`--single-compile-per-obj-dir`
  Disallow simultaneous compilations in the same object directory when
  project files are used.

:switch:`--subdirs={subdir}`
  Actual object directory of each project file is the subdirectory subdir of the
  object directory specified or defaulted in the project file.

:switch:`--unchecked-shared-lib-imports`
  By default, shared library projects are not allowed to import static library
  projects. When this switch is used on the command line, this restriction is
  relaxed.

:switch:`--source-info={source info file}`
  Specify a source info file. This switch is active only when project files
  are used. If the source info file is specified as a relative path, then it is
  relative to the object directory of the main project. If the source info file
  does not exist, then after the Project Manager has successfully parsed and
  processed the project files and found the sources, it creates the source info
  file. If the source info file already exists and can be read successfully,
  then the Project Manager will get all the needed information about the sources
  from the source info file and will not look for them. This reduces the time
  to process the project files, especially when looking for sources that take a
  long time. If the source info file exists but cannot be parsed successfully,
  the Project Manager will attempt to recreate it. If the Project Manager fails
  to create the source info file, a message is issued, but gnatmake does not
  fail. ``gnatmake`` "trusts" the source info file. This means that
  if the source files have changed (addition, deletion, moving to a different
  source directory), then the source info file need to be deleted and recreated.


.. index:: -a  (gnatmake)

:switch:`-a`
  Consider all files in the make process, even the GNAT internal system
  files (for example, the predefined Ada library files), as well as any
  locked files. Locked files are files whose ALI file is write-protected.
  By default,
  ``gnatmake`` does not check these files,
  because the assumption is that the GNAT internal files are properly up
  to date, and also that any write protected ALI files have been properly
  installed. Note that if there is an installation problem, such that one
  of these files is not up to date, it will be properly caught by the
  binder.
  You may have to specify this switch if you are working on GNAT
  itself. The switch ``-a`` is also useful
  in conjunction with ``-f``
  if you need to recompile an entire application,
  including run-time files, using special configuration pragmas,
  such as a ``Normalize_Scalars`` pragma.

  By default
  ``gnatmake -a`` compiles all GNAT
  internal files with
  ``gcc -c -gnatpg`` rather than ``gcc -c``.


.. index:: -b  (gnatmake)

:switch:`-b`
  Bind only. Can be combined with :switch:`-c` to do
  compilation and binding, but no link.
  Can be combined with :switch:`-l`
  to do binding and linking. When not combined with
  :switch:`-c`
  all the units in the closure of the main program must have been previously
  compiled and must be up to date. The root unit specified by ``file_name``
  may be given without extension, with the source extension or, if no GNAT
  Project File is specified, with the ALI file extension.


.. index:: -c  (gnatmake)

:switch:`-c`
  Compile only. Do not perform binding, except when :switch:`-b`
  is also specified. Do not perform linking, except if both
  :switch:`-b` and
  :switch:`-l` are also specified.
  If the root unit specified by ``file_name`` is not a main unit, this is the
  default. Otherwise ``gnatmake`` will attempt binding and linking
  unless all objects are up to date and the executable is more recent than
  the objects.


.. index:: -C  (gnatmake)

:switch:`-C`
  Use a temporary mapping file. A mapping file is a way to communicate
  to the compiler two mappings: from unit names to file names (without
  any directory information) and from file names to path names (with
  full directory information). A mapping file can make the compiler's
  file searches faster, especially if there are many source directories,
  or the sources are read over a slow network connection. If
  :switch:`-P` is used, a mapping file is always used, so
  :switch:`-C` is unnecessary; in this case the mapping file
  is initially populated based on the project file. If
  :switch:`-C` is used without
  :switch:`-P`,
  the mapping file is initially empty. Each invocation of the compiler
  will add any newly accessed sources to the mapping file.


.. index:: -C=  (gnatmake)

:switch:`-C={file}`
  Use a specific mapping file. The file, specified as a path name (absolute or
  relative) by this switch, should already exist, otherwise the switch is
  ineffective. The specified mapping file will be communicated to the compiler.
  This switch is not compatible with a project file
  (-P`file`) or with multiple compiling processes
  (-jnnn, when nnn is greater than 1).


.. index:: -d  (gnatmake)

:switch:`-d`
  Display progress for each source, up to date or not, as a single line:

  ::

      completed x out of y (zz%)

  If the file needs to be compiled this is displayed after the invocation of
  the compiler. These lines are displayed even in quiet output mode.


.. index:: -D  (gnatmake)

:switch:`-D {dir}`
  Put all object files and ALI file in directory ``dir``.
  If the :switch:`-D` switch is not used, all object files
  and ALI files go in the current working directory.

  This switch cannot be used when using a project file.


.. index:: -eI  (gnatmake)

:switch:`-eI{nnn}`
  Indicates that the main source is a multi-unit source and the rank of the unit
  in the source file is nnn. nnn needs to be a positive number and a valid
  index in the source. This switch cannot be used when ``gnatmake`` is
  invoked for several mains.


.. index:: -eL  (gnatmake)
.. index:: symbolic links

:switch:`-eL`
  Follow all symbolic links when processing project files.
  This should be used if your project uses symbolic links for files or
  directories, but is not needed in other cases.

  .. index:: naming scheme

  This also assumes that no directory matches the naming scheme for files (for
  instance that you do not have a directory called "sources.ads" when using the
  default GNAT naming scheme).

  When you do not have to use this switch (i.e., by default), gnatmake is able to
  save a lot of system calls (several per source file and object file), which
  can result in a significant speed up to load and manipulate a project file,
  especially when using source files from a remote system.


.. index:: -eS  (gnatmake)

:switch:`-eS`
  Output the commands for the compiler, the binder and the linker
  on standard output,
  instead of standard error.


.. index:: -f  (gnatmake)

:switch:`-f`
  Force recompilations. Recompile all sources, even though some object
  files may be up to date, but don't recompile predefined or GNAT internal
  files or locked files (files with a write-protected ALI file),
  unless the :switch:`-a` switch is also specified.


.. index:: -F  (gnatmake)

:switch:`-F`
  When using project files, if some errors or warnings are detected during
  parsing and verbose mode is not in effect (no use of switch
  -v), then error lines start with the full path name of the project
  file, rather than its simple file name.


.. index:: -g  (gnatmake)

:switch:`-g`
  Enable debugging. This switch is simply passed to the compiler and to the
  linker.


.. index:: -i  (gnatmake)

:switch:`-i`
  In normal mode, ``gnatmake`` compiles all object files and ALI files
  into the current directory. If the :switch:`-i` switch is used,
  then instead object files and ALI files that already exist are overwritten
  in place. This means that once a large project is organized into separate
  directories in the desired manner, then ``gnatmake`` will automatically
  maintain and update this organization. If no ALI files are found on the
  Ada object path (see :ref:`Search_Paths_and_the_Run-Time_Library_RTL`),
  the new object and ALI files are created in the
  directory containing the source being compiled. If another organization
  is desired, where objects and sources are kept in different directories,
  a useful technique is to create dummy ALI files in the desired directories.
  When detecting such a dummy file, ``gnatmake`` will be forced to
  recompile the corresponding source file, and it will be put the resulting
  object and ALI files in the directory where it found the dummy file.


.. index:: -j  (gnatmake)
.. index:: Parallel make

:switch:`-j{n}`
  Use ``n`` processes to carry out the (re)compilations. On a multiprocessor
  machine compilations will occur in parallel. If ``n`` is 0, then the
  maximum number of parallel compilations is the number of core processors
  on the platform. In the event of compilation errors, messages from various
  compilations might get interspersed (but ``gnatmake`` will give you the
  full ordered list of failing compiles at the end). If this is problematic,
  rerun the make process with n set to 1 to get a clean list of messages.


.. index:: -k  (gnatmake)

:switch:`-k`
  Keep going. Continue as much as possible after a compilation error. To
  ease the programmer's task in case of compilation errors, the list of
  sources for which the compile fails is given when ``gnatmake``
  terminates.

  If ``gnatmake`` is invoked with several :file:`file_names` and with this
  switch, if there are compilation errors when building an executable,
  ``gnatmake`` will not attempt to build the following executables.


.. index:: -l  (gnatmake)

:switch:`-l`
  Link only. Can be combined with :switch:`-b` to binding
  and linking. Linking will not be performed if combined with
  :switch:`-c`
  but not with :switch:`-b`.
  When not combined with :switch:`-b`
  all the units in the closure of the main program must have been previously
  compiled and must be up to date, and the main program needs to have been bound.
  The root unit specified by ``file_name``
  may be given without extension, with the source extension or, if no GNAT
  Project File is specified, with the ALI file extension.


.. index:: -m  (gnatmake)

:switch:`-m`
  Specify that the minimum necessary amount of recompilations
  be performed. In this mode ``gnatmake`` ignores time
  stamp differences when the only
  modifications to a source file consist in adding/removing comments,
  empty lines, spaces or tabs. This means that if you have changed the
  comments in a source file or have simply reformatted it, using this
  switch will tell ``gnatmake`` not to recompile files that depend on it
  (provided other sources on which these files depend have undergone no
  semantic modifications). Note that the debugging information may be
  out of date with respect to the sources if the :switch:`-m` switch causes
  a compilation to be switched, so the use of this switch represents a
  trade-off between compilation time and accurate debugging information.


.. index:: Dependencies, producing list
.. index:: -M  (gnatmake)

:switch:`-M`
  Check if all objects are up to date. If they are, output the object
  dependences to :file:`stdout` in a form that can be directly exploited in
  a :file:`Makefile`. By default, each source file is prefixed with its
  (relative or absolute) directory name. This name is whatever you
  specified in the various :switch:`-aI`
  and :switch:`-I` switches. If you use
  ``gnatmake -M``  :switch:`-q`
  (see below), only the source file names,
  without relative paths, are output. If you just specify the  :switch:`-M`
  switch, dependencies of the GNAT internal system files are omitted. This
  is typically what you want. If you also specify
  the :switch:`-a` switch,
  dependencies of the GNAT internal files are also listed. Note that
  dependencies of the objects in external Ada libraries (see
  switch  :switch:`-aL{dir}` in the following list)
  are never reported.


.. index:: -n  (gnatmake)

:switch:`-n`
  Don't compile, bind, or link. Checks if all objects are up to date.
  If they are not, the full name of the first file that needs to be
  recompiled is printed.
  Repeated use of this option, followed by compiling the indicated source
  file, will eventually result in recompiling all required units.


.. index:: -o  (gnatmake)

:switch:`-o {exec_name}`
  Output executable name. The name of the final executable program will be
  ``exec_name``. If the :switch:`-o` switch is omitted the default
  name for the executable will be the name of the input file in appropriate form
  for an executable file on the host system.

  This switch cannot be used when invoking ``gnatmake`` with several
  :file:`file_names`.


.. index:: -p  (gnatmake)

:switch:`-p`
  Same as :switch:`--create-missing-dirs`

.. index:: -P  (gnatmake)

:switch:`-P{project}`
  Use project file ``project``. Only one such switch can be used.

.. -- Comment:
  :ref:`gnatmake_and_Project_Files`.


.. index:: -q  (gnatmake)

:switch:`-q`
  Quiet. When this flag is not set, the commands carried out by
  ``gnatmake`` are displayed.


.. index:: -s  (gnatmake)

:switch:`-s`
  Recompile if compiler switches have changed since last compilation.
  All compiler switches but -I and -o are taken into account in the
  following way:
  orders between different 'first letter' switches are ignored, but
  orders between same switches are taken into account. For example,
  :switch:`-O -O2` is different than :switch:`-O2 -O`, but :switch:`-g -O`
  is equivalent to :switch:`-O -g`.

  This switch is recommended when Integrated Preprocessing is used.


.. index:: -u  (gnatmake)

:switch:`-u`
  Unique. Recompile at most the main files. It implies -c. Combined with
  -f, it is equivalent to calling the compiler directly. Note that using
  -u with a project file and no main has a special meaning.

.. --Comment
  (See :ref:`Project_Files_and_Main_Subprograms`.)


.. index:: -U  (gnatmake)

:switch:`-U`
  When used without a project file or with one or several mains on the command
  line, is equivalent to -u. When used with a project file and no main
  on the command line, all sources of all project files are checked and compiled
  if not up to date, and libraries are rebuilt, if necessary.


.. index:: -v  (gnatmake)

:switch:`-v`
  Verbose. Display the reason for all recompilations ``gnatmake``
  decides are necessary, with the highest verbosity level.


.. index:: -vl  (gnatmake)

:switch:`-vl`
  Verbosity level Low. Display fewer lines than in verbosity Medium.


.. index:: -vm  (gnatmake)

:switch:`-vm`
  Verbosity level Medium. Potentially display fewer lines than in verbosity High.


.. index:: -vm  (gnatmake)

:switch:`-vh`
  Verbosity level High. Equivalent to -v.


:switch:`-vP{x}`
  Indicate the verbosity of the parsing of GNAT project files.
  See :ref:`Switches_Related_to_Project_Files`.


.. index:: -x  (gnatmake)

:switch:`-x`
  Indicate that sources that are not part of any Project File may be compiled.
  Normally, when using Project Files, only sources that are part of a Project
  File may be compile. When this switch is used, a source outside of all Project
  Files may be compiled. The ALI file and the object file will be put in the
  object directory of the main Project. The compilation switches used will only
  be those specified on the command line. Even when
  :switch:`-x` is used, mains specified on the
  command line need to be sources of a project file.


:switch:`-X{name}={value}`
  Indicate that external variable ``name`` has the value ``value``.
  The Project Manager will use this value for occurrences of
  ``external(name)`` when parsing the project file.
  :ref:`Switches_Related_to_Project_Files`.


.. index:: -z  (gnatmake)

:switch:`-z`
  No main subprogram. Bind and link the program even if the unit name
  given on the command line is a package name. The resulting executable
  will execute the elaboration routines of the package and its closure,
  then the finalization routines.


.. rubric:: GCC switches

Any uppercase or multi-character switch that is not a ``gnatmake`` switch
is passed to ``gcc`` (e.g., :switch:`-O`, :switch:`-gnato,` etc.)


.. rubric:: Source and library search path switches

.. index:: -aI  (gnatmake)

:switch:`-aI{dir}`
  When looking for source files also look in directory ``dir``.
  The order in which source files search is undertaken is
  described in :ref:`Search_Paths_and_the_Run-Time_Library_RTL`.


.. index:: -aL  (gnatmake)

:switch:`-aL{dir}`
  Consider ``dir`` as being an externally provided Ada library.
  Instructs ``gnatmake`` to skip compilation units whose :file:`.ALI`
  files have been located in directory ``dir``. This allows you to have
  missing bodies for the units in ``dir`` and to ignore out of date bodies
  for the same units. You still need to specify
  the location of the specs for these units by using the switches
  :switch:`-aI{dir}`  or :switch:`-I{dir}`.
  Note: this switch is provided for compatibility with previous versions
  of ``gnatmake``. The easier method of causing standard libraries
  to be excluded from consideration is to write-protect the corresponding
  ALI files.


.. index:: -aO  (gnatmake)

:switch:`-aO{dir}`
  When searching for library and object files, look in directory
  ``dir``. The order in which library files are searched is described in
  :ref:`Search_Paths_for_gnatbind`.


.. index:: Search paths, for gnatmake
.. index:: -A  (gnatmake)

:switch:`-A{dir}`
  Equivalent to :switch:`-aL{dir}` :switch:`-aI{dir}`.


  .. index:: -I  (gnatmake)

:switch:`-I{dir}`
  Equivalent to :switch:`-aO{dir} -aI{dir}`.


.. index:: -I-  (gnatmake)
.. index:: Source files, suppressing search

:switch:`-I-`
  Do not look for source files in the directory containing the source
  file named in the command line.
  Do not look for ALI or object files in the directory
  where ``gnatmake`` was invoked.


.. index:: -L  (gnatmake)
.. index:: Linker libraries

:switch:`-L{dir}`
  Add directory ``dir`` to the list of directories in which the linker
  will search for libraries. This is equivalent to
  :switch:`-largs` :switch:`-L{dir}`.
  Furthermore, under Windows, the sources pointed to by the libraries path
  set in the registry are not searched for.


.. index:: -nostdinc  (gnatmake)

:switch:`-nostdinc`
  Do not look for source files in the system default directory.


.. index:: -nostdlib  (gnatmake)

:switch:`-nostdlib`
  Do not look for library files in the system default directory.


.. index:: --RTS  (gnatmake)

:switch:`--RTS={rts-path}`
  Specifies the default location of the run-time library. GNAT looks for the
  run-time
  in the following directories, and stops as soon as a valid run-time is found
  (:file:`adainclude` or :file:`ada_source_path`, and :file:`adalib` or
  :file:`ada_object_path` present):

  * *<current directory>/$rts_path*

  * *<default-search-dir>/$rts_path*

  * *<default-search-dir>/rts-$rts_path*

  * The selected path is handled like a normal RTS path.


.. _Mode_Switches_for_gnatmake:

Mode Switches for ``gnatmake``
------------------------------

The mode switches (referred to as ``mode_switches``) allow the
inclusion of switches that are to be passed to the compiler itself, the
binder or the linker. The effect of a mode switch is to cause all
subsequent switches up to the end of the switch list, or up to the next
mode switch, to be interpreted as switches to be passed on to the
designated component of GNAT.

.. index:: -cargs  (gnatmake)

:switch:`-cargs {switches}`
  Compiler switches. Here ``switches`` is a list of switches
  that are valid switches for ``gcc``. They will be passed on to
  all compile steps performed by ``gnatmake``.


.. index:: -bargs  (gnatmake)

:switch:`-bargs {switches}`
  Binder switches. Here ``switches`` is a list of switches
  that are valid switches for ``gnatbind``. They will be passed on to
  all bind steps performed by ``gnatmake``.


.. index:: -largs  (gnatmake)

:switch:`-largs {switches}`
  Linker switches. Here ``switches`` is a list of switches
  that are valid switches for ``gnatlink``. They will be passed on to
  all link steps performed by ``gnatmake``.


.. index:: -margs  (gnatmake)

:switch:`-margs {switches}`
  Make switches. The switches are directly interpreted by ``gnatmake``,
  regardless of any previous occurrence of :switch:`-cargs`, :switch:`-bargs`
  or :switch:`-largs`.


.. _Notes_on_the_Command_Line:

Notes on the Command Line
-------------------------

This section contains some additional useful notes on the operation
of the ``gnatmake`` command.

.. index:: Recompilation (by gnatmake)

* If ``gnatmake`` finds no ALI files, it recompiles the main program
  and all other units required by the main program.
  This means that ``gnatmake``
  can be used for the initial compile, as well as during subsequent steps of
  the development cycle.

* If you enter ``gnatmake foo.adb``, where ``foo``
  is a subunit or body of a generic unit, ``gnatmake`` recompiles
  :file:`foo.adb` (because it finds no ALI) and stops, issuing a
  warning.

* In ``gnatmake`` the switch :switch:`-I`
  is used to specify both source and
  library file paths. Use :switch:`-aI`
  instead if you just want to specify
  source paths only and :switch:`-aO`
  if you want to specify library paths
  only.

* ``gnatmake`` will ignore any files whose ALI file is write-protected.
  This may conveniently be used to exclude standard libraries from
  consideration and in particular it means that the use of the
  :switch:`-f` switch will not recompile these files
  unless :switch:`-a` is also specified.

* ``gnatmake`` has been designed to make the use of Ada libraries
  particularly convenient. Assume you have an Ada library organized
  as follows: *obj-dir* contains the objects and ALI files for
  of your Ada compilation units,
  whereas *include-dir* contains the
  specs of these units, but no bodies. Then to compile a unit
  stored in ``main.adb``, which uses this Ada library you would just type:

  .. code-block:: sh

      $ gnatmake -aI`include-dir`  -aL`obj-dir`  main

* Using ``gnatmake`` along with the :switch:`-m (minimal recompilation)`
  switch provides a mechanism for avoiding unnecessary recompilations. Using
  this switch,
  you can update the comments/format of your
  source files without having to recompile everything. Note, however, that
  adding or deleting lines in a source files may render its debugging
  info obsolete. If the file in question is a spec, the impact is rather
  limited, as that debugging info will only be useful during the
  elaboration phase of your program. For bodies the impact can be more
  significant. In all events, your debugger will warn you if a source file
  is more recent than the corresponding object, and alert you to the fact
  that the debugging information may be out of date.


.. _How_gnatmake_Works:

How ``gnatmake`` Works
----------------------

Generally ``gnatmake`` automatically performs all necessary
recompilations and you don't need to worry about how it works. However,
it may be useful to have some basic understanding of the ``gnatmake``
approach and in particular to understand how it uses the results of
previous compilations without incorrectly depending on them.

First a definition: an object file is considered *up to date* if the
corresponding ALI file exists and if all the source files listed in the
dependency section of this ALI file have time stamps matching those in
the ALI file. This means that neither the source file itself nor any
files that it depends on have been modified, and hence there is no need
to recompile this file.

``gnatmake`` works by first checking if the specified main unit is up
to date. If so, no compilations are required for the main unit. If not,
``gnatmake`` compiles the main program to build a new ALI file that
reflects the latest sources. Then the ALI file of the main unit is
examined to find all the source files on which the main program depends,
and ``gnatmake`` recursively applies the above procedure on all these
files.

This process ensures that ``gnatmake`` only trusts the dependencies
in an existing ALI file if they are known to be correct. Otherwise it
always recompiles to determine a new, guaranteed accurate set of
dependencies. As a result the program is compiled 'upside down' from what may
be more familiar as the required order of compilation in some other Ada
systems. In particular, clients are compiled before the units on which
they depend. The ability of GNAT to compile in any order is critical in
allowing an order of compilation to be chosen that guarantees that
``gnatmake`` will recompute a correct set of new dependencies if
necessary.

When invoking ``gnatmake`` with several ``file_names``, if a unit is
imported by several of the executables, it will be recompiled at most once.

Note: when using non-standard naming conventions
(:ref:`Using_Other_File_Names`), changing through a configuration pragmas
file the version of a source and invoking ``gnatmake`` to recompile may
have no effect, if the previous version of the source is still accessible
by ``gnatmake``. It may be necessary to use the switch
-f.


.. _Examples_of_gnatmake_Usage:

Examples of ``gnatmake`` Usage
------------------------------

*gnatmake hello.adb*
  Compile all files necessary to bind and link the main program
  :file:`hello.adb` (containing unit ``Hello``) and bind and link the
  resulting object files to generate an executable file :file:`hello`.

*gnatmake main1 main2 main3*
  Compile all files necessary to bind and link the main programs
  :file:`main1.adb` (containing unit ``Main1``), :file:`main2.adb`
  (containing unit ``Main2``) and :file:`main3.adb`
  (containing unit ``Main3``) and bind and link the resulting object files
  to generate three executable files :file:`main1`,
  :file:`main2`  and :file:`main3`.

*gnatmake -q Main_Unit -cargs -O2 -bargs -l*
  Compile all files necessary to bind and link the main program unit
  ``Main_Unit`` (from file :file:`main_unit.adb`). All compilations will
  be done with optimization level 2 and the order of elaboration will be
  listed by the binder. ``gnatmake`` will operate in quiet mode, not
  displaying commands it is executing.


.. _Compiling_with_gcc:

Compiling with ``gcc``
======================

This section discusses how to compile Ada programs using the ``gcc``
command. It also describes the set of switches
that can be used to control the behavior of the compiler.

.. _Compiling_Programs:

Compiling Programs
------------------

The first step in creating an executable program is to compile the units
of the program using the ``gcc`` command. You must compile the
following files:

* the body file (:file:`.adb`) for a library level subprogram or generic
  subprogram

* the spec file (:file:`.ads`) for a library level package or generic
  package that has no body

* the body file (:file:`.adb`) for a library level package
  or generic package that has a body

You need *not* compile the following files

* the spec of a library unit which has a body

* subunits

because they are compiled as part of compiling related units. GNAT
package specs
when the corresponding body is compiled, and subunits when the parent is
compiled.

.. index:: cannot generate code

If you attempt to compile any of these files, you will get one of the
following error messages (where ``fff`` is the name of the file you
compiled):

  ::

    cannot generate code for file ``fff`` (package spec)
    to check package spec, use -gnatc

    cannot generate code for file ``fff`` (missing subunits)
    to check parent unit, use -gnatc

    cannot generate code for file ``fff`` (subprogram spec)
    to check subprogram spec, use -gnatc

    cannot generate code for file ``fff`` (subunit)
    to check subunit, use -gnatc


As indicated by the above error messages, if you want to submit
one of these files to the compiler to check for correct semantics
without generating code, then use the :switch:`-gnatc` switch.

The basic command for compiling a file containing an Ada unit is:

.. code-block:: sh

  $ gcc -c [switches] <file name>

where ``file name`` is the name of the Ada file (usually
having an extension :file:`.ads` for a spec or :file:`.adb` for a body).
You specify the
:switch:`-c` switch to tell ``gcc`` to compile, but not link, the file.
The result of a successful compilation is an object file, which has the
same name as the source file but an extension of :file:`.o` and an Ada
Library Information (ALI) file, which also has the same name as the
source file, but with :file:`.ali` as the extension. GNAT creates these
two output files in the current directory, but you may specify a source
file in any directory using an absolute or relative path specification
containing the directory information.

TESTING: the :switch:`--foobar{NN}` switch

.. index::  gnat1

``gcc`` is actually a driver program that looks at the extensions of
the file arguments and loads the appropriate compiler. For example, the
GNU C compiler is :file:`cc1`, and the Ada compiler is :file:`gnat1`.
These programs are in directories known to the driver program (in some
configurations via environment variables you set), but need not be in
your path. The ``gcc`` driver also calls the assembler and any other
utilities needed to complete the generation of the required object
files.

It is possible to supply several file names on the same ``gcc``
command. This causes ``gcc`` to call the appropriate compiler for
each file. For example, the following command lists two separate
files to be compiled:

.. code-block:: sh

  $ gcc -c x.adb y.adb


calls ``gnat1`` (the Ada compiler) twice to compile :file:`x.adb` and
:file:`y.adb`.
The compiler generates two object files :file:`x.o` and :file:`y.o`
and the two ALI files :file:`x.ali` and :file:`y.ali`.

Any switches apply to all the files listed, see :ref:`Switches_for_gcc` for a
list of available ``gcc`` switches.

.. _Search_Paths_and_the_Run-Time_Library_RTL:

Search Paths and the Run-Time Library (RTL)
-------------------------------------------

With the GNAT source-based library system, the compiler must be able to
find source files for units that are needed by the unit being compiled.
Search paths are used to guide this process.

The compiler compiles one source file whose name must be given
explicitly on the command line. In other words, no searching is done
for this file. To find all other source files that are needed (the most
common being the specs of units), the compiler examines the following
directories, in the following order:

* The directory containing the source file of the main unit being compiled
  (the file name on the command line).

* Each directory named by an :switch:`-I` switch given on the ``gcc``
  command line, in the order given.

  .. index:: ADA_PRJ_INCLUDE_FILE

* Each of the directories listed in the text file whose name is given
  by the :envvar:`ADA_PRJ_INCLUDE_FILE` environment variable.
  :envvar:`ADA_PRJ_INCLUDE_FILE` is normally set by gnatmake or by the gnat
  driver when project files are used. It should not normally be set
  by other means.

  .. index:: ADA_INCLUDE_PATH

* Each of the directories listed in the value of the
  :envvar:`ADA_INCLUDE_PATH` environment variable.
  Construct this value
  exactly as the :envvar:`PATH` environment variable: a list of directory
  names separated by colons (semicolons when working with the NT version).

* The content of the :file:`ada_source_path` file which is part of the GNAT
  installation tree and is used to store standard libraries such as the
  GNAT Run Time Library (RTL) source files.
  :ref:`Installing_a_library`

Specifying the switch :switch:`-I-`
inhibits the use of the directory
containing the source file named in the command line. You can still
have this directory on your search path, but in this case it must be
explicitly requested with a :switch:`-I` switch.

Specifying the switch :switch:`-nostdinc`
inhibits the search of the default location for the GNAT Run Time
Library (RTL) source files.

The compiler outputs its object files and ALI files in the current
working directory.
Caution: The object file can be redirected with the :switch:`-o` switch;
however, ``gcc`` and ``gnat1`` have not been coordinated on this
so the :file:`ALI` file will not go to the right place. Therefore, you should
avoid using the :switch:`-o` switch.

.. index:: System.IO

The packages ``Ada``, ``System``, and ``Interfaces`` and their
children make up the GNAT RTL, together with the simple ``System.IO``
package used in the ``"Hello World"`` example. The sources for these units
are needed by the compiler and are kept together in one directory. Not
all of the bodies are needed, but all of the sources are kept together
anyway. In a normal installation, you need not specify these directory
names when compiling or binding. Either the environment variables or
the built-in defaults cause these files to be found.

In addition to the language-defined hierarchies (``System``, ``Ada`` and
``Interfaces``), the GNAT distribution provides a fourth hierarchy,
consisting of child units of ``GNAT``. This is a collection of generally
useful types, subprograms, etc. See the :title:`GNAT_Reference_Manual`
for further details.

Besides simplifying access to the RTL, a major use of search paths is
in compiling sources from multiple directories. This can make
development environments much more flexible.

.. _Order_of_Compilation_Issues:

Order of Compilation Issues
---------------------------

If, in our earlier example, there was a spec for the ``hello``
procedure, it would be contained in the file :file:`hello.ads`; yet this
file would not have to be explicitly compiled. This is the result of the
model we chose to implement library management. Some of the consequences
of this model are as follows:

* There is no point in compiling specs (except for package
  specs with no bodies) because these are compiled as needed by clients. If
  you attempt a useless compilation, you will receive an error message.
  It is also useless to compile subunits because they are compiled as needed
  by the parent.

* There are no order of compilation requirements: performing a
  compilation never obsoletes anything. The only way you can obsolete
  something and require recompilations is to modify one of the
  source files on which it depends.

* There is no library as such, apart from the ALI files
  (:ref:`The_Ada_Library_Information_Files`, for information on the format
  of these files). For now we find it convenient to create separate ALI files,
  but eventually the information therein may be incorporated into the object
  file directly.

* When you compile a unit, the source files for the specs of all units
  that it |withs|, all its subunits, and the bodies of any generics it
  instantiates must be available (reachable by the search-paths mechanism
  described above), or you will receive a fatal error message.

.. _Examples:

Examples
--------

The following are some typical Ada compilation command line examples:

.. code-block:: sh

    $ gcc -c xyz.adb

Compile body in file :file:`xyz.adb` with all default options.

.. code-block:: sh

    $ gcc -c -O2 -gnata xyz-def.adb

Compile the child unit package in file :file:`xyz-def.adb` with extensive
optimizations, and pragma ``Assert``/`Debug` statements
enabled.

.. code-block:: sh

    $ gcc -c -gnatc abc-def.adb

Compile the subunit in file :file:`abc-def.adb` in semantic-checking-only
mode.


.. _Switches_for_gcc:

Compiler Switches
=================

The ``gcc`` command accepts switches that control the
compilation process. These switches are fully described in this section:
first an alphabetical listing of all switches with a brief description,
and then functionally grouped sets of switches with more detailed
information.

More switches exist for GCC than those documented here, especially
for specific targets. However, their use is not recommended as
they may change code generation in ways that are incompatible with
the Ada run-time library, or can cause inconsistencies between
compilation units.

.. _Alphabetical_List_of_All_Switches:

Alphabetical List of All Switches
---------------------------------

.. index:: -b  (gcc)

:switch:`-b {target}`
  Compile your program to run on ``target``, which is the name of a
  system configuration. You must have a GNAT cross-compiler built if
  ``target`` is not the same as your host system.


.. index:: -B  (gcc)

:switch:`-B{dir}`
  Load compiler executables (for example, ``gnat1``, the Ada compiler)
  from ``dir`` instead of the default location. Only use this switch
  when multiple versions of the GNAT compiler are available.
  See the "Options for Directory Search" section in the
  :title:`Using the GNU Compiler Collection (GCC)` manual for further details.
  You would normally use the :switch:`-b` or :switch:`-V` switch instead.

.. index:: -c  (gcc)

:switch:`-c`
  Compile. Always use this switch when compiling Ada programs.

  Note: for some other languages when using ``gcc``, notably in
  the case of C and C++, it is possible to use
  use ``gcc`` without a :switch:`-c` switch to
  compile and link in one step. In the case of GNAT, you
  cannot use this approach, because the binder must be run
  and ``gcc`` cannot be used to run the GNAT binder.


.. index:: -fcallgraph-info  (gcc)

:switch:`-fcallgraph-info[=su,da]`
  Makes the compiler output callgraph information for the program, on a
  per-file basis. The information is generated in the VCG format.  It can
  be decorated with additional, per-node and/or per-edge information, if a
  list of comma-separated markers is additionally specified. When the
  ``su`` marker is specified, the callgraph is decorated with stack usage
  information; it is equivalent to :switch:`-fstack-usage`. When the ``da``
  marker is specified, the callgraph is decorated with information about
  dynamically allocated objects.


.. index:: -fdump-scos  (gcc)

:switch:`-fdump-scos`
  Generates SCO (Source Coverage Obligation) information in the ALI file.
  This information is used by advanced coverage tools. See unit :file:`SCOs`
  in the compiler sources for details in files :file:`scos.ads` and
  :file:`scos.adb`.


.. index:: -fgnat-encodings  (gcc)

:switch:`-fgnat-encodings=[all|gdb|minimal]`
  This switch controls the balance between GNAT encodings and standard DWARF
  emitted in the debug information.


.. index:: -flto  (gcc)

:switch:`-flto[={n}]`
  Enables Link Time Optimization. This switch must be used in conjunction
  with the :switch:`-Ox` switches (but not with the :switch:`-gnatn` switch
  since it is a full replacement for the latter) and instructs the compiler
  to defer most optimizations until the link stage. The advantage of this
  approach is that the compiler can do a whole-program analysis and choose
  the best interprocedural optimization strategy based on a complete view
  of the program, instead of a fragmentary view with the usual approach.
  This can also speed up the compilation of big programs and reduce the
  size of the executable, compared with a traditional per-unit compilation
  with inlining across units enabled by the :switch:`-gnatn` switch.
  The drawback of this approach is that it may require more memory and that
  the debugging information generated by -g with it might be hardly usable.
  The switch, as well as the accompanying :switch:`-Ox` switches, must be
  specified both for the compilation and the link phases.
  If the ``n`` parameter is specified, the optimization and final code
  generation at link time are executed using ``n`` parallel jobs by
  means of an installed ``make`` program.


.. index:: -fno-inline  (gcc)

:switch:`-fno-inline`
  Suppresses all inlining, unless requested with pragma ``Inline_Always``. The
  effect is enforced regardless of other optimization or inlining switches.
  Note that inlining can also be suppressed on a finer-grained basis with
  pragma ``No_Inline``.


.. index:: -fno-inline-functions  (gcc)

:switch:`-fno-inline-functions`
  Suppresses automatic inlining of subprograms, which is enabled
  if :switch:`-O3` is used.


.. index:: -fno-inline-small-functions  (gcc)

:switch:`-fno-inline-small-functions`
  Suppresses automatic inlining of small subprograms, which is enabled
  if :switch:`-O2` is used.


.. index:: -fno-inline-functions-called-once  (gcc)

:switch:`-fno-inline-functions-called-once`
  Suppresses inlining of subprograms local to the unit and called once
  from within it, which is enabled if :switch:`-O1` is used.


.. index:: -fno-ivopts  (gcc)

:switch:`-fno-ivopts`
  Suppresses high-level loop induction variable optimizations, which are
  enabled if :switch:`-O1` is used. These optimizations are generally
  profitable but, for some specific cases of loops with numerous uses
  of the iteration variable that follow a common pattern, they may end
  up destroying the regularity that could be exploited at a lower level
  and thus producing inferior code.


.. index:: -fno-strict-aliasing  (gcc)

:switch:`-fno-strict-aliasing`
  Causes the compiler to avoid assumptions regarding non-aliasing
  of objects of different types. See
  :ref:`Optimization_and_Strict_Aliasing` for details.


.. index:: -fno-strict-overflow  (gcc)

:switch:`-fno-strict-overflow`
  Causes the compiler to avoid assumptions regarding the rules of signed
  integer overflow. These rules specify that signed integer overflow will
  result in a Constraint_Error exception at run time and are enforced in
  default mode by the compiler, so this switch should not be necessary in
  normal operating mode. It might be useful in conjunction with :switch:`-gnato0`
  for very peculiar cases of low-level programming.


.. index:: -fstack-check  (gcc)

:switch:`-fstack-check`
  Activates stack checking.
  See :ref:`Stack_Overflow_Checking` for details.


.. index:: -fstack-usage  (gcc)

:switch:`-fstack-usage`
  Makes the compiler output stack usage information for the program, on a
  per-subprogram basis. See :ref:`Static_Stack_Usage_Analysis` for details.


.. index:: -g  (gcc)

:switch:`-g`
  Generate debugging information. This information is stored in the object
  file and copied from there to the final executable file by the linker,
  where it can be read by the debugger. You must use the
  :switch:`-g` switch if you plan on using the debugger.


.. index:: -gnat05  (gcc)

:switch:`-gnat05`
  Allow full Ada 2005 features.


.. index:: -gnat12  (gcc)

:switch:`-gnat12`
  Allow full Ada 2012 features.

.. index:: -gnat83  (gcc)

.. index:: -gnat2005  (gcc)

:switch:`-gnat2005`
  Allow full Ada 2005 features (same as :switch:`-gnat05`)


.. index:: -gnat2012  (gcc)

:switch:`-gnat2012`
  Allow full Ada 2012 features (same as :switch:`-gnat12`)


:switch:`-gnat83`
  Enforce Ada 83 restrictions.


.. index:: -gnat95  (gcc)

:switch:`-gnat95`
  Enforce Ada 95 restrictions.

  Note: for compatibility with some Ada 95 compilers which support only
  the ``overriding`` keyword of Ada 2005, the :switch:`-gnatd.D` switch can
  be used along with :switch:`-gnat95` to achieve a similar effect with GNAT.

  :switch:`-gnatd.D` instructs GNAT to consider ``overriding`` as a keyword
  and handle its associated semantic checks, even in Ada 95 mode.


.. index:: -gnata  (gcc)

:switch:`-gnata`
  Assertions enabled. ``Pragma Assert`` and ``pragma Debug`` to be
  activated. Note that these pragmas can also be controlled using the
  configuration pragmas ``Assertion_Policy`` and ``Debug_Policy``.
  It also activates pragmas ``Check``, ``Precondition``, and
  ``Postcondition``. Note that these pragmas can also be controlled
  using the configuration pragma ``Check_Policy``. In Ada 2012, it
  also activates all assertions defined in the RM as aspects: preconditions,
  postconditions, type invariants and (sub)type predicates. In all Ada modes,
  corresponding pragmas for type invariants and (sub)type predicates are
  also activated. The default is that all these assertions are disabled,
  and have no effect, other than being checked for syntactic validity, and
  in the case of subtype predicates, constructions such as membership tests
  still test predicates even if assertions are turned off.


.. index:: -gnatA  (gcc)

:switch:`-gnatA`
  Avoid processing :file:`gnat.adc`. If a :file:`gnat.adc` file is present,
  it will be ignored.


.. index:: -gnatb  (gcc)

:switch:`-gnatb`
  Generate brief messages to :file:`stderr` even if verbose mode set.


.. index:: -gnatB  (gcc)

:switch:`-gnatB`
  Assume no invalid (bad) values except for 'Valid attribute use
  (:ref:`Validity_Checking`).


.. index:: -gnatc  (gcc)

:switch:`-gnatc`
  Check syntax and semantics only (no code generation attempted). When the
  compiler is invoked by ``gnatmake``, if the switch :switch:`-gnatc` is
  only given to the compiler (after :switch:`-cargs` or in package Compiler of
  the project file, ``gnatmake`` will fail because it will not find the
  object file after compilation. If ``gnatmake`` is called with
  :switch:`-gnatc` as a builder switch (before :switch:`-cargs` or in package
  Builder of the project file) then ``gnatmake`` will not fail because
  it will not look for the object files after compilation, and it will not try
  to build and link.


.. index:: -gnatC  (gcc)

:switch:`-gnatC`
  Generate CodePeer intermediate format (no code generation attempted).
  This switch will generate an intermediate representation suitable for
  use by CodePeer (:file:`.scil` files). This switch is not compatible with
  code generation (it will, among other things, disable some switches such
  as -gnatn, and enable others such as -gnata).


.. index:: -gnatd  (gcc)

:switch:`-gnatd`
  Specify debug options for the compiler. The string of characters after
  the :switch:`-gnatd` specify the specific debug options. The possible
  characters are 0-9, a-z, A-Z, optionally preceded by a dot. See
  compiler source file :file:`debug.adb` for details of the implemented
  debug options. Certain debug options are relevant to applications
  programmers, and these are documented at appropriate points in this
  users guide.


.. index:: -gnatD[nn]  (gcc)

:switch:`-gnatD`
  Create expanded source files for source level debugging. This switch
  also suppresses generation of cross-reference information
  (see :switch:`-gnatx`). Note that this switch is not allowed if a previous
  -gnatR switch has been given, since these two switches are not compatible.


.. index:: -gnateA  (gcc)

:switch:`-gnateA`
  Check that the actual parameters of a subprogram call are not aliases of one
  another. To qualify as aliasing, the actuals must denote objects of a composite
  type, their memory locations must be identical or overlapping, and at least one
  of the corresponding formal parameters must be of mode OUT or IN OUT.


  .. code-block:: ada

      type Rec_Typ is record
         Data : Integer := 0;
      end record;

      function Self (Val : Rec_Typ) return Rec_Typ is
      begin
         return Val;
      end Self;

      procedure Detect_Aliasing (Val_1 : in out Rec_Typ; Val_2 : Rec_Typ) is
      begin
         null;
      end Detect_Aliasing;

      Obj : Rec_Typ;

      Detect_Aliasing (Obj, Obj);
      Detect_Aliasing (Obj, Self (Obj));


  In the example above, the first call to ``Detect_Aliasing`` fails with a
  ``Program_Error`` at run time because the actuals for ``Val_1`` and
  ``Val_2`` denote the same object. The second call executes without raising
  an exception because ``Self(Obj)`` produces an anonymous object which does
  not share the memory location of ``Obj``.


.. index:: -gnatec  (gcc)

:switch:`-gnatec={path}`
  Specify a configuration pragma file
  (the equal sign is optional)
  (:ref:`The_Configuration_Pragmas_Files`).


.. index:: -gnateC  (gcc)

:switch:`-gnateC`
  Generate CodePeer messages in a compiler-like format. This switch is only
  effective if :switch:`-gnatcC` is also specified and requires an installation
  of CodePeer.


.. index:: -gnated  (gcc)

:switch:`-gnated`
  Disable atomic synchronization


.. index:: -gnateD  (gcc)

:switch:`-gnateDsymbol[={value}]`
  Defines a symbol, associated with ``value``, for preprocessing.
  (:ref:`Integrated_Preprocessing`).


.. index:: -gnateE  (gcc)

:switch:`-gnateE`
  Generate extra information in exception messages. In particular, display
  extra column information and the value and range associated with index and
  range check failures, and extra column information for access checks.
  In cases where the compiler is able to determine at compile time that
  a check will fail, it gives a warning, and the extra information is not
  produced at run time.


.. index:: -gnatef  (gcc)

:switch:`-gnatef`
  Display full source path name in brief error messages.


.. index:: -gnateF  (gcc)

:switch:`-gnateF`
  Check for overflow on all floating-point operations, including those
  for unconstrained predefined types. See description of pragma
  ``Check_Float_Overflow`` in GNAT RM.


.. index:: -gnateg  (gcc)

:switch:`-gnateg`
:switch:`-gnatceg`

  The :switch:`-gnatc` switch must always be specified before this switch, e.g.
  :switch:`-gnatceg`. Generate a C header from the Ada input file. See
  :ref:`Generating_C_Headers_for_Ada_Specifications` for more
  information.


.. index:: -gnateG  (gcc)

:switch:`-gnateG`
  Save result of preprocessing in a text file.


.. index:: -gnatei  (gcc)

:switch:`-gnatei{nnn}`
  Set maximum number of instantiations during compilation of a single unit to
  ``nnn``. This may be useful in increasing the default maximum of 8000 for
  the rare case when a single unit legitimately exceeds this limit.


.. index:: -gnateI  (gcc)

:switch:`-gnateI{nnn}`
  Indicates that the source is a multi-unit source and that the index of the
  unit to compile is ``nnn``. ``nnn`` needs to be a positive number and need
  to be a valid index in the multi-unit source.


.. index:: -gnatel  (gcc)

:switch:`-gnatel`
  This switch can be used with the static elaboration model to issue info
  messages showing
  where implicit ``pragma Elaborate`` and ``pragma Elaborate_All``
  are generated. This is useful in diagnosing elaboration circularities
  caused by these implicit pragmas when using the static elaboration
  model. See See the section in this guide on elaboration checking for
  further details. These messages are not generated by default, and are
  intended only for temporary use when debugging circularity problems.


.. index:: -gnatel  (gcc)

:switch:`-gnateL`
  This switch turns off the info messages about implicit elaboration pragmas.


.. index:: -gnatem  (gcc)

:switch:`-gnatem={path}`
  Specify a mapping file
  (the equal sign is optional)
  (:ref:`Units_to_Sources_Mapping_Files`).


.. index:: -gnatep  (gcc)

:switch:`-gnatep={file}`
  Specify a preprocessing data file
  (the equal sign is optional)
  (:ref:`Integrated_Preprocessing`).


.. index:: -gnateP  (gcc)

:switch:`-gnateP`
  Turn categorization dependency errors into warnings.
  Ada requires that units that WITH one another have compatible categories, for
  example a Pure unit cannot WITH a Preelaborate unit. If this switch is used,
  these errors become warnings (which can be ignored, or suppressed in the usual
  manner). This can be useful in some specialized circumstances such as the
  temporary use of special test software.


.. index:: -gnateS  (gcc)

:switch:`-gnateS`
  Synonym of :switch:`-fdump-scos`, kept for backwards compatibility.


.. index:: -gnatet=file  (gcc)

:switch:`-gnatet={path}`
  Generate target dependent information. The format of the output file is
  described in the section about switch :switch:`-gnateT`.


.. index:: -gnateT  (gcc)

:switch:`-gnateT={path}`
  Read target dependent information, such as endianness or sizes and alignments
  of base type. If this switch is passed, the default target dependent
  information of the compiler is replaced by the one read from the input file.
  This is used by tools other than the compiler, e.g. to do
  semantic analysis of programs that will run on some other target than
  the machine on which the tool is run.

  The following target dependent values should be defined,
  where ``Nat`` denotes a natural integer value, ``Pos`` denotes a
  positive integer value, and fields marked with a question mark are
  boolean fields, where a value of 0 is False, and a value of 1 is True:


  ::

    Bits_BE                    : Nat; -- Bits stored big-endian?
    Bits_Per_Unit              : Pos; -- Bits in a storage unit
    Bits_Per_Word              : Pos; -- Bits in a word
    Bytes_BE                   : Nat; -- Bytes stored big-endian?
    Char_Size                  : Pos; -- Standard.Character'Size
    Double_Float_Alignment     : Nat; -- Alignment of double float
    Double_Scalar_Alignment    : Nat; -- Alignment of double length scalar
    Double_Size                : Pos; -- Standard.Long_Float'Size
    Float_Size                 : Pos; -- Standard.Float'Size
    Float_Words_BE             : Nat; -- Float words stored big-endian?
    Int_Size                   : Pos; -- Standard.Integer'Size
    Long_Double_Size           : Pos; -- Standard.Long_Long_Float'Size
    Long_Long_Size             : Pos; -- Standard.Long_Long_Integer'Size
    Long_Size                  : Pos; -- Standard.Long_Integer'Size
    Maximum_Alignment          : Pos; -- Maximum permitted alignment
    Max_Unaligned_Field        : Pos; -- Maximum size for unaligned bit field
    Pointer_Size               : Pos; -- System.Address'Size
    Short_Enums                : Nat; -- Foreign enums use short size?
    Short_Size                 : Pos; -- Standard.Short_Integer'Size
    Strict_Alignment           : Nat; -- Strict alignment?
    System_Allocator_Alignment : Nat; -- Alignment for malloc calls
    Wchar_T_Size               : Pos; -- Interfaces.C.wchar_t'Size
    Words_BE                   : Nat; -- Words stored big-endian?

  ``Bits_Per_Unit`` is the number of bits in a storage unit, the equivalent of
  GCC macro ``BITS_PER_UNIT`` documented as follows: `Define this macro to be
  the number of bits in an addressable storage unit (byte); normally 8.`

  ``Bits_Per_Word`` is the number of bits in a machine word, the equivalent of
  GCC macro ``BITS_PER_WORD`` documented as follows: `Number of bits in a word;
  normally 32.`

  ``Double_Float_Alignment``, if not zero, is the maximum alignment that the
  compiler can choose by default for a 64-bit floating-point type or object.

  ``Double_Scalar_Alignment``, if not zero, is the maximum alignment that the
  compiler can choose by default for a 64-bit or larger scalar type or object.

  ``Maximum_Alignment`` is the maximum alignment that the compiler can choose
  by default for a type or object, which is also the maximum alignment that can
  be specified in GNAT. It is computed for GCC backends as ``BIGGEST_ALIGNMENT
  / BITS_PER_UNIT`` where GCC macro ``BIGGEST_ALIGNMENT`` is documented as
  follows: `Biggest alignment that any data type can require on this machine,
  in bits.`

  ``Max_Unaligned_Field`` is the maximum size for unaligned bit field, which is
  64 for the majority of GCC targets (but can be different on some targets like
  AAMP).

  ``Strict_Alignment`` is the equivalent of GCC macro ``STRICT_ALIGNMENT``
  documented as follows: `Define this macro to be the value 1 if instructions
  will fail to work if given data not on the nominal alignment. If instructions
  will merely go slower in that case, define this macro as 0.`

  ``System_Allocator_Alignment`` is the guaranteed alignment of data returned
  by calls to ``malloc``.


  The format of the input file is as follows. First come the values of
  the variables defined above, with one line per value:


  ::

    name  value

  where ``name`` is the name of the parameter, spelled out in full,
  and cased as in the above list, and ``value`` is an unsigned decimal
  integer. Two or more blanks separates the name from the value.

  All the variables must be present, in alphabetical order (i.e. the
  same order as the list above).

  Then there is a blank line to separate the two parts of the file. Then
  come the lines showing the floating-point types to be registered, with
  one line per registered mode:


  ::

    name  digs float_rep size alignment


  where ``name`` is the string name of the type (which can have
  single spaces embedded in the name (e.g. long double), ``digs`` is
  the number of digits for the floating-point type, ``float_rep`` is
  the float representation (I/V/A for IEEE-754-Binary, Vax_Native,
  AAMP), ``size`` is the size in bits, ``alignment`` is the
  alignment in bits. The name is followed by at least two blanks, fields
  are separated by at least one blank, and a LF character immediately
  follows the alignment field.

  Here is an example of a target parameterization file:


  ::

    Bits_BE                       0
    Bits_Per_Unit                 8
    Bits_Per_Word                64
    Bytes_BE                      0
    Char_Size                     8
    Double_Float_Alignment        0
    Double_Scalar_Alignment       0
    Double_Size                  64
    Float_Size                   32
    Float_Words_BE                0
    Int_Size                     64
    Long_Double_Size            128
    Long_Long_Size               64
    Long_Size                    64
    Maximum_Alignment            16
    Max_Unaligned_Field          64
    Pointer_Size                 64
    Short_Size                   16
    Strict_Alignment              0
    System_Allocator_Alignment   16
    Wchar_T_Size                 32
    Words_BE                      0

    float         15  I  64  64
    double        15  I  64  64
    long double   18  I  80 128
    TF            33  I 128 128



.. index:: -gnateu  (gcc)

:switch:`-gnateu`
  Ignore unrecognized validity, warning, and style switches that
  appear after this switch is given. This may be useful when
  compiling sources developed on a later version of the compiler
  with an earlier version. Of course the earlier version must
  support this switch.


.. index:: -gnateV  (gcc)

:switch:`-gnateV`
  Check that all actual parameters of a subprogram call are valid according to
  the rules of validity checking (:ref:`Validity_Checking`).


.. index:: -gnateY  (gcc)

:switch:`-gnateY`
  Ignore all STYLE_CHECKS pragmas. Full legality checks
  are still carried out, but the pragmas have no effect
  on what style checks are active. This allows all style
  checking options to be controlled from the command line.


.. index:: -gnatE  (gcc)

:switch:`-gnatE`
  Dynamic elaboration checking mode enabled. For further details see
  :ref:`Elaboration_Order_Handling_in_GNAT`.


.. index:: -gnatf  (gcc)

:switch:`-gnatf`
  Full errors. Multiple errors per line, all undefined references, do not
  attempt to suppress cascaded errors.


.. index:: -gnatF  (gcc)

:switch:`-gnatF`
  Externals names are folded to all uppercase.


.. index:: -gnatg  (gcc)

:switch:`-gnatg`
  Internal GNAT implementation mode. This should not be used for applications
  programs, it is intended only for use by the compiler and its run-time
  library. For documentation, see the GNAT sources. Note that :switch:`-gnatg`
  implies :switch:`-gnatw.ge` and :switch:`-gnatyg` so that all standard
  warnings and all standard style options are turned on. All warnings and style
  messages are treated as errors.


.. index:: -gnatG[nn]  (gcc)

:switch:`-gnatG=nn`
  List generated expanded code in source form.


.. index:: -gnath  (gcc)

:switch:`-gnath`
  Output usage information. The output is written to :file:`stdout`.


.. index:: -gnatH  (gcc)

:switch:`-gnatH`
  Legacy elaboration-checking mode enabled. When this switch is in effect,
  the pre-18.x access-before-elaboration model becomes the de facto model.
  For further details see :ref:`Elaboration_Order_Handling_in_GNAT`.


.. index:: -gnati  (gcc)

:switch:`-gnati{c}`
  Identifier character set (``c`` = 1/2/3/4/8/9/p/f/n/w).
  For details of the possible selections for ``c``,
  see :ref:`Character_Set_Control`.


.. index:: -gnatI  (gcc)

:switch:`-gnatI`
  Ignore representation clauses. When this switch is used,
  representation clauses are treated as comments. This is useful
  when initially porting code where you want to ignore rep clause
  problems, and also for compiling foreign code (particularly
  for use with ASIS). The representation clauses that are ignored
  are: enumeration_representation_clause, record_representation_clause,
  and attribute_definition_clause for the following attributes:
  Address, Alignment, Bit_Order, Component_Size, Machine_Radix,
  Object_Size, Scalar_Storage_Order, Size, Small, Stream_Size,
  and Value_Size. Pragma Default_Scalar_Storage_Order is also ignored.
  Note that this option should be used only for compiling -- the
  code is likely to malfunction at run time.


.. index:: -gnatjnn  (gcc)

:switch:`-gnatj{nn}`
  Reformat error messages to fit on ``nn`` character lines


.. index:: -gnatJ  (gcc)

:switch:`-gnatJ`
  Permissive elaboration-checking mode enabled. When this switch is in effect,
  the post-18.x access-before-elaboration model ignores potential issues with:

  - Accept statements
  - Activations of tasks defined in instances
  - Assertion pragmas
  - Calls from within an instance to its enclosing context
  - Calls through generic formal parameters
  - Calls to subprograms defined in instances
  - Entry calls
  - Indirect calls using 'Access
  - Requeue statements
  - Select statements
  - Synchronous task suspension

  and does not emit compile-time diagnostics or run-time checks. For further
  details see :ref:`Elaboration_Order_Handling_in_GNAT`.


.. index:: -gnatk  (gcc)

:switch:`-gnatk={n}`
  Limit file names to ``n`` (1-999) characters (``k`` = krunch).


.. index:: -gnatl  (gcc)

:switch:`-gnatl`
  Output full source listing with embedded error messages.


.. index:: -gnatL  (gcc)

:switch:`-gnatL`
  Used in conjunction with -gnatG or -gnatD to intersperse original
  source lines (as comment lines with line numbers) in the expanded
  source output.


.. index:: -gnatm  (gcc)

:switch:`-gnatm={n}`
  Limit number of detected error or warning messages to ``n``
  where ``n`` is in the range 1..999999. The default setting if
  no switch is given is 9999. If the number of warnings reaches this
  limit, then a message is output and further warnings are suppressed,
  but the compilation is continued. If the number of error messages
  reaches this limit, then a message is output and the compilation
  is abandoned. The equal sign here is optional. A value of zero
  means that no limit applies.


.. index:: -gnatn  (gcc)

:switch:`-gnatn[12]`
  Activate inlining across units for subprograms for which pragma ``Inline``
  is specified. This inlining is performed by the GCC back-end. An optional
  digit sets the inlining level: 1 for moderate inlining across units
  or 2 for full inlining across units. If no inlining level is specified,
  the compiler will pick it based on the optimization level.


.. index:: -gnatN  (gcc)

:switch:`-gnatN`
  Activate front end inlining for subprograms for which
  pragma ``Inline`` is specified. This inlining is performed
  by the front end and will be visible in the
  :switch:`-gnatG` output.

  When using a gcc-based back end (in practice this means using any version
  of GNAT other than the JGNAT, .NET or GNAAMP versions), then the use of
  :switch:`-gnatN` is deprecated, and the use of :switch:`-gnatn` is preferred.
  Historically front end inlining was more extensive than the gcc back end
  inlining, but that is no longer the case.


.. index:: -gnato0  (gcc)

:switch:`-gnato0`
  Suppresses overflow checking. This causes the behavior of the compiler to
  match the default for older versions where overflow checking was suppressed
  by default. This is equivalent to having
  ``pragma Suppress (Overflow_Check)`` in a configuration pragma file.


.. index:: -gnato??  (gcc)

:switch:`-gnato??`
  Set default mode for handling generation of code to avoid intermediate
  arithmetic overflow. Here ``??`` is two digits, a
  single digit, or nothing. Each digit is one of the digits ``1``
  through ``3``:

  ===== ===============================================================
  Digit Interpretation
  ----- ---------------------------------------------------------------
  *1*   All intermediate overflows checked against base type (``STRICT``)
  *2*   Minimize intermediate overflows (``MINIMIZED``)
  *3*   Eliminate intermediate overflows (``ELIMINATED``)
  ===== ===============================================================

  If only one digit appears, then it applies to all
  cases; if two digits are given, then the first applies outside
  assertions, pre/postconditions, and type invariants, and the second
  applies within assertions, pre/postconditions, and type invariants.

  If no digits follow the :switch:`-gnato`, then it is equivalent to
  :switch:`-gnato11`,
  causing all intermediate overflows to be handled in strict
  mode.

  This switch also causes arithmetic overflow checking to be performed
  (as though ``pragma Unsuppress (Overflow_Check)`` had been specified).

  The default if no option :switch:`-gnato` is given is that overflow handling
  is in ``STRICT`` mode (computations done using the base type), and that
  overflow checking is enabled.

  Note that division by zero is a separate check that is not
  controlled by this switch (divide-by-zero checking is on by default).

  See also :ref:`Specifying_the_Desired_Mode`.


.. index:: -gnatp  (gcc)

:switch:`-gnatp`
  Suppress all checks. See :ref:`Run-Time_Checks` for details. This switch
  has no effect if cancelled by a subsequent :switch:`-gnat-p` switch.


.. index:: -gnat-p  (gcc)

:switch:`-gnat-p`
  Cancel effect of previous :switch:`-gnatp` switch.


.. index:: -gnatP  (gcc)

:switch:`-gnatP`
  Enable polling. This is required on some systems (notably Windows NT) to
  obtain asynchronous abort and asynchronous transfer of control capability.
  See ``Pragma_Polling`` in the :title:`GNAT_Reference_Manual` for full
  details.


.. index:: -gnatq  (gcc)

:switch:`-gnatq`
  Don't quit. Try semantics, even if parse errors.


.. index:: -gnatQ  (gcc)

:switch:`-gnatQ`
  Don't quit. Generate :file:`ALI` and tree files even if illegalities.
  Note that code generation is still suppressed in the presence of any
  errors, so even with :switch:`-gnatQ` no object file is generated.


.. index:: -gnatr  (gcc)

:switch:`-gnatr`
  Treat pragma Restrictions as Restriction_Warnings.


.. index:: -gnatR  (gcc)

:switch:`-gnatR[0|1|2|3|4][e][j][m][s]`
  Output representation information for declared types, objects and
  subprograms. Note that this switch is not allowed if a previous
  :switch:`-gnatD` switch has been given, since these two switches
  are not compatible.


.. index:: -gnats  (gcc)

:switch:`-gnats`
  Syntax check only.


.. index:: -gnatS  (gcc)

:switch:`-gnatS`
  Print package Standard.


.. index:: -gnatT  (gcc)

:switch:`-gnatT{nnn}`
  All compiler tables start at ``nnn`` times usual starting size.


.. index:: -gnatu  (gcc)

:switch:`-gnatu`
  List units for this compilation.


.. index:: -gnatU  (gcc)

:switch:`-gnatU`
  Tag all error messages with the unique string 'error:'


.. index:: -gnatv  (gcc)

:switch:`-gnatv`
  Verbose mode. Full error output with source lines to :file:`stdout`.


.. index:: -gnatV  (gcc)

:switch:`-gnatV`
  Control level of validity checking (:ref:`Validity_Checking`).


.. index:: -gnatw  (gcc)

:switch:`-gnatw{xxx}`
  Warning mode where
  ``xxx`` is a string of option letters that denotes
  the exact warnings that
  are enabled or disabled (:ref:`Warning_Message_Control`).


.. index:: -gnatW  (gcc)

:switch:`-gnatW{e}`
  Wide character encoding method
  (``e``\ =n/h/u/s/e/8).


.. index:: -gnatx  (gcc)

:switch:`-gnatx`
  Suppress generation of cross-reference information.


.. index:: -gnatX  (gcc)

:switch:`-gnatX`
  Enable GNAT implementation extensions and latest Ada version.


.. index:: -gnaty  (gcc)

:switch:`-gnaty`
  Enable built-in style checks (:ref:`Style_Checking`).


.. index:: -gnatz  (gcc)

:switch:`-gnatz{m}`
  Distribution stub generation and compilation
  (``m``\ =r/c for receiver/caller stubs).


.. index:: -I  (gcc)

:switch:`-I{dir}`
  .. index:: RTL

  Direct GNAT to search the ``dir`` directory for source files needed by
  the current compilation
  (see :ref:`Search_Paths_and_the_Run-Time_Library_RTL`).


.. index:: -I-  (gcc)

:switch:`-I-`
  .. index:: RTL

  Except for the source file named in the command line, do not look for source
  files in the directory containing the source file named in the command line
  (see :ref:`Search_Paths_and_the_Run-Time_Library_RTL`).


.. index:: -o  (gcc)

:switch:`-o {file}`
  This switch is used in ``gcc`` to redirect the generated object file
  and its associated ALI file. Beware of this switch with GNAT, because it may
  cause the object file and ALI file to have different names which in turn
  may confuse the binder and the linker.


.. index:: -nostdinc  (gcc)

:switch:`-nostdinc`
  Inhibit the search of the default location for the GNAT Run Time
  Library (RTL) source files.


.. index:: -nostdlib  (gcc)

:switch:`-nostdlib`
  Inhibit the search of the default location for the GNAT Run Time
  Library (RTL) ALI files.


.. index:: -O  (gcc)

:switch:`-O[{n}]`
  ``n`` controls the optimization level:

  ======= ==================================================================
   *n*     Effect
  ------- ------------------------------------------------------------------
  *0*      No optimization, the default setting if no :switch:`-O` appears
  *1*      Normal optimization, the default if you specify :switch:`-O` without an
           operand. A good compromise between code quality and compilation
           time.
  *2*      Extensive optimization, may improve execution time, possibly at
           the cost of substantially increased compilation time.
  *3*      Same as :switch:`-O2`, and also includes inline expansion for small
           subprograms in the same unit.
  *s*      Optimize space usage
  ======= ==================================================================

  See also :ref:`Optimization_Levels`.


.. index:: -pass-exit-codes  (gcc)

:switch:`-pass-exit-codes`
  Catch exit codes from the compiler and use the most meaningful as
  exit status.


.. index:: --RTS  (gcc)

:switch:`--RTS={rts-path}`
  Specifies the default location of the run-time library. Same meaning as the
  equivalent ``gnatmake`` flag (:ref:`Switches_for_gnatmake`).


.. index:: -S  (gcc)

:switch:`-S`
  Used in place of :switch:`-c` to
  cause the assembler source file to be
  generated, using :file:`.s` as the extension,
  instead of the object file.
  This may be useful if you need to examine the generated assembly code.


.. index:: -fverbose-asm  (gcc)

:switch:`-fverbose-asm`
  Used in conjunction with :switch:`-S`
  to cause the generated assembly code file to be annotated with variable
  names, making it significantly easier to follow.


.. index:: -v  (gcc)

:switch:`-v`
  Show commands generated by the ``gcc`` driver. Normally used only for
  debugging purposes or if you need to be sure what version of the
  compiler you are executing.


.. index:: -V  (gcc)

:switch:`-V {ver}`
  Execute ``ver`` version of the compiler. This is the ``gcc``
  version, not the GNAT version.


.. index:: -w  (gcc)

:switch:`-w`
  Turn off warnings generated by the back end of the compiler. Use of
  this switch also causes the default for front end warnings to be set
  to suppress (as though :switch:`-gnatws` had appeared at the start of
  the options).


.. index:: Combining GNAT switches

You may combine a sequence of GNAT switches into a single switch. For
example, the combined switch

  ::

    -gnatofi3

is equivalent to specifying the following sequence of switches:

  ::

    -gnato -gnatf -gnati3

The following restrictions apply to the combination of switches
in this manner:

* The switch :switch:`-gnatc` if combined with other switches must come
  first in the string.

* The switch :switch:`-gnats` if combined with other switches must come
  first in the string.

* The switches
  :switch:`-gnatzc` and :switch:`-gnatzr` may not be combined with any other
  switches, and only one of them may appear in the command line.

* The switch :switch:`-gnat-p` may not be combined with any other switch.

* Once a 'y' appears in the string (that is a use of the :switch:`-gnaty`
  switch), then all further characters in the switch are interpreted
  as style modifiers (see description of :switch:`-gnaty`).

* Once a 'd' appears in the string (that is a use of the :switch:`-gnatd`
  switch), then all further characters in the switch are interpreted
  as debug flags (see description of :switch:`-gnatd`).

* Once a 'w' appears in the string (that is a use of the :switch:`-gnatw`
  switch), then all further characters in the switch are interpreted
  as warning mode modifiers (see description of :switch:`-gnatw`).

* Once a 'V' appears in the string (that is a use of the :switch:`-gnatV`
  switch), then all further characters in the switch are interpreted
  as validity checking options (:ref:`Validity_Checking`).

* Option 'em', 'ec', 'ep', 'l=' and 'R' must be the last options in
  a combined list of options.

.. _Output_and_Error_Message_Control:

Output and Error Message Control
--------------------------------

.. index:: stderr

The standard default format for error messages is called 'brief format'.
Brief format messages are written to :file:`stderr` (the standard error
file) and have the following form:

::

  e.adb:3:04: Incorrect spelling of keyword "function"
  e.adb:4:20: ";" should be "is"

The first integer after the file name is the line number in the file,
and the second integer is the column number within the line.
``GNAT Studio`` can parse the error messages
and point to the referenced character.
The following switches provide control over the error message
format:


.. index:: -gnatv  (gcc)

:switch:`-gnatv`
  The ``v`` stands for verbose.
  The effect of this setting is to write long-format error
  messages to :file:`stdout` (the standard output file.
  The same program compiled with the
  :switch:`-gnatv` switch would generate:

  ::

    3. funcion X (Q : Integer)
       |
    >>> Incorrect spelling of keyword "function"
    4. return Integer;
                     |
    >>> ";" should be "is"


  The vertical bar indicates the location of the error, and the ``>>>``
  prefix can be used to search for error messages. When this switch is
  used the only source lines output are those with errors.


.. index:: -gnatl  (gcc)

:switch:`-gnatl`
  The ``l`` stands for list.
  This switch causes a full listing of
  the file to be generated. In the case where a body is
  compiled, the corresponding spec is also listed, along
  with any subunits. Typical output from compiling a package
  body :file:`p.adb` might look like::

    Compiling: p.adb

         1. package body p is
         2.    procedure a;
         3.    procedure a is separate;
         4. begin
         5.    null
                   |
            >>> missing ";"

         6. end;

    Compiling: p.ads

         1. package p is
         2.    pragma Elaborate_Body
                                    |
            >>> missing ";"

         3. end p;

    Compiling: p-a.adb

         1. separate p
                    |
            >>> missing "("

         2. procedure a is
         3. begin
         4.    null
                   |
            >>> missing ";"

         5. end;


  When you specify the :switch:`-gnatv` or :switch:`-gnatl` switches and
  standard output is redirected, a brief summary is written to
  :file:`stderr` (standard error) giving the number of error messages and
  warning messages generated.


.. index:: -gnatl=fname  (gcc)

:switch:`-gnatl={fname}`
  This has the same effect as :switch:`-gnatl` except that the output is
  written to a file instead of to standard output. If the given name
  :file:`fname` does not start with a period, then it is the full name
  of the file to be written. If :file:`fname` is an extension, it is
  appended to the name of the file being compiled. For example, if
  file :file:`xyz.adb` is compiled with :switch:`-gnatl=.lst`,
  then the output is written to file xyz.adb.lst.


.. index:: -gnatU  (gcc)

:switch:`-gnatU`
  This switch forces all error messages to be preceded by the unique
  string 'error:'. This means that error messages take a few more
  characters in space, but allows easy searching for and identification
  of error messages.


.. index:: -gnatb  (gcc)

:switch:`-gnatb`
  The ``b`` stands for brief.
  This switch causes GNAT to generate the
  brief format error messages to :file:`stderr` (the standard error
  file) as well as the verbose
  format message or full listing (which as usual is written to
  :file:`stdout` (the standard output file).


.. index:: -gnatm  (gcc)

:switch:`-gnatm={n}`
  The ``m`` stands for maximum.
  ``n`` is a decimal integer in the
  range of 1 to 999999 and limits the number of error or warning
  messages to be generated. For example, using
  :switch:`-gnatm2` might yield

  ::

    e.adb:3:04: Incorrect spelling of keyword "function"
    e.adb:5:35: missing ".."
    fatal error: maximum number of errors detected
    compilation abandoned


  The default setting if
  no switch is given is 9999. If the number of warnings reaches this
  limit, then a message is output and further warnings are suppressed,
  but the compilation is continued. If the number of error messages
  reaches this limit, then a message is output and the compilation
  is abandoned. A value of zero means that no limit applies.

  Note that the equal sign is optional, so the switches
  :switch:`-gnatm2` and :switch:`-gnatm=2` are equivalent.


.. index:: -gnatf  (gcc)

:switch:`-gnatf`
  .. index:: Error messages, suppressing

  The ``f`` stands for full.
  Normally, the compiler suppresses error messages that are likely to be
  redundant. This switch causes all error
  messages to be generated. In particular, in the case of
  references to undefined variables. If a given variable is referenced
  several times, the normal format of messages is

  ::

    e.adb:7:07: "V" is undefined (more references follow)

  where the parenthetical comment warns that there are additional
  references to the variable ``V``. Compiling the same program with the
  :switch:`-gnatf` switch yields

  ::

    e.adb:7:07: "V" is undefined
    e.adb:8:07: "V" is undefined
    e.adb:8:12: "V" is undefined
    e.adb:8:16: "V" is undefined
    e.adb:9:07: "V" is undefined
    e.adb:9:12: "V" is undefined

  The :switch:`-gnatf` switch also generates additional information for
  some error messages.  Some examples are:

  * Details on possibly non-portable unchecked conversion

  * List possible interpretations for ambiguous calls

  * Additional details on incorrect parameters


.. index:: -gnatjnn  (gcc)

:switch:`-gnatjnn`
  In normal operation mode (or if :switch:`-gnatj0` is used), then error messages
  with continuation lines are treated as though the continuation lines were
  separate messages (and so a warning with two continuation lines counts as
  three warnings, and is listed as three separate messages).

  If the :switch:`-gnatjnn` switch is used with a positive value for nn, then
  messages are output in a different manner. A message and all its continuation
  lines are treated as a unit, and count as only one warning or message in the
  statistics totals. Furthermore, the message is reformatted so that no line
  is longer than nn characters.


.. index:: -gnatq  (gcc)

:switch:`-gnatq`
  The ``q`` stands for quit (really 'don't quit').
  In normal operation mode, the compiler first parses the program and
  determines if there are any syntax errors. If there are, appropriate
  error messages are generated and compilation is immediately terminated.
  This switch tells
  GNAT to continue with semantic analysis even if syntax errors have been
  found. This may enable the detection of more errors in a single run. On
  the other hand, the semantic analyzer is more likely to encounter some
  internal fatal error when given a syntactically invalid tree.


.. index:: -gnatQ  (gcc)

:switch:`-gnatQ`
  In normal operation mode, the :file:`ALI` file is not generated if any
  illegalities are detected in the program. The use of :switch:`-gnatQ` forces
  generation of the :file:`ALI` file. This file is marked as being in
  error, so it cannot be used for binding purposes, but it does contain
  reasonably complete cross-reference information, and thus may be useful
  for use by tools (e.g., semantic browsing tools or integrated development
  environments) that are driven from the :file:`ALI` file. This switch
  implies :switch:`-gnatq`, since the semantic phase must be run to get a
  meaningful ALI file.

  When :switch:`-gnatQ` is used and the generated :file:`ALI` file is marked as
  being in error, ``gnatmake`` will attempt to recompile the source when it
  finds such an :file:`ALI` file, including with switch :switch:`-gnatc`.

  Note that :switch:`-gnatQ` has no effect if :switch:`-gnats` is specified,
  since ALI files are never generated if :switch:`-gnats` is set.


.. _Warning_Message_Control:

Warning Message Control
-----------------------

.. index:: Warning messages

In addition to error messages, which correspond to illegalities as defined
in the Ada Reference Manual, the compiler detects two kinds of warning
situations.

First, the compiler considers some constructs suspicious and generates a
warning message to alert you to a possible error. Second, if the
compiler detects a situation that is sure to raise an exception at
run time, it generates a warning message. The following shows an example
of warning messages:

::

  e.adb:4:24: warning: creation of object may raise Storage_Error
  e.adb:10:17: warning: static value out of range
  e.adb:10:17: warning: "Constraint_Error" will be raised at run time


GNAT considers a large number of situations as appropriate
for the generation of warning messages. As always, warnings are not
definite indications of errors. For example, if you do an out-of-range
assignment with the deliberate intention of raising a
``Constraint_Error`` exception, then the warning that may be
issued does not indicate an error. Some of the situations for which GNAT
issues warnings (at least some of the time) are given in the following
list. This list is not complete, and new warnings are often added to
subsequent versions of GNAT. The list is intended to give a general idea
of the kinds of warnings that are generated.

* Possible infinitely recursive calls

* Out-of-range values being assigned

* Possible order of elaboration problems

* Size not a multiple of alignment for a record type

* Assertions (pragma Assert) that are sure to fail

* Unreachable code

* Address clauses with possibly unaligned values, or where an attempt is
  made to overlay a smaller variable with a larger one.

* Fixed-point type declarations with a null range

* Direct_IO or Sequential_IO instantiated with a type that has access values

* Variables that are never assigned a value

* Variables that are referenced before being initialized

* Task entries with no corresponding ``accept`` statement

* Duplicate accepts for the same task entry in a ``select``

* Objects that take too much storage

* Unchecked conversion between types of differing sizes

* Missing ``return`` statement along some execution path in a function

* Incorrect (unrecognized) pragmas

* Incorrect external names

* Allocation from empty storage pool

* Potentially blocking operation in protected type

* Suspicious parenthesization of expressions

* Mismatching bounds in an aggregate

* Attempt to return local value by reference

* Premature instantiation of a generic body

* Attempt to pack aliased components

* Out of bounds array subscripts

* Wrong length on string assignment

* Violations of style rules if style checking is enabled

* Unused |with| clauses

* ``Bit_Order`` usage that does not have any effect

* ``Standard.Duration`` used to resolve universal fixed expression

* Dereference of possibly null value

* Declaration that is likely to cause storage error

* Internal GNAT unit |withed| by application unit

* Values known to be out of range at compile time

* Unreferenced or unmodified variables. Note that a special
  exemption applies to variables which contain any of the substrings
  ``DISCARD, DUMMY, IGNORE, JUNK, UNUSED``, in any casing. Such variables
  are considered likely to be intentionally used in a situation where
  otherwise a warning would be given, so warnings of this kind are
  always suppressed for such variables.

* Address overlays that could clobber memory

* Unexpected initialization when address clause present

* Bad alignment for address clause

* Useless type conversions

* Redundant assignment statements and other redundant constructs

* Useless exception handlers

* Accidental hiding of name by child unit

* Access before elaboration detected at compile time

* A range in a ``for`` loop that is known to be null or might be null


The following section lists compiler switches that are available
to control the handling of warning messages. It is also possible
to exercise much finer control over what warnings are issued and
suppressed using the GNAT pragma Warnings (see the description
of the pragma in the :title:`GNAT_Reference_manual`).


.. index:: -gnatwa  (gcc)

:switch:`-gnatwa`
  *Activate most optional warnings.*

  This switch activates most optional warning messages. See the remaining list
  in this section for details on optional warning messages that can be
  individually controlled.  The warnings that are not turned on by this
  switch are:


  * :switch:`-gnatwd` (implicit dereferencing)

  * :switch:`-gnatw.d` (tag warnings with -gnatw switch)

  * :switch:`-gnatwh` (hiding)

  * :switch:`-gnatw.h` (holes in record layouts)

  * :switch:`-gnatw.j` (late primitives of tagged types)

  * :switch:`-gnatw.k` (redefinition of names in standard)

  * :switch:`-gnatwl` (elaboration warnings)

  * :switch:`-gnatw.l` (inherited aspects)

  * :switch:`-gnatw.n` (atomic synchronization)

  * :switch:`-gnatwo` (address clause overlay)

  * :switch:`-gnatw.o` (values set by out parameters ignored)

  * :switch:`-gnatw.q` (questionable layout of record types)

  * :switch:`-gnatw_r` (out-of-order record representation clauses)

  * :switch:`-gnatw.s` (overridden size clause)

  * :switch:`-gnatwt` (tracking of deleted conditional code)

  * :switch:`-gnatw.u` (unordered enumeration)

  * :switch:`-gnatw.w` (use of Warnings Off)

  * :switch:`-gnatw.y` (reasons for package needing body)

  All other optional warnings are turned on.


.. index:: -gnatwA  (gcc)

:switch:`-gnatwA`
  *Suppress all optional errors.*

  This switch suppresses all optional warning messages, see remaining list
  in this section for details on optional warning messages that can be
  individually controlled. Note that unlike switch :switch:`-gnatws`, the
  use of switch :switch:`-gnatwA` does not suppress warnings that are
  normally given unconditionally and cannot be individually controlled
  (for example, the warning about a missing exit path in a function).
  Also, again unlike switch :switch:`-gnatws`, warnings suppressed by
  the use of switch :switch:`-gnatwA` can be individually turned back
  on. For example the use of switch :switch:`-gnatwA` followed by
  switch :switch:`-gnatwd` will suppress all optional warnings except
  the warnings for implicit dereferencing.

.. index:: -gnatw.a  (gcc)

:switch:`-gnatw.a`
  *Activate warnings on failing assertions.*

  .. index:: Assert failures

  This switch activates warnings for assertions where the compiler can tell at
  compile time that the assertion will fail. Note that this warning is given
  even if assertions are disabled. The default is that such warnings are
  generated.


.. index:: -gnatw.A  (gcc)

:switch:`-gnatw.A`
  *Suppress warnings on failing assertions.*

  .. index:: Assert failures

  This switch suppresses warnings for assertions where the compiler can tell at
  compile time that the assertion will fail.


.. index:: -gnatw_a

:switch:`-gnatw_a`
  *Activate warnings on anonymous allocators.*

  .. index:: Anonymous allocators

  This switch activates warnings for allocators of anonymous access types,
  which can involve run-time accessibility checks and lead to unexpected
  accessibility violations. For more details on the rules involved, see
  RM 3.10.2 (14).


.. index:: -gnatw_A

:switch:`-gnatw_A`
  *Supress warnings on anonymous allocators.*

  .. index:: Anonymous allocators

  This switch suppresses warnings for anonymous access type allocators.


.. index:: -gnatwb  (gcc)

:switch:`-gnatwb`
  *Activate warnings on bad fixed values.*

  .. index:: Bad fixed values

  .. index:: Fixed-point Small value

  .. index:: Small value

  This switch activates warnings for static fixed-point expressions whose
  value is not an exact multiple of Small. Such values are implementation
  dependent, since an implementation is free to choose either of the multiples
  that surround the value. GNAT always chooses the closer one, but this is not
  required behavior, and it is better to specify a value that is an exact
  multiple, ensuring predictable execution. The default is that such warnings
  are not generated.


.. index:: -gnatwB  (gcc)

:switch:`-gnatwB`
  *Suppress warnings on bad fixed values.*

  This switch suppresses warnings for static fixed-point expressions whose
  value is not an exact multiple of Small.


.. index:: -gnatw.b  (gcc)

:switch:`-gnatw.b`
  *Activate warnings on biased representation.*

  .. index:: Biased representation

  This switch activates warnings when a size clause, value size clause, component
  clause, or component size clause forces the use of biased representation for an
  integer type (e.g. representing a range of 10..11 in a single bit by using 0/1
  to represent 10/11). The default is that such warnings are generated.


.. index:: -gnatwB  (gcc)

:switch:`-gnatw.B`
  *Suppress warnings on biased representation.*

  This switch suppresses warnings for representation clauses that force the use
  of biased representation.


.. index:: -gnatwc  (gcc)

:switch:`-gnatwc`
  *Activate warnings on conditionals.*

  .. index:: Conditionals, constant

  This switch activates warnings for conditional expressions used in
  tests that are known to be True or False at compile time. The default
  is that such warnings are not generated.
  Note that this warning does
  not get issued for the use of boolean variables or constants whose
  values are known at compile time, since this is a standard technique
  for conditional compilation in Ada, and this would generate too many
  false positive warnings.

  This warning option also activates a special test for comparisons using
  the operators '>=' and' <='.
  If the compiler can tell that only the equality condition is possible,
  then it will warn that the '>' or '<' part of the test
  is useless and that the operator could be replaced by '='.
  An example would be comparing a ``Natural`` variable <= 0.

  This warning option also generates warnings if
  one or both tests is optimized away in a membership test for integer
  values if the result can be determined at compile time. Range tests on
  enumeration types are not included, since it is common for such tests
  to include an end point.

  This warning can also be turned on using :switch:`-gnatwa`.


.. index:: -gnatwC  (gcc)

:switch:`-gnatwC`
  *Suppress warnings on conditionals.*

  This switch suppresses warnings for conditional expressions used in
  tests that are known to be True or False at compile time.


.. index:: -gnatw.c  (gcc)

:switch:`-gnatw.c`
  *Activate warnings on missing component clauses.*

  .. index:: Component clause, missing

  This switch activates warnings for record components where a record
  representation clause is present and has component clauses for the
  majority, but not all, of the components. A warning is given for each
  component for which no component clause is present.


.. index:: -gnatw.C  (gcc)

:switch:`-gnatw.C`
  *Suppress warnings on missing component clauses.*

  This switch suppresses warnings for record components that are
  missing a component clause in the situation described above.


.. index:: -gnatw_c  (gcc)

:switch:`-gnatw_c`
  *Activate warnings on unknown condition in Compile_Time_Warning.*

  .. index:: Compile_Time_Warning
  .. index:: Compile_Time_Error

  This switch activates warnings on a pragma Compile_Time_Warning
  or Compile_Time_Error whose condition has a value that is not
  known at compile time.
  The default is that such warnings are generated.


.. index:: -gnatw_C  (gcc)

:switch:`-gnatw_C`
  *Suppress warnings on unknown condition in Compile_Time_Warning.*

  This switch supresses warnings on a pragma Compile_Time_Warning
  or Compile_Time_Error whose condition has a value that is not
  known at compile time.


.. index:: -gnatwd  (gcc)

:switch:`-gnatwd`
  *Activate warnings on implicit dereferencing.*

  If this switch is set, then the use of a prefix of an access type
  in an indexed component, slice, or selected component without an
  explicit ``.all`` will generate a warning. With this warning
  enabled, access checks occur only at points where an explicit
  ``.all`` appears in the source code (assuming no warnings are
  generated as a result of this switch). The default is that such
  warnings are not generated.


.. index:: -gnatwD  (gcc)

:switch:`-gnatwD`
  *Suppress warnings on implicit dereferencing.*

  .. index:: Implicit dereferencing

  .. index:: Dereferencing, implicit

  This switch suppresses warnings for implicit dereferences in
  indexed components, slices, and selected components.


.. index:: -gnatw.d  (gcc)

:switch:`-gnatw.d`
  *Activate tagging of warning and info messages.*

  If this switch is set, then warning messages are tagged, with one of the
  following strings:

    - *[-gnatw?]*
      Used to tag warnings controlled by the switch :switch:`-gnatwx` where x
      is a letter a-z.


    - *[-gnatw.?]*
      Used to tag warnings controlled by the switch :switch:`-gnatw.x` where x
      is a letter a-z.


    - *[-gnatel]*
      Used to tag elaboration information (info) messages generated when the
      static model of elaboration is used and the :switch:`-gnatel` switch is set.


    - *[restriction warning]*
      Used to tag warning messages for restriction violations, activated by use
      of the pragma ``Restriction_Warnings``.


    - *[warning-as-error]*
      Used to tag warning messages that have been converted to error messages by
      use of the pragma Warning_As_Error. Note that such warnings are prefixed by
      the string "error: " rather than "warning: ".


    - *[enabled by default]*
      Used to tag all other warnings that are always given by default, unless
      warnings are completely suppressed using pragma *Warnings(Off)* or
      the switch :switch:`-gnatws`.



.. index:: -gnatw.d  (gcc)

:switch:`-gnatw.D`
  *Deactivate tagging of warning and info messages messages.*

  If this switch is set, then warning messages return to the default
  mode in which warnings and info messages are not tagged as described above for
  :switch:`-gnatw.d`.


.. index:: -gnatwe  (gcc)
.. index:: Warnings, treat as error

:switch:`-gnatwe`
  *Treat warnings and style checks as errors.*

  This switch causes warning messages and style check messages to be
  treated as errors.
  The warning string still appears, but the warning messages are counted
  as errors, and prevent the generation of an object file. Note that this
  is the only -gnatw switch that affects the handling of style check messages.
  Note also that this switch has no effect on info (information) messages, which
  are not treated as errors if this switch is present.


.. index:: -gnatw.e  (gcc)

:switch:`-gnatw.e`
  *Activate every optional warning.*

  .. index:: Warnings, activate every optional warning

  This switch activates all optional warnings, including those which
  are not activated by :switch:`-gnatwa`. The use of this switch is not
  recommended for normal use. If you turn this switch on, it is almost
  certain that you will get large numbers of useless warnings. The
  warnings that are excluded from :switch:`-gnatwa` are typically highly
  specialized warnings that are suitable for use only in code that has
  been specifically designed according to specialized coding rules.


.. index:: -gnatwE  (gcc)
.. index:: Warnings, treat as error

:switch:`-gnatwE`
  *Treat all run-time exception warnings as errors.*

  This switch causes warning messages regarding errors that will be raised
  during run-time execution to be treated as errors.


.. index:: -gnatwf  (gcc)

:switch:`-gnatwf`
  *Activate warnings on unreferenced formals.*

  .. index:: Formals, unreferenced

  This switch causes a warning to be generated if a formal parameter
  is not referenced in the body of the subprogram. This warning can
  also be turned on using :switch:`-gnatwu`. The
  default is that these warnings are not generated.


.. index:: -gnatwF  (gcc)

:switch:`-gnatwF`
  *Suppress warnings on unreferenced formals.*

  This switch suppresses warnings for unreferenced formal
  parameters. Note that the
  combination :switch:`-gnatwu` followed by :switch:`-gnatwF` has the
  effect of warning on unreferenced entities other than subprogram
  formals.


.. index:: -gnatwg  (gcc)

:switch:`-gnatwg`
  *Activate warnings on unrecognized pragmas.*

  .. index:: Pragmas, unrecognized

  This switch causes a warning to be generated if an unrecognized
  pragma is encountered. Apart from issuing this warning, the
  pragma is ignored and has no effect. The default
  is that such warnings are issued (satisfying the Ada Reference
  Manual requirement that such warnings appear).


.. index:: -gnatwG  (gcc)

:switch:`-gnatwG`
  *Suppress warnings on unrecognized pragmas.*

  This switch suppresses warnings for unrecognized pragmas.


.. index:: -gnatw.g  (gcc)

:switch:`-gnatw.g`
  *Warnings used for GNAT sources.*

  This switch sets the warning categories that are used by the standard
  GNAT style. Currently this is equivalent to
  :switch:`-gnatwAao.q.s.CI.V.X.Z`
  but more warnings may be added in the future without advanced notice.


.. index:: -gnatwh  (gcc)

:switch:`-gnatwh`
  *Activate warnings on hiding.*

  .. index:: Hiding of Declarations

  This switch activates warnings on hiding declarations that are considered
  potentially confusing. Not all cases of hiding cause warnings; for example an
  overriding declaration hides an implicit declaration, which is just normal
  code. The default is that warnings on hiding are not generated.


.. index:: -gnatwH  (gcc)

:switch:`-gnatwH`
  *Suppress warnings on hiding.*

  This switch suppresses warnings on hiding declarations.


.. index:: -gnatw.h  (gcc)

:switch:`-gnatw.h`
  *Activate warnings on holes/gaps in records.*

  .. index:: Record Representation (gaps)

  This switch activates warnings on component clauses in record
  representation clauses that leave holes (gaps) in the record layout.
  If this warning option is active, then record representation clauses
  should specify a contiguous layout, adding unused fill fields if needed.


.. index:: -gnatw.H  (gcc)

:switch:`-gnatw.H`
  *Suppress warnings on holes/gaps in records.*

  This switch suppresses warnings on component clauses in record
  representation clauses that leave holes (haps) in the record layout.


.. index:: -gnatwi  (gcc)

:switch:`-gnatwi`
  *Activate warnings on implementation units.*

  This switch activates warnings for a |with| of an internal GNAT
  implementation unit, defined as any unit from the ``Ada``,
  ``Interfaces``, ``GNAT``,
  or ``System``
  hierarchies that is not
  documented in either the Ada Reference Manual or the GNAT
  Programmer's Reference Manual. Such units are intended only
  for internal implementation purposes and should not be |withed|
  by user programs. The default is that such warnings are generated


.. index:: -gnatwI  (gcc)

:switch:`-gnatwI`
  *Disable warnings on implementation units.*

  This switch disables warnings for a |with| of an internal GNAT
  implementation unit.


.. index:: -gnatw.i  (gcc)

:switch:`-gnatw.i`
  *Activate warnings on overlapping actuals.*

  This switch enables a warning on statically detectable overlapping actuals in
  a subprogram call, when one of the actuals is an in-out parameter, and the
  types of the actuals are not by-copy types. This warning is off by default.


.. index:: -gnatw.I  (gcc)

:switch:`-gnatw.I`
  *Disable warnings on overlapping actuals.*

  This switch disables warnings on overlapping actuals in a call..


.. index:: -gnatwj  (gcc)

:switch:`-gnatwj`
  *Activate warnings on obsolescent features (Annex J).*

  .. index:: Features, obsolescent

  .. index:: Obsolescent features

  If this warning option is activated, then warnings are generated for
  calls to subprograms marked with ``pragma Obsolescent`` and
  for use of features in Annex J of the Ada Reference Manual. In the
  case of Annex J, not all features are flagged. In particular use
  of the renamed packages (like ``Text_IO``) and use of package
  ``ASCII`` are not flagged, since these are very common and
  would generate many annoying positive warnings. The default is that
  such warnings are not generated.

  In addition to the above cases, warnings are also generated for
  GNAT features that have been provided in past versions but which
  have been superseded (typically by features in the new Ada standard).
  For example, ``pragma Ravenscar`` will be flagged since its
  function is replaced by ``pragma Profile(Ravenscar)``, and
  ``pragma Interface_Name`` will be flagged since its function
  is replaced by ``pragma Import``.

  Note that this warning option functions differently from the
  restriction ``No_Obsolescent_Features`` in two respects.
  First, the restriction applies only to annex J features.
  Second, the restriction does flag uses of package ``ASCII``.


.. index:: -gnatwJ  (gcc)

:switch:`-gnatwJ`
  *Suppress warnings on obsolescent features (Annex J).*

  This switch disables warnings on use of obsolescent features.


.. index:: -gnatw.j  (gcc)

:switch:`-gnatw.j`
  *Activate warnings on late declarations of tagged type primitives.*

  This switch activates warnings on visible primitives added to a
  tagged type after deriving a private extension from it.


.. index:: -gnatw.J  (gcc)

:switch:`-gnatw.J`
  *Suppress warnings on late declarations of tagged type primitives.*

  This switch suppresses warnings on visible primitives added to a
  tagged type after deriving a private extension from it.


.. index:: -gnatwk  (gcc)

:switch:`-gnatwk`
  *Activate warnings on variables that could be constants.*

  This switch activates warnings for variables that are initialized but
  never modified, and then could be declared constants. The default is that
  such warnings are not given.


.. index:: -gnatwK  (gcc)

:switch:`-gnatwK`
  *Suppress warnings on variables that could be constants.*

  This switch disables warnings on variables that could be declared constants.


.. index:: -gnatw.k  (gcc)

:switch:`-gnatw.k`
  *Activate warnings on redefinition of names in standard.*

  This switch activates warnings for declarations that declare a name that
  is defined in package Standard. Such declarations can be confusing,
  especially since the names in package Standard continue to be directly
  visible, meaning that use visibiliy on such redeclared names does not
  work as expected. Names of discriminants and components in records are
  not included in this check.


.. index:: -gnatwK  (gcc)

:switch:`-gnatw.K`
  *Suppress warnings on redefinition of names in standard.*

  This switch activates warnings for declarations that declare a name that
  is defined in package Standard.


.. index:: -gnatwl  (gcc)

:switch:`-gnatwl`
  *Activate warnings for elaboration pragmas.*

  .. index:: Elaboration, warnings

  This switch activates warnings for possible elaboration problems,
  including suspicious use
  of ``Elaborate`` pragmas, when using the static elaboration model, and
  possible situations that may raise ``Program_Error`` when using the
  dynamic elaboration model.
  See the section in this guide on elaboration checking for further details.
  The default is that such warnings
  are not generated.


.. index:: -gnatwL  (gcc)

:switch:`-gnatwL`
  *Suppress warnings for elaboration pragmas.*

  This switch suppresses warnings for possible elaboration problems.


.. index:: -gnatw.l  (gcc)

:switch:`-gnatw.l`
  *List inherited aspects.*

  This switch causes the compiler to list inherited invariants,
  preconditions, and postconditions from Type_Invariant'Class, Invariant'Class,
  Pre'Class, and Post'Class aspects. Also list inherited subtype predicates.


.. index:: -gnatw.L  (gcc)

:switch:`-gnatw.L`
  *Suppress listing of inherited aspects.*

  This switch suppresses listing of inherited aspects.


.. index:: -gnatwm  (gcc)

:switch:`-gnatwm`
  *Activate warnings on modified but unreferenced variables.*

  This switch activates warnings for variables that are assigned (using
  an initialization value or with one or more assignment statements) but
  whose value is never read. The warning is suppressed for volatile
  variables and also for variables that are renamings of other variables
  or for which an address clause is given.
  The default is that these warnings are not given.


.. index:: -gnatwM  (gcc)

:switch:`-gnatwM`
  *Disable warnings on modified but unreferenced variables.*

  This switch disables warnings for variables that are assigned or
  initialized, but never read.


.. index:: -gnatw.m  (gcc)

:switch:`-gnatw.m`
  *Activate warnings on suspicious modulus values.*

  This switch activates warnings for modulus values that seem suspicious.
  The cases caught are where the size is the same as the modulus (e.g.
  a modulus of 7 with a size of 7 bits), and modulus values of 32 or 64
  with no size clause. The guess in both cases is that 2**x was intended
  rather than x. In addition expressions of the form 2*x for small x
  generate a warning (the almost certainly accurate guess being that
  2**x was intended). The default is that these warnings are given.


.. index:: -gnatw.M  (gcc)

:switch:`-gnatw.M`
  *Disable warnings on suspicious modulus values.*

  This switch disables warnings for suspicious modulus values.


.. index:: -gnatwn  (gcc)

:switch:`-gnatwn`
  *Set normal warnings mode.*

  This switch sets normal warning mode, in which enabled warnings are
  issued and treated as warnings rather than errors. This is the default
  mode. the switch :switch:`-gnatwn` can be used to cancel the effect of
  an explicit :switch:`-gnatws` or
  :switch:`-gnatwe`. It also cancels the effect of the
  implicit :switch:`-gnatwe` that is activated by the
  use of :switch:`-gnatg`.


.. index:: -gnatw.n  (gcc)
.. index:: Atomic Synchronization, warnings

:switch:`-gnatw.n`
  *Activate warnings on atomic synchronization.*

  This switch actives warnings when an access to an atomic variable
  requires the generation of atomic synchronization code. These
  warnings are off by default.

.. index:: -gnatw.N  (gcc)

:switch:`-gnatw.N`
  *Suppress warnings on atomic synchronization.*

  .. index:: Atomic Synchronization, warnings

  This switch suppresses warnings when an access to an atomic variable
  requires the generation of atomic synchronization code.


.. index:: -gnatwo  (gcc)
.. index:: Address Clauses, warnings

:switch:`-gnatwo`
  *Activate warnings on address clause overlays.*

  This switch activates warnings for possibly unintended initialization
  effects of defining address clauses that cause one variable to overlap
  another. The default is that such warnings are generated.


.. index:: -gnatwO  (gcc)

:switch:`-gnatwO`
  *Suppress warnings on address clause overlays.*

  This switch suppresses warnings on possibly unintended initialization
  effects of defining address clauses that cause one variable to overlap
  another.


.. index:: -gnatw.o  (gcc)

:switch:`-gnatw.o`
  *Activate warnings on modified but unreferenced out parameters.*

  This switch activates warnings for variables that are modified by using
  them as actuals for a call to a procedure with an out mode formal, where
  the resulting assigned value is never read. It is applicable in the case
  where there is more than one out mode formal. If there is only one out
  mode formal, the warning is issued by default (controlled by -gnatwu).
  The warning is suppressed for volatile
  variables and also for variables that are renamings of other variables
  or for which an address clause is given.
  The default is that these warnings are not given.


.. index:: -gnatw.O  (gcc)

:switch:`-gnatw.O`
  *Disable warnings on modified but unreferenced out parameters.*

  This switch suppresses warnings for variables that are modified by using
  them as actuals for a call to a procedure with an out mode formal, where
  the resulting assigned value is never read.


.. index:: -gnatwp  (gcc)
.. index:: Inlining, warnings

:switch:`-gnatwp`
  *Activate warnings on ineffective pragma Inlines.*

  This switch activates warnings for failure of front end inlining
  (activated by :switch:`-gnatN`) to inline a particular call. There are
  many reasons for not being able to inline a call, including most
  commonly that the call is too complex to inline. The default is
  that such warnings are not given.
  Warnings on ineffective inlining by the gcc back-end can be activated
  separately, using the gcc switch -Winline.


.. index:: -gnatwP  (gcc)

:switch:`-gnatwP`
  *Suppress warnings on ineffective pragma Inlines.*

  This switch suppresses warnings on ineffective pragma Inlines. If the
  inlining mechanism cannot inline a call, it will simply ignore the
  request silently.


.. index:: -gnatw.p  (gcc)
.. index:: Parameter order, warnings

:switch:`-gnatw.p`
  *Activate warnings on parameter ordering.*

  This switch activates warnings for cases of suspicious parameter
  ordering when the list of arguments are all simple identifiers that
  match the names of the formals, but are in a different order. The
  warning is suppressed if any use of named parameter notation is used,
  so this is the appropriate way to suppress a false positive (and
  serves to emphasize that the "misordering" is deliberate). The
  default is that such warnings are not given.


.. index:: -gnatw.P  (gcc)

:switch:`-gnatw.P`
  *Suppress warnings on parameter ordering.*

  This switch suppresses warnings on cases of suspicious parameter
  ordering.


.. index:: -gnatwq  (gcc)
.. index:: Parentheses, warnings

:switch:`-gnatwq`
  *Activate warnings on questionable missing parentheses.*

  This switch activates warnings for cases where parentheses are not used and
  the result is potential ambiguity from a readers point of view. For example
  (not a > b) when a and b are modular means ((not a) > b) and very likely the
  programmer intended (not (a > b)). Similarly (-x mod 5) means (-(x mod 5)) and
  quite likely ((-x) mod 5) was intended. In such situations it seems best to
  follow the rule of always parenthesizing to make the association clear, and
  this warning switch warns if such parentheses are not present. The default
  is that these warnings are given.


.. index:: -gnatwQ  (gcc)

:switch:`-gnatwQ`
  *Suppress warnings on questionable missing parentheses.*

  This switch suppresses warnings for cases where the association is not
  clear and the use of parentheses is preferred.


.. index:: -gnatw.q  (gcc)
.. index:: Layout, warnings

:switch:`-gnatw.q`
  *Activate warnings on questionable layout of record types.*

  This switch activates warnings for cases where the default layout of
  a record type, that is to say the layout of its components in textual
  order of the source code, would very likely cause inefficiencies in
  the code generated by the compiler, both in terms of space and speed
  during execution. One warning is issued for each problematic component
  without representation clause in the nonvariant part and then in each
  variant recursively, if any.

  The purpose of these warnings is neither to prescribe an optimal layout
  nor to force the use of representation clauses, but rather to get rid of
  the most blatant inefficiencies in the layout. Therefore, the default
  layout is matched against the following synthetic ordered layout and
  the deviations are flagged on a component-by-component basis:

  * first all components or groups of components whose length is fixed
    and a multiple of the storage unit,

  * then the remaining components whose length is fixed and not a multiple
    of the storage unit,

  * then the remaining components whose length doesn't depend on discriminants
    (that is to say, with variable but uniform length for all objects),

  * then all components whose length depends on discriminants,

  * finally the variant part (if any),

  for the nonvariant part and for each variant recursively, if any.

  The exact wording of the warning depends on whether the compiler is allowed
  to reorder the components in the record type or precluded from doing it by
  means of pragma ``No_Component_Reordering``.

  The default is that these warnings are not given.

.. index:: -gnatw.Q  (gcc)

:switch:`-gnatw.Q`
  *Suppress warnings on questionable layout of record types.*

  This switch suppresses warnings for cases where the default layout of
  a record type would very likely cause inefficiencies.


.. index:: -gnatwr  (gcc)

:switch:`-gnatwr`
  *Activate warnings on redundant constructs.*

  This switch activates warnings for redundant constructs. The following
  is the current list of constructs regarded as redundant:

  * Assignment of an item to itself.

  * Type conversion that converts an expression to its own type.

  * Use of the attribute ``Base`` where ``typ'Base`` is the same
    as ``typ``.

  * Use of pragma ``Pack`` when all components are placed by a record
    representation clause.

  * Exception handler containing only a reraise statement (raise with no
    operand) which has no effect.

  * Use of the operator abs on an operand that is known at compile time
    to be non-negative

  * Comparison of an object or (unary or binary) operation of boolean type to
    an explicit True value.

  The default is that warnings for redundant constructs are not given.


.. index:: -gnatwR  (gcc)

:switch:`-gnatwR`
  *Suppress warnings on redundant constructs.*

  This switch suppresses warnings for redundant constructs.


.. index:: -gnatw.r  (gcc)

:switch:`-gnatw.r`
  *Activate warnings for object renaming function.*

  This switch activates warnings for an object renaming that renames a
  function call, which is equivalent to a constant declaration (as
  opposed to renaming the function itself).  The default is that these
  warnings are given.


.. index:: -gnatw.R  (gcc)

:switch:`-gnatw.R`
  *Suppress warnings for object renaming function.*

  This switch suppresses warnings for object renaming function.


.. index:: -gnatw_r  (gcc)

:switch:`-gnatw_r`
  *Activate warnings for out-of-order record representation clauses.*

  This switch activates warnings for record representation clauses,
  if the order of component declarations, component clauses,
  and bit-level layout do not all agree.
  The default is that these warnings are not given.


.. index:: -gnatw_R  (gcc)

:switch:`-gnatw_R`
  *Suppress warnings for out-of-order record representation clauses.*


.. index:: -gnatws  (gcc)

:switch:`-gnatws`
  *Suppress all warnings.*

  This switch completely suppresses the
  output of all warning messages from the GNAT front end, including
  both warnings that can be controlled by switches described in this
  section, and those that are normally given unconditionally. The
  effect of this suppress action can only be cancelled by a subsequent
  use of the switch :switch:`-gnatwn`.

  Note that switch :switch:`-gnatws` does not suppress
  warnings from the ``gcc`` back end.
  To suppress these back end warnings as well, use the switch :switch:`-w`
  in addition to :switch:`-gnatws`. Also this switch has no effect on the
  handling of style check messages.


.. index:: -gnatw.s  (gcc)
.. index:: Record Representation (component sizes)

:switch:`-gnatw.s`
  *Activate warnings on overridden size clauses.*

  This switch activates warnings on component clauses in record
  representation clauses where the length given overrides that
  specified by an explicit size clause for the component type. A
  warning is similarly given in the array case if a specified
  component size overrides an explicit size clause for the array
  component type.


.. index:: -gnatw.S  (gcc)

:switch:`-gnatw.S`
  *Suppress warnings on overridden size clauses.*

  This switch suppresses warnings on component clauses in record
  representation clauses that override size clauses, and similar
  warnings when an array component size overrides a size clause.


.. index:: -gnatwt  (gcc)
.. index:: Deactivated code, warnings
.. index:: Deleted code, warnings

:switch:`-gnatwt`
  *Activate warnings for tracking of deleted conditional code.*

  This switch activates warnings for tracking of code in conditionals (IF and
  CASE statements) that is detected to be dead code which cannot be executed, and
  which is removed by the front end. This warning is off by default. This may be
  useful for detecting deactivated code in certified applications.


.. index:: -gnatwT  (gcc)

:switch:`-gnatwT`
  *Suppress warnings for tracking of deleted conditional code.*

  This switch suppresses warnings for tracking of deleted conditional code.


.. index:: -gnatw.t  (gcc)

:switch:`-gnatw.t`
  *Activate warnings on suspicious contracts.*

  This switch activates warnings on suspicious contracts. This includes
  warnings on suspicious postconditions (whether a pragma ``Postcondition`` or a
  ``Post`` aspect in Ada 2012) and suspicious contract cases (pragma or aspect
  ``Contract_Cases``). A function postcondition or contract case is suspicious
  when no postcondition or contract case for this function mentions the result
  of the function.  A procedure postcondition or contract case is suspicious
  when it only refers to the pre-state of the procedure, because in that case
  it should rather be expressed as a precondition. This switch also controls
  warnings on suspicious cases of expressions typically found in contracts like
  quantified expressions and uses of Update attribute. The default is that such
  warnings are generated.


.. index:: -gnatw.T  (gcc)

:switch:`-gnatw.T`
  *Suppress warnings on suspicious contracts.*

  This switch suppresses warnings on suspicious contracts.


.. index:: -gnatwu  (gcc)

:switch:`-gnatwu`
  *Activate warnings on unused entities.*

  This switch activates warnings to be generated for entities that
  are declared but not referenced, and for units that are |withed|
  and not
  referenced. In the case of packages, a warning is also generated if
  no entities in the package are referenced. This means that if a with'ed
  package is referenced but the only references are in ``use``
  clauses or ``renames``
  declarations, a warning is still generated. A warning is also generated
  for a generic package that is |withed| but never instantiated.
  In the case where a package or subprogram body is compiled, and there
  is a |with| on the corresponding spec
  that is only referenced in the body,
  a warning is also generated, noting that the
  |with| can be moved to the body. The default is that
  such warnings are not generated.
  This switch also activates warnings on unreferenced formals
  (it includes the effect of :switch:`-gnatwf`).


.. index:: -gnatwU  (gcc)

:switch:`-gnatwU`
  *Suppress warnings on unused entities.*

  This switch suppresses warnings for unused entities and packages.
  It also turns off warnings on unreferenced formals (and thus includes
  the effect of :switch:`-gnatwF`).


.. index:: -gnatw.u  (gcc)

:switch:`-gnatw.u`
  *Activate warnings on unordered enumeration types.*

  This switch causes enumeration types to be considered as conceptually
  unordered, unless an explicit pragma ``Ordered`` is given for the type.
  The effect is to generate warnings in clients that use explicit comparisons
  or subranges, since these constructs both treat objects of the type as
  ordered. (A *client* is defined as a unit that is other than the unit in
  which the type is declared, or its body or subunits.) Please refer to
  the description of pragma ``Ordered`` in the
  :title:`GNAT Reference Manual` for further details.
  The default is that such warnings are not generated.


.. index:: -gnatw.U  (gcc)

:switch:`-gnatw.U`
  *Deactivate warnings on unordered enumeration types.*

  This switch causes all enumeration types to be considered as ordered, so
  that no warnings are given for comparisons or subranges for any type.


.. index:: -gnatwv  (gcc)
.. index:: Unassigned variable warnings

:switch:`-gnatwv`
  *Activate warnings on unassigned variables.*

  This switch activates warnings for access to variables which
  may not be properly initialized. The default is that
  such warnings are generated.


.. index:: -gnatwV  (gcc)

:switch:`-gnatwV`
  *Suppress warnings on unassigned variables.*

  This switch suppresses warnings for access to variables which
  may not be properly initialized.
  For variables of a composite type, the warning can also be suppressed in
  Ada 2005 by using a default initialization with a box. For example, if
  Table is an array of records whose components are only partially uninitialized,
  then the following code:

  .. code-block:: ada

       Tab : Table := (others => <>);

  will suppress warnings on subsequent statements that access components
  of variable Tab.


.. index:: -gnatw.v  (gcc)
.. index:: bit order warnings

:switch:`-gnatw.v`
  *Activate info messages for non-default bit order.*

  This switch activates messages (labeled "info", they are not warnings,
  just informational messages) about the effects of non-default bit-order
  on records to which a component clause is applied. The effect of specifying
  non-default bit ordering is a bit subtle (and changed with Ada 2005), so
  these messages, which are given by default, are useful in understanding the
  exact consequences of using this feature.


.. index:: -gnatw.V  (gcc)

:switch:`-gnatw.V`
  *Suppress info messages for non-default bit order.*

  This switch suppresses information messages for the effects of specifying
  non-default bit order on record components with component clauses.


.. index:: -gnatww  (gcc)
.. index:: String indexing warnings

:switch:`-gnatww`
  *Activate warnings on wrong low bound assumption.*

  This switch activates warnings for indexing an unconstrained string parameter
  with a literal or S'Length. This is a case where the code is assuming that the
  low bound is one, which is in general not true (for example when a slice is
  passed). The default is that such warnings are generated.


.. index:: -gnatwW  (gcc)

:switch:`-gnatwW`
  *Suppress warnings on wrong low bound assumption.*

  This switch suppresses warnings for indexing an unconstrained string parameter
  with a literal or S'Length. Note that this warning can also be suppressed
  in a particular case by adding an assertion that the lower bound is 1,
  as shown in the following example:

  .. code-block:: ada

       procedure K (S : String) is
          pragma Assert (S'First = 1);
          ...


.. index:: -gnatw.w  (gcc)
.. index:: Warnings Off control

:switch:`-gnatw.w`
  *Activate warnings on Warnings Off pragmas.*

  This switch activates warnings for use of ``pragma Warnings (Off, entity)``
  where either the pragma is entirely useless (because it suppresses no
  warnings), or it could be replaced by ``pragma Unreferenced`` or
  ``pragma Unmodified``.
  Also activates warnings for the case of
  Warnings (Off, String), where either there is no matching
  Warnings (On, String), or the Warnings (Off) did not suppress any warning.
  The default is that these warnings are not given.


.. index:: -gnatw.W  (gcc)

:switch:`-gnatw.W`
  *Suppress warnings on unnecessary Warnings Off pragmas.*

  This switch suppresses warnings for use of ``pragma Warnings (Off, ...)``.


.. index:: -gnatwx  (gcc)
.. index:: Export/Import pragma warnings

:switch:`-gnatwx`
  *Activate warnings on Export/Import pragmas.*

  This switch activates warnings on Export/Import pragmas when
  the compiler detects a possible conflict between the Ada and
  foreign language calling sequences. For example, the use of
  default parameters in a convention C procedure is dubious
  because the C compiler cannot supply the proper default, so
  a warning is issued. The default is that such warnings are
  generated.


.. index:: -gnatwX  (gcc)

:switch:`-gnatwX`
  *Suppress warnings on Export/Import pragmas.*

  This switch suppresses warnings on Export/Import pragmas.
  The sense of this is that you are telling the compiler that
  you know what you are doing in writing the pragma, and it
  should not complain at you.


.. index:: -gnatwm  (gcc)

:switch:`-gnatw.x`
  *Activate warnings for No_Exception_Propagation mode.*

  This switch activates warnings for exception usage when pragma Restrictions
  (No_Exception_Propagation) is in effect. Warnings are given for implicit or
  explicit exception raises which are not covered by a local handler, and for
  exception handlers which do not cover a local raise. The default is that
  these warnings are given for units that contain exception handlers.


:switch:`-gnatw.X`
  *Disable warnings for No_Exception_Propagation mode.*

  This switch disables warnings for exception usage when pragma Restrictions
  (No_Exception_Propagation) is in effect.


.. index:: -gnatwy  (gcc)
.. index:: Ada compatibility issues warnings

:switch:`-gnatwy`
  *Activate warnings for Ada compatibility issues.*

  For the most part, newer versions of Ada are upwards compatible
  with older versions. For example, Ada 2005 programs will almost
  always work when compiled as Ada 2012.
  However there are some exceptions (for example the fact that
  ``some`` is now a reserved word in Ada 2012). This
  switch activates several warnings to help in identifying
  and correcting such incompatibilities. The default is that
  these warnings are generated. Note that at one point Ada 2005
  was called Ada 0Y, hence the choice of character.


.. index:: -gnatwY  (gcc)
.. index:: Ada compatibility issues warnings

:switch:`-gnatwY`
  *Disable warnings for Ada compatibility issues.*

  This switch suppresses the warnings intended to help in identifying
  incompatibilities between Ada language versions.


.. index:: -gnatw.y  (gcc)
.. index:: Package spec needing body

:switch:`-gnatw.y`
  *Activate information messages for why package spec needs body.*

  There are a number of cases in which a package spec needs a body.
  For example, the use of pragma Elaborate_Body, or the declaration
  of a procedure specification requiring a completion. This switch
  causes information messages to be output showing why a package
  specification requires a body. This can be useful in the case of
  a large package specification which is unexpectedly requiring a
  body. The default is that such information messages are not output.


.. index:: -gnatw.Y  (gcc)
.. index:: No information messages for why package spec needs body

:switch:`-gnatw.Y`
  *Disable information messages for why package spec needs body.*

  This switch suppresses the output of information messages showing why
  a package specification needs a body.


.. index:: -gnatwz  (gcc)
.. index:: Unchecked_Conversion warnings

:switch:`-gnatwz`
  *Activate warnings on unchecked conversions.*

  This switch activates warnings for unchecked conversions
  where the types are known at compile time to have different
  sizes. The default is that such warnings are generated. Warnings are also
  generated for subprogram pointers with different conventions.


.. index:: -gnatwZ  (gcc)

:switch:`-gnatwZ`
  *Suppress warnings on unchecked conversions.*

  This switch suppresses warnings for unchecked conversions
  where the types are known at compile time to have different
  sizes or conventions.


.. index:: -gnatw.z  (gcc)
.. index:: Size/Alignment warnings

:switch:`-gnatw.z`
  *Activate warnings for size not a multiple of alignment.*

  This switch activates warnings for cases of array and record types
  with specified ``Size`` and ``Alignment`` attributes where the
  size is not a multiple of the alignment, resulting in an object
  size that is greater than the specified size. The default
  is that such warnings are generated.


.. index:: -gnatw.Z  (gcc)
.. index:: Size/Alignment warnings

:switch:`-gnatw.Z`
  *Suppress warnings for size not a multiple of alignment.*

  This switch suppresses warnings for cases of array and record types
  with specified ``Size`` and ``Alignment`` attributes where the
  size is not a multiple of the alignment, resulting in an object
  size that is greater than the specified size. The warning can also
  be suppressed by giving an explicit ``Object_Size`` value.


.. index:: -Wunused (gcc)

:switch:`-Wunused`
  The warnings controlled by the :switch:`-gnatw` switch are generated by
  the front end of the compiler. The GCC back end can provide
  additional warnings and they are controlled by the :switch:`-W` switch.
  For example, :switch:`-Wunused` activates back end
  warnings for entities that are declared but not referenced.


.. index:: -Wuninitialized (gcc)

:switch:`-Wuninitialized`
  Similarly, :switch:`-Wuninitialized` activates
  the back end warning for uninitialized variables. This switch must be
  used in conjunction with an optimization level greater than zero.


.. index:: -Wstack-usage (gcc)

:switch:`-Wstack-usage={len}`
  Warn if the stack usage of a subprogram might be larger than ``len`` bytes.
  See :ref:`Static_Stack_Usage_Analysis` for details.


.. index:: -Wall (gcc)

:switch:`-Wall`
  This switch enables most warnings from the GCC back end.
  The code generator detects a number of warning situations that are missed
  by the GNAT front end, and this switch can be used to activate them.
  The use of this switch also sets the default front end warning mode to
  :switch:`-gnatwa`, that is, most front end warnings activated as well.


.. index:: -w (gcc)

:switch:`-w`
  Conversely, this switch suppresses warnings from the GCC back end.
  The use of this switch also sets the default front end warning mode to
  :switch:`-gnatws`, that is, front end warnings suppressed as well.


.. index:: -Werror (gcc)

:switch:`-Werror`
  This switch causes warnings from the GCC back end to be treated as
  errors.  The warning string still appears, but the warning messages are
  counted as errors, and prevent the generation of an object file.


A string of warning parameters can be used in the same parameter. For example::

  -gnatwaGe


will turn on all optional warnings except for unrecognized pragma warnings,
and also specify that warnings should be treated as errors.

When no switch :switch:`-gnatw` is used, this is equivalent to:

  * :switch:`-gnatw.a`

  * :switch:`-gnatwB`

  * :switch:`-gnatw.b`

  * :switch:`-gnatwC`

  * :switch:`-gnatw.C`

  * :switch:`-gnatwD`

  * :switch:`-gnatw.D`

  * :switch:`-gnatwF`

  * :switch:`-gnatw.F`

  * :switch:`-gnatwg`

  * :switch:`-gnatwH`

  * :switch:`-gnatw.H`

  * :switch:`-gnatwi`

  * :switch:`-gnatwJ`

  * :switch:`-gnatw.J`

  * :switch:`-gnatwK`

  * :switch:`-gnatw.K`

  * :switch:`-gnatwL`

  * :switch:`-gnatw.L`

  * :switch:`-gnatwM`

  * :switch:`-gnatw.m`

  * :switch:`-gnatwn`

  * :switch:`-gnatw.N`

  * :switch:`-gnatwo`

  * :switch:`-gnatw.O`

  * :switch:`-gnatwP`

  * :switch:`-gnatw.P`

  * :switch:`-gnatwq`

  * :switch:`-gnatw.Q`

  * :switch:`-gnatwR`

  * :switch:`-gnatw.R`

  * :switch:`-gnatw.S`

  * :switch:`-gnatwT`

  * :switch:`-gnatw.t`

  * :switch:`-gnatwU`

  * :switch:`-gnatw.U`

  * :switch:`-gnatwv`

  * :switch:`-gnatw.v`

  * :switch:`-gnatww`

  * :switch:`-gnatw.W`

  * :switch:`-gnatwx`

  * :switch:`-gnatw.X`

  * :switch:`-gnatwy`

  * :switch:`-gnatw.Y`

  * :switch:`-gnatwz`

  * :switch:`-gnatw.z`

.. _Debugging_and_Assertion_Control:

Debugging and Assertion Control
-------------------------------



.. index:: -gnata  (gcc)

:switch:`-gnata`
  .. index:: Assert
  .. index:: Debug
  .. index:: Assertions
  .. index:: Precondition
  .. index:: Postcondition
  .. index:: Type invariants
  .. index:: Subtype predicates

  The :switch:`-gnata` option is equivalent to the following ``Assertion_Policy`` pragma::

       pragma Assertion_Policy (Check);

  Which is a shorthand for::

       pragma Assertion_Policy
         (Assert               => Check,
          Static_Predicate     => Check,
          Dynamic_Predicate    => Check,
          Pre                  => Check,
          Pre'Class            => Check,
          Post                 => Check,
          Post'Class           => Check,
          Type_Invariant       => Check,
          Type_Invariant'Class => Check);

  The pragmas ``Assert`` and ``Debug`` normally have no effect and
  are ignored. This switch, where ``a`` stands for 'assert', causes
  pragmas ``Assert`` and ``Debug`` to be activated. This switch also
  causes preconditions, postconditions, subtype predicates, and
  type invariants to be activated.

  The pragmas have the form::

       pragma Assert (<Boolean-expression> [, <static-string-expression>])
       pragma Debug (<procedure call>)
       pragma Type_Invariant (<type-local-name>, <Boolean-expression>)
       pragma Predicate (<type-local-name>, <Boolean-expression>)
       pragma Precondition (<Boolean-expression>, <string-expression>)
       pragma Postcondition (<Boolean-expression>, <string-expression>)

  The aspects have the form::

       with [Pre|Post|Type_Invariant|Dynamic_Predicate|Static_Predicate]
         => <Boolean-expression>;

  The ``Assert`` pragma causes ``Boolean-expression`` to be tested.
  If the result is ``True``, the pragma has no effect (other than
  possible side effects from evaluating the expression). If the result is
  ``False``, the exception ``Assert_Failure`` declared in the package
  ``System.Assertions`` is raised (passing ``static-string-expression``, if
  present, as the message associated with the exception). If no string
  expression is given, the default is a string containing the file name and
  line number of the pragma.

  The ``Debug`` pragma causes ``procedure`` to be called. Note that
  ``pragma Debug`` may appear within a declaration sequence, allowing
  debugging procedures to be called between declarations.

  For the aspect specification, the ``Boolean-expression`` is evaluated.
  If the result is ``True``, the aspect has no effect. If the result
  is ``False``, the exception ``Assert_Failure`` is raised.

.. _Validity_Checking:

Validity Checking
-----------------

.. index:: Validity Checking

The Ada Reference Manual defines the concept of invalid values (see
RM 13.9.1). The primary source of invalid values is uninitialized
variables. A scalar variable that is left uninitialized may contain
an invalid value; the concept of invalid does not apply to access or
composite types.

It is an error to read an invalid value, but the RM does not require
run-time checks to detect such errors, except for some minimal
checking to prevent erroneous execution (i.e. unpredictable
behavior). This corresponds to the :switch:`-gnatVd` switch below,
which is the default. For example, by default, if the expression of a
case statement is invalid, it will raise Constraint_Error rather than
causing a wild jump, and if an array index on the left-hand side of an
assignment is invalid, it will raise Constraint_Error rather than
overwriting an arbitrary memory location.

The :switch:`-gnatVa` may be used to enable additional validity checks,
which are not required by the RM. These checks are often very
expensive (which is why the RM does not require them). These checks
are useful in tracking down uninitialized variables, but they are
not usually recommended for production builds, and in particular
we do not recommend using these extra validity checking options in
combination with optimization, since this can confuse the optimizer.
If performance is a consideration, leading to the need to optimize,
then the validity checking options should not be used.

The other :switch:`-gnatV{x}` switches below allow finer-grained
control; you can enable whichever validity checks you desire. However,
for most debugging purposes, :switch:`-gnatVa` is sufficient, and the
default :switch:`-gnatVd` (i.e. standard Ada behavior) is usually
sufficient for non-debugging use.

The :switch:`-gnatB` switch tells the compiler to assume that all
values are valid (that is, within their declared subtype range)
except in the context of a use of the Valid attribute. This means
the compiler can generate more efficient code, since the range
of values is better known at compile time. However, an uninitialized
variable can cause wild jumps and memory corruption in this mode.

The :switch:`-gnatV{x}` switch allows control over the validity
checking mode as described below.
The ``x`` argument is a string of letters that
indicate validity checks that are performed or not performed in addition
to the default checks required by Ada as described above.


.. index:: -gnatVa  (gcc)

:switch:`-gnatVa`
  *All validity checks.*

  All validity checks are turned on.
  That is, :switch:`-gnatVa` is
  equivalent to ``gnatVcdfimoprst``.


.. index:: -gnatVc  (gcc)

:switch:`-gnatVc`
  *Validity checks for copies.*

  The right hand side of assignments, and the initializing values of
  object declarations are validity checked.


.. index:: -gnatVd  (gcc)

:switch:`-gnatVd`
  *Default (RM) validity checks.*

  Some validity checks are done by default following normal Ada semantics
  (RM 13.9.1 (9-11)).
  A check is done in case statements that the expression is within the range
  of the subtype. If it is not, Constraint_Error is raised.
  For assignments to array components, a check is done that the expression used
  as index is within the range. If it is not, Constraint_Error is raised.
  Both these validity checks may be turned off using switch :switch:`-gnatVD`.
  They are turned on by default. If :switch:`-gnatVD` is specified, a subsequent
  switch :switch:`-gnatVd` will leave the checks turned on.
  Switch :switch:`-gnatVD` should be used only if you are sure that all such
  expressions have valid values. If you use this switch and invalid values
  are present, then the program is erroneous, and wild jumps or memory
  overwriting may occur.


.. index:: -gnatVe  (gcc)

:switch:`-gnatVe`
  *Validity checks for elementary components.*

  In the absence of this switch, assignments to record or array components are
  not validity checked, even if validity checks for assignments generally
  (:switch:`-gnatVc`) are turned on. In Ada, assignment of composite values do not
  require valid data, but assignment of individual components does. So for
  example, there is a difference between copying the elements of an array with a
  slice assignment, compared to assigning element by element in a loop. This
  switch allows you to turn off validity checking for components, even when they
  are assigned component by component.


.. index:: -gnatVf  (gcc)

:switch:`-gnatVf`
  *Validity checks for floating-point values.*

  In the absence of this switch, validity checking occurs only for discrete
  values. If :switch:`-gnatVf` is specified, then validity checking also applies
  for floating-point values, and NaNs and infinities are considered invalid,
  as well as out of range values for constrained types. Note that this means
  that standard IEEE infinity mode is not allowed. The exact contexts
  in which floating-point values are checked depends on the setting of other
  options. For example, :switch:`-gnatVif` or :switch:`-gnatVfi`
  (the order does not matter) specifies that floating-point parameters of mode
  ``in`` should be validity checked.


.. index:: -gnatVi  (gcc)

:switch:`-gnatVi`
  *Validity checks for ``in`` mode parameters.*

  Arguments for parameters of mode ``in`` are validity checked in function
  and procedure calls at the point of call.


.. index:: -gnatVm  (gcc)

:switch:`-gnatVm`
  *Validity checks for ``in out`` mode parameters.*

  Arguments for parameters of mode ``in out`` are validity checked in
  procedure calls at the point of call. The ``'m'`` here stands for
  modify, since this concerns parameters that can be modified by the call.
  Note that there is no specific option to test ``out`` parameters,
  but any reference within the subprogram will be tested in the usual
  manner, and if an invalid value is copied back, any reference to it
  will be subject to validity checking.


.. index:: -gnatVn  (gcc)

:switch:`-gnatVn`
  *No validity checks.*

  This switch turns off all validity checking, including the default checking
  for case statements and left hand side subscripts. Note that the use of
  the switch :switch:`-gnatp` suppresses all run-time checks, including
  validity checks, and thus implies :switch:`-gnatVn`. When this switch
  is used, it cancels any other :switch:`-gnatV` previously issued.


.. index:: -gnatVo  (gcc)

:switch:`-gnatVo`
  *Validity checks for operator and attribute operands.*

  Arguments for predefined operators and attributes are validity checked.
  This includes all operators in package ``Standard``,
  the shift operators defined as intrinsic in package ``Interfaces``
  and operands for attributes such as ``Pos``. Checks are also made
  on individual component values for composite comparisons, and on the
  expressions in type conversions and qualified expressions. Checks are
  also made on explicit ranges using :samp:`..` (e.g., slices, loops etc).


.. index:: -gnatVp  (gcc)

:switch:`-gnatVp`
  *Validity checks for parameters.*

  This controls the treatment of parameters within a subprogram (as opposed
  to :switch:`-gnatVi` and :switch:`-gnatVm` which control validity testing
  of parameters on a call. If either of these call options is used, then
  normally an assumption is made within a subprogram that the input arguments
  have been validity checking at the point of call, and do not need checking
  again within a subprogram). If :switch:`-gnatVp` is set, then this assumption
  is not made, and parameters are not assumed to be valid, so their validity
  will be checked (or rechecked) within the subprogram.


.. index:: -gnatVr  (gcc)

:switch:`-gnatVr`
  *Validity checks for function returns.*

  The expression in ``return`` statements in functions is validity
  checked.


.. index:: -gnatVs  (gcc)

:switch:`-gnatVs`
  *Validity checks for subscripts.*

  All subscripts expressions are checked for validity, whether they appear
  on the right side or left side (in default mode only left side subscripts
  are validity checked).


.. index:: -gnatVt  (gcc)

:switch:`-gnatVt`
  *Validity checks for tests.*

  Expressions used as conditions in ``if``, ``while`` or ``exit``
  statements are checked, as well as guard expressions in entry calls.


The :switch:`-gnatV` switch may be followed by a string of letters
to turn on a series of validity checking options.
For example, :switch:`-gnatVcr`
specifies that in addition to the default validity checking, copies and
function return expressions are to be validity checked.
In order to make it easier to specify the desired combination of effects,
the upper case letters ``CDFIMORST`` may
be used to turn off the corresponding lower case option.
Thus :switch:`-gnatVaM` turns on all validity checking options except for
checking of ``in out`` parameters.

The specification of additional validity checking generates extra code (and
in the case of :switch:`-gnatVa` the code expansion can be substantial).
However, these additional checks can be very useful in detecting
uninitialized variables, incorrect use of unchecked conversion, and other
errors leading to invalid values. The use of pragma ``Initialize_Scalars``
is useful in conjunction with the extra validity checking, since this
ensures that wherever possible uninitialized variables have invalid values.

See also the pragma ``Validity_Checks`` which allows modification of
the validity checking mode at the program source level, and also allows for
temporary disabling of validity checks.

.. _Style_Checking:

Style Checking
--------------

.. index:: Style checking

.. index:: -gnaty  (gcc)

The :switch:`-gnatyx` switch causes the compiler to
enforce specified style rules. A limited set of style rules has been used
in writing the GNAT sources themselves. This switch allows user programs
to activate all or some of these checks. If the source program fails a
specified style check, an appropriate message is given, preceded by
the character sequence '(style)'. This message does not prevent
successful compilation (unless the :switch:`-gnatwe` switch is used).

Note that this is by no means intended to be a general facility for
checking arbitrary coding standards. It is simply an embedding of the
style rules we have chosen for the GNAT sources. If you are starting
a project which does not have established style standards, you may
find it useful to adopt the entire set of GNAT coding standards, or
some subset of them.

.. only:: PRO or GPL

  If you already have an established set of coding
  standards, then the selected style checking options may
  indeed correspond to choices you have made, but for general checking
  of an existing set of coding rules, you should look to the gnatcheck
  tool, which is designed for that purpose.

The string ``x`` is a sequence of letters or digits
indicating the particular style
checks to be performed. The following checks are defined:


.. index:: -gnaty[0-9]   (gcc)

:switch:`-gnaty0`
  *Specify indentation level.*

  If a digit from 1-9 appears
  in the string after :switch:`-gnaty`
  then proper indentation is checked, with the digit indicating the
  indentation level required. A value of zero turns off this style check.
  The general style of required indentation is as specified by
  the examples in the Ada Reference Manual. Full line comments must be
  aligned with the ``--`` starting on a column that is a multiple of
  the alignment level, or they may be aligned the same way as the following
  non-blank line (this is useful when full line comments appear in the middle
  of a statement, or they may be aligned with the source line on the previous
  non-blank line.

.. index:: -gnatya   (gcc)

:switch:`-gnatya`
  *Check attribute casing.*

  Attribute names, including the case of keywords such as ``digits``
  used as attributes names, must be written in mixed case, that is, the
  initial letter and any letter following an underscore must be uppercase.
  All other letters must be lowercase.


.. index:: -gnatyA (gcc)

:switch:`-gnatyA`
  *Use of array index numbers in array attributes.*

  When using the array attributes First, Last, Range,
  or Length, the index number must be omitted for one-dimensional arrays
  and is required for multi-dimensional arrays.


.. index:: -gnatyb (gcc)

:switch:`-gnatyb`
  *Blanks not allowed at statement end.*

  Trailing blanks are not allowed at the end of statements. The purpose of this
  rule, together with h (no horizontal tabs), is to enforce a canonical format
  for the use of blanks to separate source tokens.


.. index:: -gnatyB (gcc)

:switch:`-gnatyB`
  *Check Boolean operators.*

  The use of AND/OR operators is not permitted except in the cases of modular
  operands, array operands, and simple stand-alone boolean variables or
  boolean constants. In all other cases ``and then``/`or else` are
  required.


.. index:: -gnatyc (gcc)

:switch:`-gnatyc`
  *Check comments, double space.*

  Comments must meet the following set of rules:

  * The ``--`` that starts the column must either start in column one,
    or else at least one blank must precede this sequence.

  * Comments that follow other tokens on a line must have at least one blank
    following the ``--`` at the start of the comment.

  * Full line comments must have at least two blanks following the
    ``--`` that starts the comment, with the following exceptions.

  * A line consisting only of the ``--`` characters, possibly preceded
    by blanks is permitted.

  * A comment starting with ``--x`` where ``x`` is a special character
    is permitted.
    This allows proper processing of the output from specialized tools
    such as ``gnatprep`` (where ``--!`` is used) and in earlier versions of the SPARK
    annotation
    language (where ``--#`` is used). For the purposes of this rule, a
    special character is defined as being in one of the ASCII ranges
    ``16#21#...16#2F#`` or ``16#3A#...16#3F#``.
    Note that this usage is not permitted
    in GNAT implementation units (i.e., when :switch:`-gnatg` is used).

  * A line consisting entirely of minus signs, possibly preceded by blanks, is
    permitted. This allows the construction of box comments where lines of minus
    signs are used to form the top and bottom of the box.

  * A comment that starts and ends with ``--`` is permitted as long as at
    least one blank follows the initial ``--``. Together with the preceding
    rule, this allows the construction of box comments, as shown in the following
    example:

    .. code-block:: ada

       ---------------------------
       -- This is a box comment --
       -- with two text lines.  --
       ---------------------------


.. index:: -gnatyC (gcc)

:switch:`-gnatyC`
  *Check comments, single space.*

  This is identical to ``c`` except that only one space
  is required following the ``--`` of a comment instead of two.


.. index:: -gnatyd (gcc)

:switch:`-gnatyd`
  *Check no DOS line terminators present.*

  All lines must be terminated by a single ASCII.LF
  character (in particular the DOS line terminator sequence CR/LF is not
  allowed).


.. index:: -gnatyD (gcc)

:switch:`-gnatyD`
  *Check declared identifiers in mixed case.*

  Declared identifiers must be in mixed case, as in
  This_Is_An_Identifier. Use -gnatyr in addition to ensure
  that references match declarations.


.. index:: -gnatye (gcc)

:switch:`-gnatye`
  *Check end/exit labels.*

  Optional labels on ``end`` statements ending subprograms and on
  ``exit`` statements exiting named loops, are required to be present.


.. index:: -gnatyf (gcc)

:switch:`-gnatyf`
  *No form feeds or vertical tabs.*

  Neither form feeds nor vertical tab characters are permitted
  in the source text.


.. index:: -gnatyg (gcc)

:switch:`-gnatyg`
  *GNAT style mode.*

  The set of style check switches is set to match that used by the GNAT sources.
  This may be useful when developing code that is eventually intended to be
  incorporated into GNAT. Currently this is equivalent to :switch:`-gnatyydISux`)
  but additional style switches may be added to this set in the future without
  advance notice.


.. index:: -gnatyh (gcc)

:switch:`-gnatyh`
  *No horizontal tabs.*

  Horizontal tab characters are not permitted in the source text.
  Together with the b (no blanks at end of line) check, this
  enforces a canonical form for the use of blanks to separate
  source tokens.


.. index:: -gnatyi (gcc)

:switch:`-gnatyi`
  *Check if-then layout.*

  The keyword ``then`` must appear either on the same
  line as corresponding ``if``, or on a line on its own, lined
  up under the ``if``.


.. index:: -gnatyI (gcc)

:switch:`-gnatyI`
  *check mode IN keywords.*

  Mode ``in`` (the default mode) is not
  allowed to be given explicitly. ``in out`` is fine,
  but not ``in`` on its own.


.. index:: -gnatyk (gcc)

:switch:`-gnatyk`
  *Check keyword casing.*

  All keywords must be in lower case (with the exception of keywords
  such as ``digits`` used as attribute names to which this check
  does not apply).


.. index:: -gnatyl (gcc)

:switch:`-gnatyl`
  *Check layout.*

  Layout of statement and declaration constructs must follow the
  recommendations in the Ada Reference Manual, as indicated by the
  form of the syntax rules. For example an ``else`` keyword must
  be lined up with the corresponding ``if`` keyword.

  There are two respects in which the style rule enforced by this check
  option are more liberal than those in the Ada Reference Manual. First
  in the case of record declarations, it is permissible to put the
  ``record`` keyword on the same line as the ``type`` keyword, and
  then the ``end`` in ``end record`` must line up under ``type``.
  This is also permitted when the type declaration is split on two lines.
  For example, any of the following three layouts is acceptable:

  .. code-block:: ada

    type q is record
       a : integer;
       b : integer;
    end record;

    type q is
       record
          a : integer;
          b : integer;
       end record;

    type q is
       record
          a : integer;
          b : integer;
    end record;

  Second, in the case of a block statement, a permitted alternative
  is to put the block label on the same line as the ``declare`` or
  ``begin`` keyword, and then line the ``end`` keyword up under
  the block label. For example both the following are permitted:

  .. code-block:: ada

    Block : declare
       A : Integer := 3;
    begin
       Proc (A, A);
    end Block;

    Block :
       declare
          A : Integer := 3;
       begin
          Proc (A, A);
       end Block;

  The same alternative format is allowed for loops. For example, both of
  the following are permitted:

  .. code-block:: ada

    Clear : while J < 10 loop
       A (J) := 0;
    end loop Clear;

    Clear :
       while J < 10 loop
          A (J) := 0;
       end loop Clear;


.. index:: -gnatyLnnn (gcc)

:switch:`-gnatyL`
  *Set maximum nesting level.*

  The maximum level of nesting of constructs (including subprograms, loops,
  blocks, packages, and conditionals) may not exceed the given value
  *nnn*. A value of zero disconnects this style check.


.. index:: -gnatym (gcc)

:switch:`-gnatym`
  *Check maximum line length.*

  The length of source lines must not exceed 79 characters, including
  any trailing blanks. The value of 79 allows convenient display on an
  80 character wide device or window, allowing for possible special
  treatment of 80 character lines. Note that this count is of
  characters in the source text. This means that a tab character counts
  as one character in this count and a wide character sequence counts as
  a single character (however many bytes are needed in the encoding).


.. index:: -gnatyMnnn (gcc)

:switch:`-gnatyM`
  *Set maximum line length.*

  The length of lines must not exceed the
  given value *nnn*. The maximum value that can be specified is 32767.
  If neither style option for setting the line length is used, then the
  default is 255. This also controls the maximum length of lexical elements,
  where the only restriction is that they must fit on a single line.


.. index:: -gnatyn (gcc)

:switch:`-gnatyn`
  *Check casing of entities in Standard.*

  Any identifier from Standard must be cased
  to match the presentation in the Ada Reference Manual (for example,
  ``Integer`` and ``ASCII.NUL``).


.. index:: -gnatyN (gcc)

:switch:`-gnatyN`
  *Turn off all style checks.*

  All style check options are turned off.


.. index:: -gnatyo (gcc)

:switch:`-gnatyo`
  *Check order of subprogram bodies.*

  All subprogram bodies in a given scope
  (e.g., a package body) must be in alphabetical order. The ordering
  rule uses normal Ada rules for comparing strings, ignoring casing
  of letters, except that if there is a trailing numeric suffix, then
  the value of this suffix is used in the ordering (e.g., Junk2 comes
  before Junk10).


.. index:: -gnatyO (gcc)

:switch:`-gnatyO`
  *Check that overriding subprograms are explicitly marked as such.*

  This applies to all subprograms of a derived type that override a primitive
  operation of the type, for both tagged and untagged types. In particular,
  the declaration of a primitive operation of a type extension that overrides
  an inherited operation must carry an overriding indicator. Another case is
  the declaration of a function that overrides a predefined operator (such
  as an equality operator).


.. index:: -gnatyp (gcc)

:switch:`-gnatyp`
  *Check pragma casing.*

  Pragma names must be written in mixed case, that is, the
  initial letter and any letter following an underscore must be uppercase.
  All other letters must be lowercase. An exception is that SPARK_Mode is
  allowed as an alternative for Spark_Mode.


.. index:: -gnatyr (gcc)

:switch:`-gnatyr`
  *Check references.*

  All identifier references must be cased in the same way as the
  corresponding declaration. No specific casing style is imposed on
  identifiers. The only requirement is for consistency of references
  with declarations.


.. index:: -gnatys (gcc)

:switch:`-gnatys`
  *Check separate specs.*

  Separate declarations ('specs') are required for subprograms (a
  body is not allowed to serve as its own declaration). The only
  exception is that parameterless library level procedures are
  not required to have a separate declaration. This exception covers
  the most frequent form of main program procedures.


.. index:: -gnatyS (gcc)

:switch:`-gnatyS`
  *Check no statements after then/else.*

  No statements are allowed
  on the same line as a ``then`` or ``else`` keyword following the
  keyword in an ``if`` statement. ``or else`` and ``and then`` are not
  affected, and a special exception allows a pragma to appear after ``else``.


.. index:: -gnatyt (gcc)

:switch:`-gnatyt`
  *Check token spacing.*

  The following token spacing rules are enforced:

  * The keywords ``abs`` and ``not`` must be followed by a space.

  * The token ``=>`` must be surrounded by spaces.

  * The token ``<>`` must be preceded by a space or a left parenthesis.

  * Binary operators other than ``**`` must be surrounded by spaces.
    There is no restriction on the layout of the ``**`` binary operator.

  * Colon must be surrounded by spaces.

  * Colon-equal (assignment, initialization) must be surrounded by spaces.

  * Comma must be the first non-blank character on the line, or be
    immediately preceded by a non-blank character, and must be followed
    by a space.

  * If the token preceding a left parenthesis ends with a letter or digit, then
    a space must separate the two tokens.

  * If the token following a right parenthesis starts with a letter or digit, then
    a space must separate the two tokens.

  * A right parenthesis must either be the first non-blank character on
    a line, or it must be preceded by a non-blank character.

  * A semicolon must not be preceded by a space, and must not be followed by
    a non-blank character.

  * A unary plus or minus may not be followed by a space.

  * A vertical bar must be surrounded by spaces.

  Exactly one blank (and no other white space) must appear between
  a ``not`` token and a following ``in`` token.


.. index:: -gnatyu (gcc)

:switch:`-gnatyu`
  *Check unnecessary blank lines.*

  Unnecessary blank lines are not allowed. A blank line is considered
  unnecessary if it appears at the end of the file, or if more than
  one blank line occurs in sequence.


.. index:: -gnatyx (gcc)

:switch:`-gnatyx`
  *Check extra parentheses.*

  Unnecessary extra level of parentheses (C-style) are not allowed
  around conditions in ``if`` statements, ``while`` statements and
  ``exit`` statements.


.. index:: -gnatyy (gcc)

:switch:`-gnatyy`
  *Set all standard style check options.*

  This is equivalent to ``gnaty3aAbcefhiklmnprst``, that is all checking
  options enabled with the exception of :switch:`-gnatyB`, :switch:`-gnatyd`,
  :switch:`-gnatyI`, :switch:`-gnatyLnnn`, :switch:`-gnatyo`, :switch:`-gnatyO`,
  :switch:`-gnatyS`, :switch:`-gnatyu`, and :switch:`-gnatyx`.


.. index:: -gnaty- (gcc)

:switch:`-gnaty-`
  *Remove style check options.*

  This causes any subsequent options in the string to act as canceling the
  corresponding style check option. To cancel maximum nesting level control,
  use the ``L`` parameter without any integer value after that, because any
  digit following *-* in the parameter string of the :switch:`-gnaty`
  option will be treated as canceling the indentation check. The same is true
  for the ``M`` parameter. ``y`` and ``N`` parameters are not
  allowed after *-*.


.. index:: -gnaty+ (gcc)

:switch:`-gnaty+`
  *Enable style check options.*

  This causes any subsequent options in the string to enable the corresponding
  style check option. That is, it cancels the effect of a previous -,
  if any.


.. end of switch description (leave this comment to ease automatic parsing for
.. GNAT Studio

In the above rules, appearing in column one is always permitted, that is,
counts as meeting either a requirement for a required preceding space,
or as meeting a requirement for no preceding space.

Appearing at the end of a line is also always permitted, that is, counts
as meeting either a requirement for a following space, or as meeting
a requirement for no following space.

If any of these style rules is violated, a message is generated giving
details on the violation. The initial characters of such messages are
always '`(style)`'. Note that these messages are treated as warning
messages, so they normally do not prevent the generation of an object
file. The :switch:`-gnatwe` switch can be used to treat warning messages,
including style messages, as fatal errors.

The switch :switch:`-gnaty` on its own (that is not
followed by any letters or digits) is equivalent
to the use of :switch:`-gnatyy` as described above, that is all
built-in standard style check options are enabled.

The switch :switch:`-gnatyN` clears any previously set style checks.

.. _Run-Time_Checks:

Run-Time Checks
---------------

.. index:: Division by zero

.. index:: Access before elaboration

.. index:: Checks, division by zero

.. index:: Checks, access before elaboration

.. index:: Checks, stack overflow checking

By default, the following checks are suppressed: stack overflow
checks, and checks for access before elaboration on subprogram
calls. All other checks, including overflow checks, range checks and
array bounds checks, are turned on by default. The following ``gcc``
switches refine this default behavior.

.. index:: -gnatp  (gcc)

:switch:`-gnatp`
  .. index:: Suppressing checks

  .. index:: Checks, suppressing

  This switch causes the unit to be compiled
  as though ``pragma Suppress (All_checks)``
  had been present in the source. Validity checks are also eliminated (in
  other words :switch:`-gnatp` also implies :switch:`-gnatVn`.
  Use this switch to improve the performance
  of the code at the expense of safety in the presence of invalid data or
  program bugs.

  Note that when checks are suppressed, the compiler is allowed, but not
  required, to omit the checking code. If the run-time cost of the
  checking code is zero or near-zero, the compiler will generate it even
  if checks are suppressed. In particular, if the compiler can prove
  that a certain check will necessarily fail, it will generate code to
  do an unconditional 'raise', even if checks are suppressed. The
  compiler warns in this case. Another case in which checks may not be
  eliminated is when they are embedded in certain run-time routines such
  as math library routines.

  Of course, run-time checks are omitted whenever the compiler can prove
  that they will not fail, whether or not checks are suppressed.

  Note that if you suppress a check that would have failed, program
  execution is erroneous, which means the behavior is totally
  unpredictable. The program might crash, or print wrong answers, or
  do anything else. It might even do exactly what you wanted it to do
  (and then it might start failing mysteriously next week or next
  year). The compiler will generate code based on the assumption that
  the condition being checked is true, which can result in erroneous
  execution if that assumption is wrong.

  The checks subject to suppression include all the checks defined by the Ada
  standard, the additional implementation defined checks ``Alignment_Check``,
  ``Duplicated_Tag_Check``, ``Predicate_Check``, ``Container_Checks``, ``Tampering_Check``,
  and ``Validity_Check``, as well as any checks introduced using ``pragma Check_Name``.
  Note that ``Atomic_Synchronization`` is not automatically suppressed by use of this option.

  If the code depends on certain checks being active, you can use
  pragma ``Unsuppress`` either as a configuration pragma or as
  a local pragma to make sure that a specified check is performed
  even if ``gnatp`` is specified.

  The :switch:`-gnatp` switch has no effect if a subsequent
  :switch:`-gnat-p` switch appears.


.. index:: -gnat-p  (gcc)
.. index:: Suppressing checks
.. index:: Checks, suppressing
.. index:: Suppress

:switch:`-gnat-p`
  This switch cancels the effect of a previous ``gnatp`` switch.


.. index:: -gnato??  (gcc)
.. index:: Overflow checks
.. index:: Overflow mode
.. index:: Check, overflow

:switch:`-gnato??`
  This switch controls the mode used for computing intermediate
  arithmetic integer operations, and also enables overflow checking.
  For a full description of overflow mode and checking control, see
  the 'Overflow Check Handling in GNAT' appendix in this
  User's Guide.

  Overflow checks are always enabled by this switch. The argument
  controls the mode, using the codes


  *1 = STRICT*
    In STRICT mode, intermediate operations are always done using the
    base type, and overflow checking ensures that the result is within
    the base type range.


  *2 = MINIMIZED*
    In MINIMIZED mode, overflows in intermediate operations are avoided
    where possible by using a larger integer type for the computation
    (typically ``Long_Long_Integer``). Overflow checking ensures that
    the result fits in this larger integer type.


  *3 = ELIMINATED*
    In ELIMINATED mode, overflows in intermediate operations are avoided
    by using multi-precision arithmetic. In this case, overflow checking
    has no effect on intermediate operations (since overflow is impossible).

  If two digits are present after :switch:`-gnato` then the first digit
  sets the mode for expressions outside assertions, and the second digit
  sets the mode for expressions within assertions. Here assertions is used
  in the technical sense (which includes for example precondition and
  postcondition expressions).

  If one digit is present, the corresponding mode is applicable to both
  expressions within and outside assertion expressions.

  If no digits are present, the default is to enable overflow checks
  and set STRICT mode for both kinds of expressions. This is compatible
  with the use of :switch:`-gnato` in previous versions of GNAT.

  .. index:: Machine_Overflows

  Note that the :switch:`-gnato??` switch does not affect the code generated
  for any floating-point operations; it applies only to integer semantics.
  For floating-point, GNAT has the ``Machine_Overflows``
  attribute set to ``False`` and the normal mode of operation is to
  generate IEEE NaN and infinite values on overflow or invalid operations
  (such as dividing 0.0 by 0.0).

  The reason that we distinguish overflow checking from other kinds of
  range constraint checking is that a failure of an overflow check, unlike
  for example the failure of a range check, can result in an incorrect
  value, but cannot cause random memory destruction (like an out of range
  subscript), or a wild jump (from an out of range case value). Overflow
  checking is also quite expensive in time and space, since in general it
  requires the use of double length arithmetic.

  Note again that the default is :switch:`-gnato11` (equivalent to :switch:`-gnato1`),
  so overflow checking is performed in STRICT mode by default.


.. index:: -gnatE  (gcc)
.. index:: Elaboration checks
.. index:: Check, elaboration

:switch:`-gnatE`
  Enables dynamic checks for access-before-elaboration
  on subprogram calls and generic instantiations.
  Note that :switch:`-gnatE` is not necessary for safety, because in the
  default mode, GNAT ensures statically that the checks would not fail.
  For full details of the effect and use of this switch,
  :ref:`Compiling_with_gcc`.


.. index:: -fstack-check  (gcc)
.. index:: Stack Overflow Checking
.. index:: Checks, stack overflow checking

:switch:`-fstack-check`
  Activates stack overflow checking. For full details of the effect and use of
  this switch see :ref:`Stack_Overflow_Checking`.

.. index:: Unsuppress

The setting of these switches only controls the default setting of the
checks. You may modify them using either ``Suppress`` (to remove
checks) or ``Unsuppress`` (to add back suppressed checks) pragmas in
the program source.


.. _Using_gcc_for_Syntax_Checking:

Using ``gcc`` for Syntax Checking
---------------------------------

.. index:: -gnats  (gcc)

:switch:`-gnats`
  The ``s`` stands for 'syntax'.

  Run GNAT in syntax checking only mode. For
  example, the command

  ::

    $ gcc -c -gnats x.adb

  compiles file :file:`x.adb` in syntax-check-only mode. You can check a
  series of files in a single command
  , and can use wildcards to specify such a group of files.
  Note that you must specify the :switch:`-c` (compile
  only) flag in addition to the :switch:`-gnats` flag.

  You may use other switches in conjunction with :switch:`-gnats`. In
  particular, :switch:`-gnatl` and :switch:`-gnatv` are useful to control the
  format of any generated error messages.

  When the source file is empty or contains only empty lines and/or comments,
  the output is a warning:


  ::

    $ gcc -c -gnats -x ada toto.txt
    toto.txt:1:01: warning: empty file, contains no compilation units
    $


  Otherwise, the output is simply the error messages, if any. No object file or
  ALI file is generated by a syntax-only compilation. Also, no units other
  than the one specified are accessed. For example, if a unit ``X``
  |withs| a unit ``Y``, compiling unit ``X`` in syntax
  check only mode does not access the source file containing unit
  ``Y``.

  .. index:: Multiple units, syntax checking

  Normally, GNAT allows only a single unit in a source file. However, this
  restriction does not apply in syntax-check-only mode, and it is possible
  to check a file containing multiple compilation units concatenated
  together. This is primarily used by the ``gnatchop`` utility
  (:ref:`Renaming_Files_with_gnatchop`).

.. _Using_gcc_for_Semantic_Checking:

Using ``gcc`` for Semantic Checking
-----------------------------------



.. index:: -gnatc  (gcc)

:switch:`-gnatc`
  The ``c`` stands for 'check'.
  Causes the compiler to operate in semantic check mode,
  with full checking for all illegalities specified in the
  Ada Reference Manual, but without generation of any object code
  (no object file is generated).

  Because dependent files must be accessed, you must follow the GNAT
  semantic restrictions on file structuring to operate in this mode:

  * The needed source files must be accessible
    (see :ref:`Search_Paths_and_the_Run-Time_Library_RTL`).

  * Each file must contain only one compilation unit.

  * The file name and unit name must match (:ref:`File_Naming_Rules`).

  The output consists of error messages as appropriate. No object file is
  generated. An :file:`ALI` file is generated for use in the context of
  cross-reference tools, but this file is marked as not being suitable
  for binding (since no object file is generated).
  The checking corresponds exactly to the notion of
  legality in the Ada Reference Manual.

  Any unit can be compiled in semantics-checking-only mode, including
  units that would not normally be compiled (subunits,
  and specifications where a separate body is present).

.. _Compiling_Different_Versions_of_Ada:

Compiling Different Versions of Ada
-----------------------------------

The switches described in this section allow you to explicitly specify
the version of the Ada language that your programs are written in.
The default mode is Ada 2012,
but you can also specify Ada 95, Ada 2005 mode, or
indicate Ada 83 compatibility mode.


.. index:: Compatibility with Ada 83
.. index:: -gnat83  (gcc)
.. index:: ACVC, Ada 83 tests
.. index:: Ada 83 mode

:switch:`-gnat83` (Ada 83 Compatibility Mode)
  Although GNAT is primarily an Ada 95 / Ada 2005 compiler, this switch
  specifies that the program is to be compiled in Ada 83 mode. With
  :switch:`-gnat83`, GNAT rejects most post-Ada 83 extensions and applies Ada 83
  semantics where this can be done easily.
  It is not possible to guarantee this switch does a perfect
  job; some subtle tests, such as are
  found in earlier ACVC tests (and that have been removed from the ACATS suite
  for Ada 95), might not compile correctly.
  Nevertheless, this switch may be useful in some circumstances, for example
  where, due to contractual reasons, existing code needs to be maintained
  using only Ada 83 features.

  With few exceptions (most notably the need to use ``<>`` on
  unconstrained :index:`generic formal parameters <Generic formal parameters>`,
  the use of the new Ada 95 / Ada 2005
  reserved words, and the use of packages
  with optional bodies), it is not necessary to specify the
  :switch:`-gnat83` switch when compiling Ada 83 programs, because, with rare
  exceptions, Ada 95 and Ada 2005 are upwardly compatible with Ada 83. Thus
  a correct Ada 83 program is usually also a correct program
  in these later versions of the language standard. For further information
  please refer to the *Compatibility and Porting Guide* chapter in the
  :title:`GNAT Reference Manual`.


.. index:: -gnat95  (gcc)
.. index:: Ada 95 mode

:switch:`-gnat95` (Ada 95 mode)
  This switch directs the compiler to implement the Ada 95 version of the
  language.
  Since Ada 95 is almost completely upwards
  compatible with Ada 83, Ada 83 programs may generally be compiled using
  this switch (see the description of the :switch:`-gnat83` switch for further
  information about Ada 83 mode).
  If an Ada 2005 program is compiled in Ada 95 mode,
  uses of the new Ada 2005 features will cause error
  messages or warnings.

  This switch also can be used to cancel the effect of a previous
  :switch:`-gnat83`, :switch:`-gnat05/2005`, or :switch:`-gnat12/2012`
  switch earlier in the command line.


.. index:: -gnat05  (gcc)
.. index:: -gnat2005  (gcc)
.. index:: Ada 2005 mode

:switch:`-gnat05` or :switch:`-gnat2005` (Ada 2005 mode)
  This switch directs the compiler to implement the Ada 2005 version of the
  language, as documented in the official Ada standards document.
  Since Ada 2005 is almost completely upwards
  compatible with Ada 95 (and thus also with Ada 83), Ada 83 and Ada 95 programs
  may generally be compiled using this switch (see the description of the
  :switch:`-gnat83` and :switch:`-gnat95` switches for further
  information).


.. index:: -gnat12  (gcc)
.. index:: -gnat2012  (gcc)
.. index:: Ada 2012 mode

:switch:`-gnat12` or :switch:`-gnat2012` (Ada 2012 mode)
  This switch directs the compiler to implement the Ada 2012 version of the
  language (also the default).
  Since Ada 2012 is almost completely upwards
  compatible with Ada 2005 (and thus also with Ada 83, and Ada 95),
  Ada 83 and Ada 95 programs
  may generally be compiled using this switch (see the description of the
  :switch:`-gnat83`, :switch:`-gnat95`, and :switch:`-gnat05/2005` switches
  for further information).


.. index:: -gnatX  (gcc)
.. index:: Ada language extensions
.. index:: GNAT extensions

:switch:`-gnatX` (Enable GNAT Extensions)
  This switch directs the compiler to implement the latest version of the
  language (currently Ada 2012) and also to enable certain GNAT implementation
  extensions that are not part of any Ada standard. For a full list of these
  extensions, see the GNAT reference manual.


.. _Character_Set_Control:

Character Set Control
---------------------

.. index:: -gnati  (gcc)

:switch:`-gnati{c}`
  Normally GNAT recognizes the Latin-1 character set in source program
  identifiers, as described in the Ada Reference Manual.
  This switch causes
  GNAT to recognize alternate character sets in identifiers. ``c`` is a
  single character  indicating the character set, as follows:

  ========== ======================================================
  *1*         ISO 8859-1 (Latin-1) identifiers
  *2*         ISO 8859-2 (Latin-2) letters allowed in identifiers
  *3*         ISO 8859-3 (Latin-3) letters allowed in identifiers
  *4*         ISO 8859-4 (Latin-4) letters allowed in identifiers
  *5*         ISO 8859-5 (Cyrillic) letters allowed in identifiers
  *9*         ISO 8859-15 (Latin-9) letters allowed in identifiers
  *p*         IBM PC letters (code page 437) allowed in identifiers
  *8*         IBM PC letters (code page 850) allowed in identifiers
  *f*         Full upper-half codes allowed in identifiers
  *n*         No upper-half codes allowed in identifiers
  *w*         Wide-character codes (that is, codes greater than 255)
              allowed in identifiers
  ========== ======================================================

  See :ref:`Foreign_Language_Representation` for full details on the
  implementation of these character sets.


.. index:: -gnatW  (gcc)

:switch:`-gnatW{e}`
  Specify the method of encoding for wide characters.
  ``e`` is one of the following:

  ========== ======================================================
  *h*        Hex encoding (brackets coding also recognized)
  *u*        Upper half encoding (brackets encoding also recognized)
  *s*        Shift/JIS encoding (brackets encoding also recognized)
  *e*        EUC encoding (brackets encoding also recognized)
  *8*        UTF-8 encoding (brackets encoding also recognized)
  *b*        Brackets encoding only (default value)
  ========== ======================================================

  For full details on these encoding
  methods see :ref:`Wide_Character_Encodings`.
  Note that brackets coding is always accepted, even if one of the other
  options is specified, so for example :switch:`-gnatW8` specifies that both
  brackets and UTF-8 encodings will be recognized. The units that are
  with'ed directly or indirectly will be scanned using the specified
  representation scheme, and so if one of the non-brackets scheme is
  used, it must be used consistently throughout the program. However,
  since brackets encoding is always recognized, it may be conveniently
  used in standard libraries, allowing these libraries to be used with
  any of the available coding schemes.

  Note that brackets encoding only applies to program text. Within comments,
  brackets are considered to be normal graphic characters, and bracket sequences
  are never recognized as wide characters.

  If no :switch:`-gnatW?` parameter is present, then the default
  representation is normally Brackets encoding only. However, if the
  first three characters of the file are 16#EF# 16#BB# 16#BF# (the standard
  byte order mark or BOM for UTF-8), then these three characters are
  skipped and the default representation for the file is set to UTF-8.

  Note that the wide character representation that is specified (explicitly
  or by default) for the main program also acts as the default encoding used
  for Wide_Text_IO files if not specifically overridden by a WCEM form
  parameter.


When no :switch:`-gnatW?` is specified, then characters (other than wide
characters represented using brackets notation) are treated as 8-bit
Latin-1 codes. The codes recognized are the Latin-1 graphic characters,
and ASCII format effectors (CR, LF, HT, VT). Other lower half control
characters in the range 16#00#..16#1F# are not accepted in program text
or in comments. Upper half control characters (16#80#..16#9F#) are rejected
in program text, but allowed and ignored in comments. Note in particular
that the Next Line (NEL) character whose encoding is 16#85# is not recognized
as an end of line in this default mode. If your source program contains
instances of the NEL character used as a line terminator,
you must use UTF-8 encoding for the whole
source program. In default mode, all lines must be ended by a standard
end of line sequence (CR, CR/LF, or LF).

Note that the convention of simply accepting all upper half characters in
comments means that programs that use standard ASCII for program text, but
UTF-8 encoding for comments are accepted in default mode, providing that the
comments are ended by an appropriate (CR, or CR/LF, or LF) line terminator.
This is a common mode for many programs with foreign language comments.

.. _File_Naming_Control:

File Naming Control
-------------------

.. index:: -gnatk  (gcc)

:switch:`-gnatk{n}`
  Activates file name 'krunching'. ``n``, a decimal integer in the range
  1-999, indicates the maximum allowable length of a file name (not
  including the :file:`.ads` or :file:`.adb` extension). The default is not
  to enable file name krunching.

  For the source file naming rules, :ref:`File_Naming_Rules`.

.. _Subprogram_Inlining_Control:

Subprogram Inlining Control
---------------------------

.. index:: -gnatn  (gcc)

:switch:`-gnatn[12]`
  The ``n`` here is intended to suggest the first syllable of the word 'inline'.
  GNAT recognizes and processes ``Inline`` pragmas. However, for inlining to
  actually occur, optimization must be enabled and, by default, inlining of
  subprograms across units is not performed. If you want to additionally
  enable inlining of subprograms specified by pragma ``Inline`` across units,
  you must also specify this switch.

  In the absence of this switch, GNAT does not attempt inlining across units
  and does not access the bodies of subprograms for which ``pragma Inline`` is
  specified if they are not in the current unit.

  You can optionally specify the inlining level: 1 for moderate inlining across
  units, which is a good compromise between compilation times and performances
  at run time, or 2 for full inlining across units, which may bring about
  longer compilation times. If no inlining level is specified, the compiler will
  pick it based on the optimization level: 1 for :switch:`-O1`, :switch:`-O2` or
  :switch:`-Os` and 2 for :switch:`-O3`.

  If you specify this switch the compiler will access these bodies,
  creating an extra source dependency for the resulting object file, and
  where possible, the call will be inlined.
  For further details on when inlining is possible
  see :ref:`Inlining_of_Subprograms`.


.. index:: -gnatN  (gcc)

:switch:`-gnatN`
  This switch activates front-end inlining which also
  generates additional dependencies.

  When using a gcc-based back end (in practice this means using any version
  of GNAT other than the JGNAT, .NET or GNAAMP versions), then the use of
  :switch:`-gnatN` is deprecated, and the use of :switch:`-gnatn` is preferred.
  Historically front end inlining was more extensive than the gcc back end
  inlining, but that is no longer the case.

.. _Auxiliary_Output_Control:

Auxiliary Output Control
------------------------

.. index:: -gnatu  (gcc)

:switch:`-gnatu`
  Print a list of units required by this compilation on :file:`stdout`.
  The listing includes all units on which the unit being compiled depends
  either directly or indirectly.


.. index:: -pass-exit-codes  (gcc)

:switch:`-pass-exit-codes`
  If this switch is not used, the exit code returned by ``gcc`` when
  compiling multiple files indicates whether all source files have
  been successfully used to generate object files or not.

  When :switch:`-pass-exit-codes` is used, ``gcc`` exits with an extended
  exit status and allows an integrated development environment to better
  react to a compilation failure. Those exit status are:

  ========== ======================================================
  *5*        There was an error in at least one source file.
  *3*        At least one source file did not generate an object file.
  *2*        The compiler died unexpectedly (internal error for example).
  *0*        An object file has been generated for every source file.
  ========== ======================================================

.. _Debugging_Control:

Debugging Control
-----------------

  .. index:: Debugging options


.. index:: -gnatd  (gcc)

:switch:`-gnatd{x}`
  Activate internal debugging switches. ``x`` is a letter or digit, or
  string of letters or digits, which specifies the type of debugging
  outputs desired. Normally these are used only for internal development
  or system debugging purposes. You can find full documentation for these
  switches in the body of the ``Debug`` unit in the compiler source
  file :file:`debug.adb`.


.. index:: -gnatG  (gcc)

:switch:`-gnatG[={nn}]`
  This switch causes the compiler to generate auxiliary output containing
  a pseudo-source listing of the generated expanded code. Like most Ada
  compilers, GNAT works by first transforming the high level Ada code into
  lower level constructs. For example, tasking operations are transformed
  into calls to the tasking run-time routines. A unique capability of GNAT
  is to list this expanded code in a form very close to normal Ada source.
  This is very useful in understanding the implications of various Ada
  usage on the efficiency of the generated code. There are many cases in
  Ada (e.g., the use of controlled types), where simple Ada statements can
  generate a lot of run-time code. By using :switch:`-gnatG` you can identify
  these cases, and consider whether it may be desirable to modify the coding
  approach to improve efficiency.

  The optional parameter ``nn`` if present after -gnatG specifies an
  alternative maximum line length that overrides the normal default of 72.
  This value is in the range 40-999999, values less than 40 being silently
  reset to 40. The equal sign is optional.

  The format of the output is very similar to standard Ada source, and is
  easily understood by an Ada programmer. The following special syntactic
  additions correspond to low level features used in the generated code that
  do not have any exact analogies in pure Ada source form. The following
  is a partial list of these special constructions. See the spec
  of package ``Sprint`` in file :file:`sprint.ads` for a full list.

  .. index:: -gnatL  (gcc)

  If the switch :switch:`-gnatL` is used in conjunction with
  :switch:`-gnatG`, then the original source lines are interspersed
  in the expanded source (as comment lines with the original line number).

  :samp:`new {xxx} [storage_pool = {yyy}]`
    Shows the storage pool being used for an allocator.


  :samp:`at end {procedure-name};`
    Shows the finalization (cleanup) procedure for a scope.


  :samp:`(if {expr} then {expr} else {expr})`
    Conditional expression equivalent to the ``x?y:z`` construction in C.


  :samp:`{target}^({source})`
    A conversion with floating-point truncation instead of rounding.


  :samp:`{target}?({source})`
    A conversion that bypasses normal Ada semantic checking. In particular
    enumeration types and fixed-point types are treated simply as integers.


  :samp:`{target}?^({source})`
    Combines the above two cases.


  :samp:`{x} #/ {y}`

  :samp:`{x} #mod {y}`

  :samp:`{x} # {y}`

  :samp:`{x} #rem {y}`
    A division or multiplication of fixed-point values which are treated as
    integers without any kind of scaling.


  :samp:`free {expr} [storage_pool = {xxx}]`
    Shows the storage pool associated with a ``free`` statement.


  :samp:`[subtype or type declaration]`
    Used to list an equivalent declaration for an internally generated
    type that is referenced elsewhere in the listing.


  :samp:`freeze {type-name} [{actions}]`
    Shows the point at which ``type-name`` is frozen, with possible
    associated actions to be performed at the freeze point.


  :samp:`reference {itype}`
    Reference (and hence definition) to internal type ``itype``.


  :samp:`{function-name}! ({arg}, {arg}, {arg})`
    Intrinsic function call.


  :samp:`{label-name} : label`
    Declaration of label ``labelname``.


  :samp:`#$ {subprogram-name}`
    An implicit call to a run-time support routine
    (to meet the requirement of H.3.1(9) in a
    convenient manner).


  :samp:`{expr} && {expr} && {expr} ... && {expr}`
    A multiple concatenation (same effect as ``expr`` & ``expr`` &
    ``expr``, but handled more efficiently).


  :samp:`[constraint_error]`
    Raise the ``Constraint_Error`` exception.


  :samp:`{expression}'reference`
    A pointer to the result of evaluating {expression}.


  :samp:`{target-type}!({source-expression})`
    An unchecked conversion of ``source-expression`` to ``target-type``.


  :samp:`[{numerator}/{denominator}]`
    Used to represent internal real literals (that) have no exact
    representation in base 2-16 (for example, the result of compile time
    evaluation of the expression 1.0/27.0).


.. index:: -gnatD  (gcc)

:switch:`-gnatD[=nn]`
  When used in conjunction with :switch:`-gnatG`, this switch causes
  the expanded source, as described above for
  :switch:`-gnatG` to be written to files with names
  :file:`xxx.dg`, where :file:`xxx` is the normal file name,
  instead of to the standard output file. For
  example, if the source file name is :file:`hello.adb`, then a file
  :file:`hello.adb.dg` will be written.  The debugging
  information generated by the ``gcc`` :switch:`-g` switch
  will refer to the generated :file:`xxx.dg` file. This allows
  you to do source level debugging using the generated code which is
  sometimes useful for complex code, for example to find out exactly
  which part of a complex construction raised an exception. This switch
  also suppresses generation of cross-reference information (see
  :switch:`-gnatx`) since otherwise the cross-reference information
  would refer to the :file:`.dg` file, which would cause
  confusion since this is not the original source file.

  Note that :switch:`-gnatD` actually implies :switch:`-gnatG`
  automatically, so it is not necessary to give both options.
  In other words :switch:`-gnatD` is equivalent to :switch:`-gnatDG`).

  .. index:: -gnatL  (gcc)

  If the switch :switch:`-gnatL` is used in conjunction with
  :switch:`-gnatDG`, then the original source lines are interspersed
  in the expanded source (as comment lines with the original line number).

  The optional parameter ``nn`` if present after -gnatD specifies an
  alternative maximum line length that overrides the normal default of 72.
  This value is in the range 40-999999, values less than 40 being silently
  reset to 40. The equal sign is optional.


.. index:: -gnatr  (gcc)
.. index:: pragma Restrictions

:switch:`-gnatr`
  This switch causes pragma Restrictions to be treated as Restriction_Warnings
  so that violation of restrictions causes warnings rather than illegalities.
  This is useful during the development process when new restrictions are added
  or investigated. The switch also causes pragma Profile to be treated as
  Profile_Warnings, and pragma Restricted_Run_Time and pragma Ravenscar set
  restriction warnings rather than restrictions.


.. index:: -gnatR  (gcc)

:switch:`-gnatR[0|1|2|3|4][e][j][m][s]`
  This switch controls output from the compiler of a listing showing
  representation information for declared types, objects and subprograms.
  For :switch:`-gnatR0`, no information is output (equivalent to omitting
  the :switch:`-gnatR` switch). For :switch:`-gnatR1` (which is the default,
  so :switch:`-gnatR` with no parameter has the same effect), size and
  alignment information is listed for declared array and record types.

  For :switch:`-gnatR2`, size and alignment information is listed for all
  declared types and objects. The ``Linker_Section`` is also listed for any
  entity for which the ``Linker_Section`` is set explicitly or implicitly (the
  latter case occurs for objects of a type for which a ``Linker_Section``
  is set).

  For :switch:`-gnatR3`, symbolic expressions for values that are computed
  at run time for records are included. These symbolic expressions have
  a mostly obvious format with #n being used to represent the value of the
  n'th discriminant. See source files :file:`repinfo.ads/adb` in the
  GNAT sources for full details on the format of :switch:`-gnatR3` output.

  For :switch:`-gnatR4`, information for relevant compiler-generated types
  is also listed, i.e. when they are structurally part of other declared
  types and objects.

  If the switch is followed by an ``e`` (e.g. :switch:`-gnatR2e`), then
  extended representation information for record sub-components of records
  is included.

  If the switch is followed by an ``m`` (e.g. :switch:`-gnatRm`), then
  subprogram conventions and parameter passing mechanisms for all the
  subprograms are included.

  If the switch is followed by a ``j`` (e.g., :switch:`-gnatRj`), then
  the output is in the JSON data interchange format specified by the
  ECMA-404 standard. The semantic description of this JSON output is
  available in the specification of the Repinfo unit present in the
  compiler sources.

  If the switch is followed by an ``s`` (e.g., :switch:`-gnatR3s`), then
  the output is to a file with the name :file:`file.rep` where ``file`` is
  the name of the corresponding source file, except if ``j`` is also
  specified, in which case the file name is :file:`file.json`.

  Note that it is possible for record components to have zero size. In
  this case, the component clause uses an obvious extension of permitted
  Ada syntax, for example ``at 0 range 0 .. -1``.


.. index:: -gnatS  (gcc)

:switch:`-gnatS`
  The use of the switch :switch:`-gnatS` for an
  Ada compilation will cause the compiler to output a
  representation of package Standard in a form very
  close to standard Ada. It is not quite possible to
  do this entirely in standard Ada (since new
  numeric base types cannot be created in standard
  Ada), but the output is easily
  readable to any Ada programmer, and is useful to
  determine the characteristics of target dependent
  types in package Standard.


.. index:: -gnatx  (gcc)

:switch:`-gnatx`
  Normally the compiler generates full cross-referencing information in
  the :file:`ALI` file. This information is used by a number of tools,
  including ``gnatfind`` and ``gnatxref``. The :switch:`-gnatx` switch
  suppresses this information. This saves some space and may slightly
  speed up compilation, but means that these tools cannot be used.


.. index:: -fgnat-encodings  (gcc)

:switch:`-fgnat-encodings=[all|gdb|minimal]`
  This switch controls the balance between GNAT encodings and standard DWARF
  emitted in the debug information.

  Historically, old debug formats like stabs were not powerful enough to
  express some Ada types (for instance, variant records or fixed-point types).
  To work around this, GNAT introduced proprietary encodings that embed the
  missing information ("GNAT encodings").

  Recent versions of the DWARF debug information format are now able to
  correctly describe most of these Ada constructs ("standard DWARF"). As
  third-party tools started to use this format, GNAT has been enhanced to
  generate it. However, most tools (including GDB) are still relying on GNAT
  encodings.

  To support all tools, GNAT needs to be versatile about the balance between
  generation of GNAT encodings and standard DWARF. This is what
  :switch:`-fgnat-encodings` is about.

  * ``=all``: Emit all GNAT encodings, and then emit as much standard DWARF as
    possible so it does not conflict with GNAT encodings.
  * ``=gdb``: Emit as much standard DWARF as possible as long as the current
    GDB handles it. Emit GNAT encodings for the rest.
  * ``=minimal``: Emit as much standard DWARF as possible and emit GNAT
    encodings for the rest.


.. _Exception_Handling_Control:

Exception Handling Control
--------------------------

GNAT uses two methods for handling exceptions at run time. The
``setjmp/longjmp`` method saves the context when entering
a frame with an exception handler. Then when an exception is
raised, the context can be restored immediately, without the
need for tracing stack frames. This method provides very fast
exception propagation, but introduces significant overhead for
the use of exception handlers, even if no exception is raised.

The other approach is called 'zero cost' exception handling.
With this method, the compiler builds static tables to describe
the exception ranges. No dynamic code is required when entering
a frame containing an exception handler. When an exception is
raised, the tables are used to control a back trace of the
subprogram invocation stack to locate the required exception
handler. This method has considerably poorer performance for
the propagation of exceptions, but there is no overhead for
exception handlers if no exception is raised. Note that in this
mode and in the context of mixed Ada and C/C++ programming,
to propagate an exception through a C/C++ code, the C/C++ code
must be compiled with the :switch:`-funwind-tables` GCC's
option.

The following switches may be used to control which of the
two exception handling methods is used.



.. index:: --RTS=sjlj  (gnatmake)

:switch:`--RTS=sjlj`
  This switch causes the setjmp/longjmp run-time (when available) to be used
  for exception handling. If the default
  mechanism for the target is zero cost exceptions, then
  this switch can be used to modify this default, and must be
  used for all units in the partition.
  This option is rarely used. One case in which it may be
  advantageous is if you have an application where exception
  raising is common and the overall performance of the
  application is improved by favoring exception propagation.


.. index:: --RTS=zcx  (gnatmake)
.. index:: Zero Cost Exceptions

:switch:`--RTS=zcx`
  This switch causes the zero cost approach to be used
  for exception handling. If this is the default mechanism for the
  target (see below), then this switch is unneeded. If the default
  mechanism for the target is setjmp/longjmp exceptions, then
  this switch can be used to modify this default, and must be
  used for all units in the partition.
  This option can only be used if the zero cost approach
  is available for the target in use, otherwise it will generate an error.

The same option :switch:`--RTS` must be used both for ``gcc``
and ``gnatbind``. Passing this option to ``gnatmake``
(:ref:`Switches_for_gnatmake`) will ensure the required consistency
through the compilation and binding steps.

.. _Units_to_Sources_Mapping_Files:

Units to Sources Mapping Files
------------------------------



.. index:: -gnatem  (gcc)

:switch:`-gnatem={path}`
  A mapping file is a way to communicate to the compiler two mappings:
  from unit names to file names (without any directory information) and from
  file names to path names (with full directory information). These mappings
  are used by the compiler to short-circuit the path search.

  The use of mapping files is not required for correct operation of the
  compiler, but mapping files can improve efficiency, particularly when
  sources are read over a slow network connection. In normal operation,
  you need not be concerned with the format or use of mapping files,
  and the :switch:`-gnatem` switch is not a switch that you would use
  explicitly. It is intended primarily for use by automatic tools such as
  ``gnatmake`` running under the project file facility. The
  description here of the format of mapping files is provided
  for completeness and for possible use by other tools.

  A mapping file is a sequence of sets of three lines. In each set, the
  first line is the unit name, in lower case, with ``%s`` appended
  for specs and ``%b`` appended for bodies; the second line is the
  file name; and the third line is the path name.

  Example::

       main%b
       main.2.ada
       /gnat/project1/sources/main.2.ada


  When the switch :switch:`-gnatem` is specified, the compiler will
  create in memory the two mappings from the specified file. If there is
  any problem (nonexistent file, truncated file or duplicate entries),
  no mapping will be created.

  Several :switch:`-gnatem` switches may be specified; however, only the
  last one on the command line will be taken into account.

  When using a project file, ``gnatmake`` creates a temporary
  mapping file and communicates it to the compiler using this switch.


.. _Code_Generation_Control:

Code Generation Control
-----------------------

The GCC technology provides a wide range of target dependent
:switch:`-m` switches for controlling
details of code generation with respect to different versions of
architectures. This includes variations in instruction sets (e.g.,
different members of the power pc family), and different requirements
for optimal arrangement of instructions (e.g., different members of
the x86 family). The list of available :switch:`-m` switches may be
found in the GCC documentation.

Use of these :switch:`-m` switches may in some cases result in improved
code performance.

The GNAT technology is tested and qualified without any
:switch:`-m` switches,
so generally the most reliable approach is to avoid the use of these
switches. However, we generally expect most of these switches to work
successfully with GNAT, and many customers have reported successful
use of these options.

Our general advice is to avoid the use of :switch:`-m` switches unless
special needs lead to requirements in this area. In particular,
there is no point in using :switch:`-m` switches to improve performance
unless you actually see a performance improvement.


.. _Linker_Switches:

Linker Switches
===============

Linker switches can be specified after :switch:`-largs` builder switch.

.. index:: -fuse-ld=name

:switch:`-fuse-ld={name}`
  Linker to be used. The default is ``bfd`` for :file:`ld.bfd`,
  the alternative being ``gold`` for :file:`ld.gold`. The later is
  a more recent and faster linker, but only available on GNU/Linux
  platforms.

.. _Binding_with_gnatbind:

Binding with ``gnatbind``
=========================

.. index:: ! gnatbind

This chapter describes the GNAT binder, ``gnatbind``, which is used
to bind compiled GNAT objects.

The ``gnatbind`` program performs four separate functions:

* Checks that a program is consistent, in accordance with the rules in
  Chapter 10 of the Ada Reference Manual. In particular, error
  messages are generated if a program uses inconsistent versions of a
  given unit.

* Checks that an acceptable order of elaboration exists for the program
  and issues an error message if it cannot find an order of elaboration
  that satisfies the rules in Chapter 10 of the Ada Language Manual.

* Generates a main program incorporating the given elaboration order.
  This program is a small Ada package (body and spec) that
  must be subsequently compiled
  using the GNAT compiler. The necessary compilation step is usually
  performed automatically by ``gnatlink``. The two most important
  functions of this program
  are to call the elaboration routines of units in an appropriate order
  and to call the main program.

* Determines the set of object files required by the given main program.
  This information is output in the forms of comments in the generated program,
  to be read by the ``gnatlink`` utility used to link the Ada application.

.. _Running_gnatbind:

Running ``gnatbind``
--------------------

The form of the ``gnatbind`` command is

.. code-block:: sh

  $ gnatbind [ switches ] mainprog[.ali] [ switches ]


where :file:`mainprog.adb` is the Ada file containing the main program
unit body. ``gnatbind`` constructs an Ada
package in two files whose names are
:file:`b~mainprog.ads`, and :file:`b~mainprog.adb`.
For example, if given the
parameter :file:`hello.ali`, for a main program contained in file
:file:`hello.adb`, the binder output files would be :file:`b~hello.ads`
and :file:`b~hello.adb`.

When doing consistency checking, the binder takes into consideration
any source files it can locate. For example, if the binder determines
that the given main program requires the package ``Pack``, whose
:file:`.ALI`
file is :file:`pack.ali` and whose corresponding source spec file is
:file:`pack.ads`, it attempts to locate the source file :file:`pack.ads`
(using the same search path conventions as previously described for the
``gcc`` command). If it can locate this source file, it checks that
the time stamps
or source checksums of the source and its references to in :file:`ALI` files
match. In other words, any :file:`ALI` files that mentions this spec must have
resulted from compiling this version of the source file (or in the case
where the source checksums match, a version close enough that the
difference does not matter).

.. index:: Source files, use by binder

The effect of this consistency checking, which includes source files, is
that the binder ensures that the program is consistent with the latest
version of the source files that can be located at bind time. Editing a
source file without compiling files that depend on the source file cause
error messages to be generated by the binder.

For example, suppose you have a main program :file:`hello.adb` and a
package ``P``, from file :file:`p.ads` and you perform the following
steps:

* Enter ``gcc -c hello.adb`` to compile the main program.

* Enter ``gcc -c p.ads`` to compile package ``P``.

* Edit file :file:`p.ads`.

* Enter ``gnatbind hello``.

At this point, the file :file:`p.ali` contains an out-of-date time stamp
because the file :file:`p.ads` has been edited. The attempt at binding
fails, and the binder generates the following error messages:


::

  error: "hello.adb" must be recompiled ("p.ads" has been modified)
  error: "p.ads" has been modified and must be recompiled


Now both files must be recompiled as indicated, and then the bind can
succeed, generating a main program. You need not normally be concerned
with the contents of this file, but for reference purposes a sample
binder output file is given in :ref:`Example_of_Binder_Output_File`.

In most normal usage, the default mode of ``gnatbind`` which is to
generate the main package in Ada, as described in the previous section.
In particular, this means that any Ada programmer can read and understand
the generated main program. It can also be debugged just like any other
Ada code provided the :switch:`-g` switch is used for
``gnatbind`` and ``gnatlink``.

.. _Switches_for_gnatbind:

Switches for ``gnatbind``
-------------------------

The following switches are available with ``gnatbind``; details will
be presented in subsequent sections.


.. index:: --version  (gnatbind)

:switch:`--version`
  Display Copyright and version, then exit disregarding all other options.


.. index:: --help  (gnatbind)

:switch:`--help`
  If :switch:`--version` was not used, display usage, then exit disregarding
  all other options.


.. index:: -a  (gnatbind)

:switch:`-a`
  Indicates that, if supported by the platform, the adainit procedure should
  be treated as an initialisation routine by the linker (a constructor). This
  is intended to be used by the Project Manager to automatically initialize
  shared Stand-Alone Libraries.


.. index:: -aO  (gnatbind)

:switch:`-aO`
  Specify directory to be searched for ALI files.


.. index:: -aI  (gnatbind)

:switch:`-aI`
  Specify directory to be searched for source file.


.. index:: -A  (gnatbind)

:switch:`-A[={filename}]`
  Output ALI list (to standard output or to the named file).


.. index:: -b  (gnatbind)

:switch:`-b`
  Generate brief messages to :file:`stderr` even if verbose mode set.


.. index:: -c  (gnatbind)

:switch:`-c`
  Check only, no generation of binder output file.


.. index:: -dnn[k|m] (gnatbind)

:switch:`-d{nn}[k|m]`
  This switch can be used to change the default task stack size value
  to a specified size ``nn``, which is expressed in bytes by default, or
  in kilobytes when suffixed with ``k`` or in megabytes when suffixed
  with ``m``.
  In the absence of a :samp:`[k|m]` suffix, this switch is equivalent,
  in effect, to completing all task specs with

  .. code-block:: ada

       pragma Storage_Size (nn);

  When they do not already have such a pragma.


.. index:: -D  (gnatbind)

:switch:`-D{nn}[k|m]`
  Set the default secondary stack size to ``nn``. The suffix indicates whether
  the size is in bytes (no suffix), kilobytes (``k`` suffix) or megabytes
  (``m`` suffix).

  The secondary stack holds objects of unconstrained types that are returned by
  functions, for example unconstrained Strings. The size of the secondary stack
  can be dynamic or fixed depending on the target.

  For most targets, the secondary stack grows on demand and is implemented as
  a chain of blocks in the heap. In this case, the default secondary stack size
  determines the initial size of the secondary stack for each task and the
  smallest amount the secondary stack can grow by.

  For Ravenscar, ZFP, and Cert run-times the size of the secondary stack is
  fixed. This switch can be used to change the default size of these stacks.
  The default secondary stack size can be overridden on a per-task basis if
  individual tasks have different secondary stack requirements. This is
  achieved through the Secondary_Stack_Size aspect that takes the size of the
  secondary stack in bytes.

.. index:: -e  (gnatbind)

:switch:`-e`
  Output complete list of elaboration-order dependencies.


.. index:: -Ea  (gnatbind)

:switch:`-Ea`
  Store tracebacks in exception occurrences when the target supports it.
  The "a" is for "address"; tracebacks will contain hexadecimal addresses,
  unless symbolic tracebacks are enabled.

  See also the packages ``GNAT.Traceback`` and
  ``GNAT.Traceback.Symbolic`` for more information.
  Note that on x86 ports, you must not use :switch:`-fomit-frame-pointer`
  ``gcc`` option.


.. index:: -Es  (gnatbind)

:switch:`-Es`
  Store tracebacks in exception occurrences when the target supports it.
  The "s" is for "symbolic"; symbolic tracebacks are enabled.


.. index:: -E  (gnatbind)

:switch:`-E`
  Currently the same as ``-Ea``.


.. index:: -f  (gnatbind)

:switch:`-f{elab-order}`
  Force elaboration order. For further details see :ref:`Elaboration_Control`
  and :ref:`Elaboration_Order_Handling_in_GNAT`.


.. index:: -F  (gnatbind)

:switch:`-F`
  Force the checks of elaboration flags. ``gnatbind`` does not normally
  generate checks of elaboration flags for the main executable, except when
  a Stand-Alone Library is used. However, there are cases when this cannot be
  detected by gnatbind. An example is importing an interface of a Stand-Alone
  Library through a pragma Import and only specifying through a linker switch
  this Stand-Alone Library. This switch is used to guarantee that elaboration
  flag checks are generated.


.. index:: -h  (gnatbind)

:switch:`-h`
  Output usage (help) information.


.. index:: -H  (gnatbind)

:switch:`-H`
  Legacy elaboration order model enabled. For further details see
  :ref:`Elaboration_Order_Handling_in_GNAT`.


.. index:: -H32  (gnatbind)

:switch:`-H32`
  Use 32-bit allocations for ``__gnat_malloc`` (and thus for access types).
  For further details see :ref:`Dynamic_Allocation_Control`.


.. index:: -H64  (gnatbind)
.. index:: __gnat_malloc

:switch:`-H64`
  Use 64-bit allocations for ``__gnat_malloc`` (and thus for access types).
  For further details see :ref:`Dynamic_Allocation_Control`.


  .. index:: -I  (gnatbind)

:switch:`-I`
  Specify directory to be searched for source and ALI files.


  .. index:: -I-  (gnatbind)

:switch:`-I-`
  Do not look for sources in the current directory where ``gnatbind`` was
  invoked, and do not look for ALI files in the directory containing the
  ALI file named in the ``gnatbind`` command line.


  .. index:: -l  (gnatbind)

:switch:`-l`
  Output chosen elaboration order.


  .. index:: -L  (gnatbind)

:switch:`-L{xxx}`
  Bind the units for library building. In this case the ``adainit`` and
  ``adafinal`` procedures (:ref:`Binding_with_Non-Ada_Main_Programs`)
  are renamed to :samp:`{xxx}init` and
  :samp:`{xxx}final`.
  Implies -n.
  (:ref:`GNAT_and_Libraries`, for more details.)


  .. index:: -M  (gnatbind)

:switch:`-M{xyz}`
  Rename generated main program from main to xyz. This option is
  supported on cross environments only.

  .. index:: -m  (gnatbind)

:switch:`-m{n}`
  Limit number of detected errors or warnings to ``n``, where ``n`` is
  in the range 1..999999. The default value if no switch is
  given is 9999. If the number of warnings reaches this limit, then a
  message is output and further warnings are suppressed, the bind
  continues in this case. If the number of errors reaches this
  limit, then a message is output and the bind is abandoned.
  A value of zero means that no limit is enforced. The equal
  sign is optional.

  .. index:: -minimal  (gnatbind)

:switch:`-minimal`
  Generate a binder file suitable for space-constrained applications. When
  active, binder-generated objects not required for program operation are no
  longer generated. **Warning:** this option comes with the following
  limitations:

  * Starting the program's execution in the debugger will cause it to
    stop at the start of the ``main`` function instead of the main subprogram. 
    This can be worked around by manually inserting a breakpoint on that 
    subprogram and resuming the program's execution until reaching that breakpoint.
  * Programs using GNAT.Compiler_Version will not link.

  .. index:: -n  (gnatbind)

:switch:`-n`
  No main program.


  .. index:: -nostdinc  (gnatbind)

:switch:`-nostdinc`
  Do not look for sources in the system default directory.


  .. index:: -nostdlib  (gnatbind)

:switch:`-nostdlib`
  Do not look for library files in the system default directory.


  .. index:: --RTS  (gnatbind)

:switch:`--RTS={rts-path}`
  Specifies the default location of the run-time library. Same meaning as the
  equivalent ``gnatmake`` flag (:ref:`Switches_for_gnatmake`).

  .. index:: -o   (gnatbind)

:switch:`-o {file}`
  Name the output file ``file`` (default is :file:`b~`xxx`.adb`).
  Note that if this option is used, then linking must be done manually,
  gnatlink cannot be used.


  .. index:: -O  (gnatbind)

:switch:`-O[={filename}]`
  Output object list (to standard output or to the named file).


  .. index:: -p  (gnatbind)

:switch:`-p`
  Pessimistic (worst-case) elaboration order.


  .. index:: -P  (gnatbind)

:switch:`-P`
  Generate binder file suitable for CodePeer.


  .. index:: -R  (gnatbind)

:switch:`-R`
  Output closure source list, which includes all non-run-time units that are
  included in the bind.


  .. index:: -Ra  (gnatbind)

:switch:`-Ra`
  Like :switch:`-R` but the list includes run-time units.


  .. index:: -s  (gnatbind)

:switch:`-s`
  Require all source files to be present.


  .. index:: -S  (gnatbind)

:switch:`-S{xxx}`
  Specifies the value to be used when detecting uninitialized scalar
  objects with pragma Initialize_Scalars.
  The ``xxx`` string specified with the switch is one of:

  * ``in`` for an invalid value.

    If zero is invalid for the discrete type in question,
    then the scalar value is set to all zero bits.
    For signed discrete types, the largest possible negative value of
    the underlying scalar is set (i.e. a one bit followed by all zero bits).
    For unsigned discrete types, the underlying scalar value is set to all
    one bits. For floating-point types, a NaN value is set
    (see body of package System.Scalar_Values for exact values).

  * ``lo`` for low value.

    If zero is invalid for the discrete type in question,
    then the scalar value is set to all zero bits.
    For signed discrete types, the largest possible negative value of
    the underlying scalar is set (i.e. a one bit followed by all zero bits).
    For unsigned discrete types, the underlying scalar value is set to all
    zero bits. For floating-point, a small value is set
    (see body of package System.Scalar_Values for exact values).

  * ``hi`` for high value.

    If zero is invalid for the discrete type in question,
    then the scalar value is set to all one bits.
    For signed discrete types, the largest possible positive value of
    the underlying scalar is set (i.e. a zero bit followed by all one bits).
    For unsigned discrete types, the underlying scalar value is set to all
    one bits. For floating-point, a large value is set
    (see body of package System.Scalar_Values for exact values).

  * ``xx`` for hex value (two hex digits).

    The underlying scalar is set to a value consisting of repeated bytes, whose
    value corresponds to the given value. For example if ``BF`` is given,
    then a 32-bit scalar value will be set to the bit patterm ``16#BFBFBFBF#``.

  .. index:: GNAT_INIT_SCALARS

  In addition, you can specify :switch:`-Sev` to indicate that the value is
  to be set at run time. In this case, the program will look for an environment
  variable of the form :samp:`GNAT_INIT_SCALARS={yy}`, where ``yy`` is one
  of :samp:`in/lo/hi/{xx}` with the same meanings as above.
  If no environment variable is found, or if it does not have a valid value,
  then the default is ``in`` (invalid values).

.. index:: -static  (gnatbind)

:switch:`-static`
  Link against a static GNAT run-time.


  .. index:: -shared  (gnatbind)

:switch:`-shared`
  Link against a shared GNAT run-time when available.


  .. index:: -t  (gnatbind)

:switch:`-t`
  Tolerate time stamp and other consistency errors.


  .. index:: -T  (gnatbind)

:switch:`-T{n}`
  Set the time slice value to ``n`` milliseconds. If the system supports
  the specification of a specific time slice value, then the indicated value
  is used. If the system does not support specific time slice values, but
  does support some general notion of round-robin scheduling, then any
  nonzero value will activate round-robin scheduling.

  A value of zero is treated specially. It turns off time
  slicing, and in addition, indicates to the tasking run-time that the
  semantics should match as closely as possible the Annex D
  requirements of the Ada RM, and in particular sets the default
  scheduling policy to ``FIFO_Within_Priorities``.


  .. index:: -u  (gnatbind)

:switch:`-u{n}`
  Enable dynamic stack usage, with ``n`` results stored and displayed
  at program termination. A result is generated when a task
  terminates. Results that can't be stored are displayed on the fly, at
  task termination. This option is currently not supported on Itanium
  platforms. (See :ref:`Dynamic_Stack_Usage_Analysis` for details.)


  .. index:: -v  (gnatbind)

:switch:`-v`
  Verbose mode. Write error messages, header, summary output to
  :file:`stdout`.


  .. index:: -V  (gnatbind)

:switch:`-V{key}={value}`
  Store the given association of ``key`` to ``value`` in the bind environment.
  Values stored this way can be retrieved at run time using
  ``GNAT.Bind_Environment``.


  .. index:: -w  (gnatbind)

:switch:`-w{x}`
  Warning mode; ``x`` = s/e for suppress/treat as error.


  .. index:: -Wx  (gnatbind)

:switch:`-Wx{e}`
  Override default wide character encoding for standard Text_IO files.


  .. index:: -x  (gnatbind)

:switch:`-x`
  Exclude source files (check object consistency only).


  .. index:: -xdr  (gnatbind)

:switch:`-xdr`
  Use the target-independent XDR protocol for stream oriented attributes
  instead of the default implementation which is based on direct binary
  representations and is therefore target-and endianness-dependent.


  .. index:: -Xnnn  (gnatbind)

:switch:`-X{nnn}`
  Set default exit status value, normally 0 for POSIX compliance.


  .. index:: -y  (gnatbind)

:switch:`-y`
  Enable leap seconds support in ``Ada.Calendar`` and its children.


  .. index:: -z  (gnatbind)

:switch:`-z`
  No main subprogram.

You may obtain this listing of switches by running ``gnatbind`` with
no arguments.


.. _Consistency-Checking_Modes:

Consistency-Checking Modes
^^^^^^^^^^^^^^^^^^^^^^^^^^

As described earlier, by default ``gnatbind`` checks
that object files are consistent with one another and are consistent
with any source files it can locate. The following switches control binder
access to sources.


  .. index:: -s  (gnatbind)

:switch:`-s`
  Require source files to be present. In this mode, the binder must be
  able to locate all source files that are referenced, in order to check
  their consistency. In normal mode, if a source file cannot be located it
  is simply ignored. If you specify this switch, a missing source
  file is an error.


  .. index:: -Wx  (gnatbind)

:switch:`-Wx{e}`
  Override default wide character encoding for standard Text_IO files.
  Normally the default wide character encoding method used for standard
  [Wide\_[Wide\_]]Text_IO files is taken from the encoding specified for
  the main source input (see description of switch
  :switch:`-gnatWx` for the compiler). The
  use of this switch for the binder (which has the same set of
  possible arguments) overrides this default as specified.


  .. index:: -x  (gnatbind)

:switch:`-x`
  Exclude source files. In this mode, the binder only checks that ALI
  files are consistent with one another. Source files are not accessed.
  The binder runs faster in this mode, and there is still a guarantee that
  the resulting program is self-consistent.
  If a source file has been edited since it was last compiled, and you
  specify this switch, the binder will not detect that the object
  file is out of date with respect to the source file. Note that this is the
  mode that is automatically used by ``gnatmake`` because in this
  case the checking against sources has already been performed by
  ``gnatmake`` in the course of compilation (i.e., before binding).


.. _Binder_Error_Message_Control:

Binder Error Message Control
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following switches provide control over the generation of error
messages from the binder:



  .. index:: -v  (gnatbind)

:switch:`-v`
  Verbose mode. In the normal mode, brief error messages are generated to
  :file:`stderr`. If this switch is present, a header is written
  to :file:`stdout` and any error messages are directed to :file:`stdout`.
  All that is written to :file:`stderr` is a brief summary message.


  .. index:: -b  (gnatbind)

:switch:`-b`
  Generate brief error messages to :file:`stderr` even if verbose mode is
  specified. This is relevant only when used with the
  :switch:`-v` switch.


  .. index:: -m  (gnatbind)

:switch:`-m{n}`
  Limits the number of error messages to ``n``, a decimal integer in the
  range 1-999. The binder terminates immediately if this limit is reached.


  .. index:: -M  (gnatbind)

:switch:`-M{xxx}`
  Renames the generated main program from ``main`` to ``xxx``.
  This is useful in the case of some cross-building environments, where
  the actual main program is separate from the one generated
  by ``gnatbind``.


  .. index:: -ws  (gnatbind)
  .. index:: Warnings

:switch:`-ws`
  Suppress all warning messages.


  .. index:: -we  (gnatbind)

:switch:`-we`
  Treat any warning messages as fatal errors.


  .. index:: -t  (gnatbind)
  .. index:: Time stamp checks, in binder
  .. index:: Binder consistency checks
  .. index:: Consistency checks, in binder

:switch:`-t`
  The binder performs a number of consistency checks including:


  * Check that time stamps of a given source unit are consistent

  * Check that checksums of a given source unit are consistent

  * Check that consistent versions of ``GNAT`` were used for compilation

  * Check consistency of configuration pragmas as required

  Normally failure of such checks, in accordance with the consistency
  requirements of the Ada Reference Manual, causes error messages to be
  generated which abort the binder and prevent the output of a binder
  file and subsequent link to obtain an executable.

  The :switch:`-t` switch converts these error messages
  into warnings, so that
  binding and linking can continue to completion even in the presence of such
  errors. The result may be a failed link (due to missing symbols), or a
  non-functional executable which has undefined semantics.

  .. note::

     This means that :switch:`-t` should be used only in unusual situations,
     with extreme care.

.. _Elaboration_Control:

Elaboration Control
^^^^^^^^^^^^^^^^^^^

The following switches provide additional control over the elaboration
order. For further details see :ref:`Elaboration_Order_Handling_in_GNAT`.


.. index:: -f  (gnatbind)

:switch:`-f{elab-order}`
  Force elaboration order.

  ``elab-order`` should be the name of a "forced elaboration order file", that
  is, a text file containing library item names, one per line. A name of the
  form "some.unit%s" or "some.unit (spec)" denotes the spec of Some.Unit. A
  name of the form "some.unit%b" or "some.unit (body)" denotes the body of
  Some.Unit. Each pair of lines is taken to mean that there is an elaboration
  dependence of the second line on the first. For example, if the file
  contains:

  .. code-block:: ada

      this (spec)
      this (body)
      that (spec)
      that (body)

  then the spec of This will be elaborated before the body of This, and the
  body of This will be elaborated before the spec of That, and the spec of That
  will be elaborated before the body of That. The first and last of these three
  dependences are already required by Ada rules, so this file is really just
  forcing the body of This to be elaborated before the spec of That.

  The given order must be consistent with Ada rules, or else ``gnatbind`` will
  give elaboration cycle errors. For example, if you say x (body) should be
  elaborated before x (spec), there will be a cycle, because Ada rules require
  x (spec) to be elaborated before x (body); you can't have the spec and body
  both elaborated before each other.

  If you later add "with That;" to the body of This, there will be a cycle, in
  which case you should erase either "this (body)" or "that (spec)" from the
  above forced elaboration order file.

  Blank lines and Ada-style comments are ignored. Unit names that do not exist
  in the program are ignored. Units in the GNAT predefined library are also
  ignored.


.. index:: -p  (gnatbind)

:switch:`-p`
  Pessimistic elaboration order

  This switch is only applicable to the pre-20.x legacy elaboration models.
  The post-20.x elaboration model uses a more informed approach of ordering
  the units.

  Normally the binder attempts to choose an elaboration order that is likely to
  minimize the likelihood of an elaboration order error resulting in raising a
  ``Program_Error`` exception. This switch reverses the action of the binder,
  and requests that it deliberately choose an order that is likely to maximize
  the likelihood of an elaboration error. This is useful in ensuring
  portability and avoiding dependence on accidental fortuitous elaboration
  ordering.

  Normally it only makes sense to use the :switch:`-p` switch if dynamic
  elaboration checking is used (:switch:`-gnatE` switch used for compilation).
  This is because in the default static elaboration mode, all necessary
  ``Elaborate`` and ``Elaborate_All`` pragmas are implicitly inserted.
  These implicit pragmas are still respected by the binder in :switch:`-p`
  mode, so a safe elaboration order is assured.

  Note that :switch:`-p` is not intended for production use; it is more for
  debugging/experimental use.

.. _Output_Control:

Output Control
^^^^^^^^^^^^^^

The following switches allow additional control over the output
generated by the binder.


  .. index:: -c  (gnatbind)

:switch:`-c`
  Check only. Do not generate the binder output file. In this mode the
  binder performs all error checks but does not generate an output file.


  .. index:: -e  (gnatbind)

:switch:`-e`
  Output complete list of elaboration-order dependencies, showing the
  reason for each dependency. This output can be rather extensive but may
  be useful in diagnosing problems with elaboration order. The output is
  written to :file:`stdout`.


  .. index:: -h  (gnatbind)

:switch:`-h`
  Output usage information. The output is written to :file:`stdout`.


  .. index:: -K  (gnatbind)

:switch:`-K`
  Output linker options to :file:`stdout`. Includes library search paths,
  contents of pragmas Ident and Linker_Options, and libraries added
  by ``gnatbind``.


  .. index:: -l  (gnatbind)

:switch:`-l`
  Output chosen elaboration order. The output is written to :file:`stdout`.


  .. index:: -O  (gnatbind)

:switch:`-O`
  Output full names of all the object files that must be linked to provide
  the Ada component of the program. The output is written to :file:`stdout`.
  This list includes the files explicitly supplied and referenced by the user
  as well as implicitly referenced run-time unit files. The latter are
  omitted if the corresponding units reside in shared libraries. The
  directory names for the run-time units depend on the system configuration.


  .. index:: -o  (gnatbind)

:switch:`-o {file}`
  Set name of output file to ``file`` instead of the normal
  :file:`b~`mainprog`.adb` default. Note that ``file`` denote the Ada
  binder generated body filename.
  Note that if this option is used, then linking must be done manually.
  It is not possible to use gnatlink in this case, since it cannot locate
  the binder file.


  .. index:: -r  (gnatbind)

:switch:`-r`
  Generate list of ``pragma Restrictions`` that could be applied to
  the current unit. This is useful for code audit purposes, and also may
  be used to improve code generation in some cases.


.. _Dynamic_Allocation_Control:

Dynamic Allocation Control
^^^^^^^^^^^^^^^^^^^^^^^^^^

The heap control switches -- :switch:`-H32` and :switch:`-H64` --
determine whether dynamic allocation uses 32-bit or 64-bit memory.
They only affect compiler-generated allocations via ``__gnat_malloc``;
explicit calls to ``malloc`` and related functions from the C
run-time library are unaffected.

:switch:`-H32`
  Allocate memory on 32-bit heap


:switch:`-H64`
  Allocate memory on 64-bit heap.  This is the default
  unless explicitly overridden by a ``'Size`` clause on the access type.

These switches are only effective on VMS platforms.


.. _Binding_with_Non-Ada_Main_Programs:

Binding with Non-Ada Main Programs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The description so far has assumed that the main
program is in Ada, and that the task of the binder is to generate a
corresponding function ``main`` that invokes this Ada main
program. GNAT also supports the building of executable programs where
the main program is not in Ada, but some of the called routines are
written in Ada and compiled using GNAT (:ref:`Mixed_Language_Programming`).
The following switch is used in this situation:


  .. index:: -n  (gnatbind)

:switch:`-n`
  No main program. The main program is not in Ada.

In this case, most of the functions of the binder are still required,
but instead of generating a main program, the binder generates a file
containing the following callable routines:

  .. index:: adainit

  ``adainit``
    You must call this routine to initialize the Ada part of the program by
    calling the necessary elaboration routines. A call to ``adainit`` is
    required before the first call to an Ada subprogram.

    Note that it is assumed that the basic execution environment must be setup
    to be appropriate for Ada execution at the point where the first Ada
    subprogram is called. In particular, if the Ada code will do any
    floating-point operations, then the FPU must be setup in an appropriate
    manner. For the case of the x86, for example, full precision mode is
    required. The procedure GNAT.Float_Control.Reset may be used to ensure
    that the FPU is in the right state.

  .. index:: adafinal

  ``adafinal``
    You must call this routine to perform any library-level finalization
    required by the Ada subprograms. A call to ``adafinal`` is required
    after the last call to an Ada subprogram, and before the program
    terminates.

.. index:: -n  (gnatbind)
.. index:: Binder, multiple input files

If the :switch:`-n` switch
is given, more than one ALI file may appear on
the command line for ``gnatbind``. The normal ``closure``
calculation is performed for each of the specified units. Calculating
the closure means finding out the set of units involved by tracing
|with| references. The reason it is necessary to be able to
specify more than one ALI file is that a given program may invoke two or
more quite separate groups of Ada units.

The binder takes the name of its output file from the last specified ALI
file, unless overridden by the use of the :switch:`-o file`.

.. index:: -o  (gnatbind)

The output is an Ada unit in source form that can be compiled with GNAT.
This compilation occurs automatically as part of the ``gnatlink``
processing.

Currently the GNAT run-time requires a FPU using 80 bits mode
precision. Under targets where this is not the default it is required to
call GNAT.Float_Control.Reset before using floating point numbers (this
include float computation, float input and output) in the Ada code. A
side effect is that this could be the wrong mode for the foreign code
where floating point computation could be broken after this call.


.. _Binding_Programs_with_No_Main_Subprogram:

Binding Programs with No Main Subprogram
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible to have an Ada program which does not have a main
subprogram. This program will call the elaboration routines of all the
packages, then the finalization routines.

The following switch is used to bind programs organized in this manner:

  .. index:: -z  (gnatbind)

:switch:`-z`
  Normally the binder checks that the unit name given on the command line
  corresponds to a suitable main subprogram. When this switch is used,
  a list of ALI files can be given, and the execution of the program
  consists of elaboration of these units in an appropriate order. Note
  that the default wide character encoding method for standard Text_IO
  files is always set to Brackets if this switch is set (you can use
  the binder switch
  :switch:`-Wx` to override this default).


.. _Command-Line_Access:

Command-Line Access
-------------------

The package ``Ada.Command_Line`` provides access to the command-line
arguments and program name. In order for this interface to operate
correctly, the two variables

.. code-block:: c

     int gnat_argc;
     char **gnat_argv;

.. index:: gnat_argv
.. index:: gnat_argc

are declared in one of the GNAT library routines. These variables must
be set from the actual ``argc`` and ``argv`` values passed to the
main program. With no *n* present, ``gnatbind``
generates the C main program to automatically set these variables.
If the *n* switch is used, there is no automatic way to
set these variables. If they are not set, the procedures in
``Ada.Command_Line`` will not be available, and any attempt to use
them will raise ``Constraint_Error``. If command line access is
required, your main program must set ``gnat_argc`` and
``gnat_argv`` from the ``argc`` and ``argv`` values passed to
it.


.. _Search_Paths_for_gnatbind:

Search Paths for ``gnatbind``
-----------------------------

The binder takes the name of an ALI file as its argument and needs to
locate source files as well as other ALI files to verify object consistency.

For source files, it follows exactly the same search rules as ``gcc``
(see :ref:`Search_Paths_and_the_Run-Time_Library_RTL`). For ALI files the
directories searched are:

* The directory containing the ALI file named in the command line, unless
  the switch :switch:`-I-` is specified.

* All directories specified by :switch:`-I`
  switches on the ``gnatbind``
  command line, in the order given.

  .. index:: ADA_PRJ_OBJECTS_FILE

* Each of the directories listed in the text file whose name is given
  by the :envvar:`ADA_PRJ_OBJECTS_FILE` environment variable.

  :envvar:`ADA_PRJ_OBJECTS_FILE` is normally set by gnatmake or by the gnat
  driver when project files are used. It should not normally be set
  by other means.

  .. index:: ADA_OBJECTS_PATH

* Each of the directories listed in the value of the
  :envvar:`ADA_OBJECTS_PATH` environment variable.
  Construct this value
  exactly as the :envvar:`PATH` environment variable: a list of directory
  names separated by colons (semicolons when working with the NT version
  of GNAT).

* The content of the :file:`ada_object_path` file which is part of the GNAT
  installation tree and is used to store standard libraries such as the
  GNAT Run-Time Library (RTL) unless the switch :switch:`-nostdlib` is
  specified. See :ref:`Installing_a_library`

.. index:: -I  (gnatbind)
.. index:: -aI  (gnatbind)
.. index:: -aO  (gnatbind)

In the binder the switch :switch:`-I`
is used to specify both source and
library file paths. Use :switch:`-aI`
instead if you want to specify
source paths only, and :switch:`-aO`
if you want to specify library paths
only. This means that for the binder
:switch:`-I{dir}` is equivalent to
:switch:`-aI{dir}`
:switch:`-aO`{dir}`.
The binder generates the bind file (a C language source file) in the
current working directory.

.. index:: Ada
.. index:: System
.. index:: Interfaces
.. index:: GNAT

The packages ``Ada``, ``System``, and ``Interfaces`` and their
children make up the GNAT Run-Time Library, together with the package
GNAT and its children, which contain a set of useful additional
library functions provided by GNAT. The sources for these units are
needed by the compiler and are kept together in one directory. The ALI
files and object files generated by compiling the RTL are needed by the
binder and the linker and are kept together in one directory, typically
different from the directory containing the sources. In a normal
installation, you need not specify these directory names when compiling
or binding. Either the environment variables or the built-in defaults
cause these files to be found.

Besides simplifying access to the RTL, a major use of search paths is
in compiling sources from multiple directories. This can make
development environments much more flexible.


.. _Examples_of_gnatbind_Usage:

Examples of ``gnatbind`` Usage
------------------------------

Here are some examples of ``gnatbind`` invovations:

  ::

     gnatbind hello

  The main program ``Hello`` (source program in :file:`hello.adb`) is
  bound using the standard switch settings. The generated main program is
  :file:`b~hello.adb`. This is the normal, default use of the binder.

  ::

     gnatbind hello -o mainprog.adb

  The main program ``Hello`` (source program in :file:`hello.adb`) is
  bound using the standard switch settings. The generated main program is
  :file:`mainprog.adb` with the associated spec in
  :file:`mainprog.ads`. Note that you must specify the body here not the
  spec. Note that if this option is used, then linking must be done manually,
  since gnatlink will not be able to find the generated file.


.. _Linking_with_gnatlink:

Linking with ``gnatlink``
=========================

.. index:: ! gnatlink

This chapter discusses ``gnatlink``, a tool that links
an Ada program and builds an executable file. This utility
invokes the system linker (via the ``gcc`` command)
with a correct list of object files and library references.
``gnatlink`` automatically determines the list of files and
references for the Ada part of a program. It uses the binder file
generated by the ``gnatbind`` to determine this list.

.. _Running_gnatlink:

Running ``gnatlink``
--------------------

The form of the ``gnatlink`` command is


.. code-block:: sh

    $ gnatlink [ switches ] mainprog [.ali]
               [ non-Ada objects ] [ linker options ]



The arguments of ``gnatlink`` (switches, main ``ALI`` file,
non-Ada objects
or linker options) may be in any order, provided that no non-Ada object may
be mistaken for a main :file:`ALI` file.
Any file name :file:`F` without the :file:`.ali`
extension will be taken as the main :file:`ALI` file if a file exists
whose name is the concatenation of :file:`F` and :file:`.ali`.

:file:`mainprog.ali` references the ALI file of the main program.
The :file:`.ali` extension of this file can be omitted. From this
reference, ``gnatlink`` locates the corresponding binder file
:file:`b~mainprog.adb` and, using the information in this file along
with the list of non-Ada objects and linker options, constructs a
linker command file to create the executable.

The arguments other than the ``gnatlink`` switches and the main
:file:`ALI` file are passed to the linker uninterpreted.
They typically include the names of
object files for units written in other languages than Ada and any library
references required to resolve references in any of these foreign language
units, or in ``Import`` pragmas in any Ada units.

``linker options`` is an optional list of linker specific
switches.
The default linker called by gnatlink is ``gcc`` which in
turn calls the appropriate system linker.

One useful option for the linker is :switch:`-s`: it reduces the size of the
executable by removing all symbol table and relocation information from the
executable.

Standard options for the linker such as :switch:`-lmy_lib` or
:switch:`-Ldir` can be added as is.
For options that are not recognized by
``gcc`` as linker options, use the ``gcc`` switches
:switch:`-Xlinker` or :switch:`-Wl,`.

Refer to the GCC documentation for
details.

Here is an example showing how to generate a linker map:

.. code-block:: sh

     $ gnatlink my_prog -Wl,-Map,MAPFILE


Using ``linker options`` it is possible to set the program stack and
heap size.
See :ref:`Setting_Stack_Size_from_gnatlink` and
:ref:`Setting_Heap_Size_from_gnatlink`.

``gnatlink`` determines the list of objects required by the Ada
program and prepends them to the list of objects passed to the linker.
``gnatlink`` also gathers any arguments set by the use of
``pragma Linker_Options`` and adds them to the list of arguments
presented to the linker.


.. _Switches_for_gnatlink:

Switches for ``gnatlink``
-------------------------

The following switches are available with the ``gnatlink`` utility:

.. index:: --version  (gnatlink)

:switch:`--version`
  Display Copyright and version, then exit disregarding all other options.


.. index:: --help  (gnatlink)

:switch:`--help`
  If :switch:`--version` was not used, display usage, then exit disregarding
  all other options.


.. index:: Command line length
.. index:: -f  (gnatlink)

:switch:`-f`
  On some targets, the command line length is limited, and ``gnatlink``
  will generate a separate file for the linker if the list of object files
  is too long.
  The :switch:`-f` switch forces this file
  to be generated even if
  the limit is not exceeded. This is useful in some cases to deal with
  special situations where the command line length is exceeded.


.. index:: Debugging information, including
.. index:: -g  (gnatlink)

:switch:`-g`
  The option to include debugging information causes the Ada bind file (in
  other words, :file:`b~mainprog.adb`) to be compiled with :switch:`-g`.
  In addition, the binder does not delete the :file:`b~mainprog.adb`,
  :file:`b~mainprog.o` and :file:`b~mainprog.ali` files.
  Without :switch:`-g`, the binder removes these files by default.

.. index:: -n  (gnatlink)

:switch:`-n`
  Do not compile the file generated by the binder. This may be used when
  a link is rerun with different options, but there is no need to recompile
  the binder file.


.. index:: -v  (gnatlink)

:switch:`-v`
  Verbose mode. Causes additional information to be output, including a full
  list of the included object files.
  This switch option is most useful when you want
  to see what set of object files are being used in the link step.


.. index:: -v -v  (gnatlink)

:switch:`-v -v`
  Very verbose mode. Requests that the compiler operate in verbose mode when
  it compiles the binder file, and that the system linker run in verbose mode.


.. index:: -o  (gnatlink)

:switch:`-o {exec-name}`
  ``exec-name`` specifies an alternate name for the generated
  executable program. If this switch is omitted, the executable has the same
  name as the main unit. For example, ``gnatlink try.ali`` creates
  an executable called :file:`try`.


.. index:: -B  (gnatlink)

:switch:`-B{dir}`
  Load compiler executables (for example, ``gnat1``, the Ada compiler)
  from ``dir`` instead of the default location. Only use this switch
  when multiple versions of the GNAT compiler are available.
  See the ``Directory Options`` section in :title:`The_GNU_Compiler_Collection`
  for further details. You would normally use the :switch:`-b` or
  :switch:`-V` switch instead.


.. index:: -M  (gnatlink)

:switch:`-M`
  When linking an executable, create a map file. The name of the map file
  has the same name as the executable with extension ".map".


.. index:: -M=  (gnatlink)

:switch:`-M={mapfile}`
  When linking an executable, create a map file. The name of the map file is
  ``mapfile``.


.. index:: --GCC=compiler_name  (gnatlink)

:switch:`--GCC={compiler_name}`
  Program used for compiling the binder file. The default is
  ``gcc``. You need to use quotes around ``compiler_name`` if
  ``compiler_name`` contains spaces or other separator characters.
  As an example ``--GCC="foo -x -y"`` will instruct ``gnatlink`` to
  use ``foo -x -y`` as your compiler. Note that switch ``-c`` is always
  inserted after your command name. Thus in the above example the compiler
  command that will be used by ``gnatlink`` will be ``foo -c -x -y``.
  A limitation of this syntax is that the name and path name of the executable
  itself must not include any embedded spaces. If the compiler executable is
  different from the default one (gcc or <prefix>-gcc), then the back-end
  switches in the ALI file are not used to compile the binder generated source.
  For example, this is the case with ``--GCC="foo -x -y"``. But the back end
  switches will be used for ``--GCC="gcc -gnatv"``. If several
  ``--GCC=compiler_name`` are used, only the last ``compiler_name``
  is taken into account. However, all the additional switches are also taken
  into account. Thus,
  ``--GCC="foo -x -y" --GCC="bar -z -t"`` is equivalent to
  ``--GCC="bar -x -y -z -t"``.


.. index:: --LINK=  (gnatlink)

:switch:`--LINK={name}`
  ``name`` is the name of the linker to be invoked. This is especially
  useful in mixed language programs since languages such as C++ require
  their own linker to be used. When this switch is omitted, the default
  name for the linker is ``gcc``. When this switch is used, the
  specified linker is called instead of ``gcc`` with exactly the same
  parameters that would have been passed to ``gcc`` so if the desired
  linker requires different parameters it is necessary to use a wrapper
  script that massages the parameters before invoking the real linker. It
  may be useful to control the exact invocation by using the verbose
  switch.


.. _Using_the_GNU_make_Utility:

Using the GNU ``make`` Utility
==============================

.. index:: make (GNU), GNU make

This chapter offers some examples of makefiles that solve specific
problems. It does not explain how to write a makefile, nor does it try to replace the
``gnatmake`` utility (:ref:`The_GNAT_Make_Program_gnatmake`).

All the examples in this section are specific to the GNU version of
make. Although ``make`` is a standard utility, and the basic language
is the same, these examples use some advanced features found only in
``GNU make``.

.. _Using_gnatmake_in_a_Makefile:

Using gnatmake in a Makefile
----------------------------

.. index makefile (GNU make)

Complex project organizations can be handled in a very powerful way by
using GNU make combined with gnatmake. For instance, here is a Makefile
which allows you to build each subsystem of a big project into a separate
shared library. Such a makefile allows you to significantly reduce the link
time of very big applications while maintaining full coherence at
each step of the build process.

The list of dependencies are handled automatically by
``gnatmake``. The Makefile is simply used to call gnatmake in each of
the appropriate directories.

Note that you should also read the example on how to automatically
create the list of directories
(:ref:`Automatically_Creating_a_List_of_Directories`)
which might help you in case your project has a lot of subdirectories.


.. code-block:: makefile

  ## This Makefile is intended to be used with the following directory
  ## configuration:
  ##  - The sources are split into a series of csc (computer software components)
  ##    Each of these csc is put in its own directory.
  ##    Their name are referenced by the directory names.
  ##    They will be compiled into shared library (although this would also work
  ##    with static libraries
  ##  - The main program (and possibly other packages that do not belong to any
  ##    csc is put in the top level directory (where the Makefile is).
  ##       toplevel_dir __ first_csc  (sources) __ lib (will contain the library)
  ##                    \\_ second_csc (sources) __ lib (will contain the library)
  ##                    \\_ ...
  ## Although this Makefile is build for shared library, it is easy to modify
  ## to build partial link objects instead (modify the lines with -shared and
  ## gnatlink below)
  ##
  ## With this makefile, you can change any file in the system or add any new
  ## file, and everything will be recompiled correctly (only the relevant shared
  ## objects will be recompiled, and the main program will be re-linked).

  # The list of computer software component for your project. This might be
  # generated automatically.
  CSC_LIST=aa bb cc

  # Name of the main program (no extension)
  MAIN=main

  # If we need to build objects with -fPIC, uncomment the following line
  #NEED_FPIC=-fPIC

  # The following variable should give the directory containing libgnat.so
  # You can get this directory through 'gnatls -v'. This is usually the last
  # directory in the Object_Path.
  GLIB=...

  # The directories for the libraries
  # (This macro expands the list of CSC to the list of shared libraries, you
  # could simply use the expanded form:
  # LIB_DIR=aa/lib/libaa.so bb/lib/libbb.so cc/lib/libcc.so
  LIB_DIR=${foreach dir,${CSC_LIST},${dir}/lib/lib${dir}.so}

  ${MAIN}: objects ${LIB_DIR}
      gnatbind ${MAIN} ${CSC_LIST:%=-aO%/lib} -shared
      gnatlink ${MAIN} ${CSC_LIST:%=-l%}

  objects::
      # recompile the sources
      gnatmake -c -i ${MAIN}.adb ${NEED_FPIC} ${CSC_LIST:%=-I%}

  # Note: In a future version of GNAT, the following commands will be simplified
  # by a new tool, gnatmlib
  ${LIB_DIR}:
      mkdir -p ${dir $@ }
      cd ${dir $@ } && gcc -shared -o ${notdir $@ } ../*.o -L${GLIB} -lgnat
      cd ${dir $@ } && cp -f ../*.ali .

  # The dependencies for the modules
  # Note that we have to force the expansion of *.o, since in some cases
  # make won't be able to do it itself.
  aa/lib/libaa.so: ${wildcard aa/*.o}
  bb/lib/libbb.so: ${wildcard bb/*.o}
  cc/lib/libcc.so: ${wildcard cc/*.o}

  # Make sure all of the shared libraries are in the path before starting the
  # program
  run::
      LD_LIBRARY_PATH=`pwd`/aa/lib:`pwd`/bb/lib:`pwd`/cc/lib ./${MAIN}

  clean::
      ${RM} -rf ${CSC_LIST:%=%/lib}
      ${RM} ${CSC_LIST:%=%/*.ali}
      ${RM} ${CSC_LIST:%=%/*.o}
      ${RM} *.o *.ali ${MAIN}


.. _Automatically_Creating_a_List_of_Directories:

Automatically Creating a List of Directories
--------------------------------------------

In most makefiles, you will have to specify a list of directories, and
store it in a variable. For small projects, it is often easier to
specify each of them by hand, since you then have full control over what
is the proper order for these directories, which ones should be
included.

However, in larger projects, which might involve hundreds of
subdirectories, it might be more convenient to generate this list
automatically.

The example below presents two methods. The first one, although less
general, gives you more control over the list. It involves wildcard
characters, that are automatically expanded by ``make``. Its
shortcoming is that you need to explicitly specify some of the
organization of your project, such as for instance the directory tree
depth, whether some directories are found in a separate tree, etc.

The second method is the most general one. It requires an external
program, called ``find``, which is standard on all Unix systems. All
the directories found under a given root directory will be added to the
list.

.. code-block:: makefile

  # The examples below are based on the following directory hierarchy:
  # All the directories can contain any number of files
  # ROOT_DIRECTORY ->  a  ->  aa  ->  aaa
  #                       ->  ab
  #                       ->  ac
  #                ->  b  ->  ba  ->  baa
  #                       ->  bb
  #                       ->  bc
  # This Makefile creates a variable called DIRS, that can be reused any time
  # you need this list (see the other examples in this section)

  # The root of your project's directory hierarchy
  ROOT_DIRECTORY=.

  ####
  # First method: specify explicitly the list of directories
  # This allows you to specify any subset of all the directories you need.
  ####

  DIRS := a/aa/ a/ab/ b/ba/

  ####
  # Second method: use wildcards
  # Note that the argument(s) to wildcard below should end with a '/'.
  # Since wildcards also return file names, we have to filter them out
  # to avoid duplicate directory names.
  # We thus use make's ``dir`` and ``sort`` functions.
  # It sets DIRs to the following value (note that the directories aaa and baa
  # are not given, unless you change the arguments to wildcard).
  # DIRS= ./a/a/ ./b/ ./a/aa/ ./a/ab/ ./a/ac/ ./b/ba/ ./b/bb/ ./b/bc/
  ####

  DIRS := ${sort ${dir ${wildcard ${ROOT_DIRECTORY}/*/
                      ${ROOT_DIRECTORY}/*/*/}}}

  ####
  # Third method: use an external program
  # This command is much faster if run on local disks, avoiding NFS slowdowns.
  # This is the most complete command: it sets DIRs to the following value:
  # DIRS= ./a ./a/aa ./a/aa/aaa ./a/ab ./a/ac ./b ./b/ba ./b/ba/baa ./b/bb ./b/bc
  ####

  DIRS := ${shell find ${ROOT_DIRECTORY} -type d -print}



.. _Generating_the_Command_Line_Switches:

Generating the Command Line Switches
------------------------------------

Once you have created the list of directories as explained in the
previous section (:ref:`Automatically_Creating_a_List_of_Directories`),
you can easily generate the command line arguments to pass to gnatmake.

For the sake of completeness, this example assumes that the source path
is not the same as the object path, and that you have two separate lists
of directories.

.. code-block:: makefile

  # see "Automatically creating a list of directories" to create
  # these variables
  SOURCE_DIRS=
  OBJECT_DIRS=

  GNATMAKE_SWITCHES := ${patsubst %,-aI%,${SOURCE_DIRS}}
  GNATMAKE_SWITCHES += ${patsubst %,-aO%,${OBJECT_DIRS}}

  all:
          gnatmake ${GNATMAKE_SWITCHES} main_unit


.. _Overcoming_Command_Line_Length_Limits:

Overcoming Command Line Length Limits
-------------------------------------

One problem that might be encountered on big projects is that many
operating systems limit the length of the command line. It is thus hard to give
gnatmake the list of source and object directories.

This example shows how you can set up environment variables, which will
make ``gnatmake`` behave exactly as if the directories had been
specified on the command line, but have a much higher length limit (or
even none on most systems).

It assumes that you have created a list of directories in your Makefile,
using one of the methods presented in
:ref:`Automatically_Creating_a_List_of_Directories`.
For the sake of completeness, we assume that the object
path (where the ALI files are found) is different from the sources patch.

Note a small trick in the Makefile below: for efficiency reasons, we
create two temporary variables (SOURCE_LIST and OBJECT_LIST), that are
expanded immediately by ``make``. This way we overcome the standard
make behavior which is to expand the variables only when they are
actually used.

On Windows, if you are using the standard Windows command shell, you must
replace colons with semicolons in the assignments to these variables.

.. code-block:: makefile

  # In this example, we create both ADA_INCLUDE_PATH and ADA_OBJECTS_PATH.
  # This is the same thing as putting the -I arguments on the command line.
  # (the equivalent of using -aI on the command line would be to define
  #  only ADA_INCLUDE_PATH, the equivalent of -aO is ADA_OBJECTS_PATH).
  # You can of course have different values for these variables.
  #
  # Note also that we need to keep the previous values of these variables, since
  # they might have been set before running 'make' to specify where the GNAT
  # library is installed.

  # see "Automatically creating a list of directories" to create these
  # variables
  SOURCE_DIRS=
  OBJECT_DIRS=

  empty:=
  space:=${empty} ${empty}
  SOURCE_LIST := ${subst ${space},:,${SOURCE_DIRS}}
  OBJECT_LIST := ${subst ${space},:,${OBJECT_DIRS}}
  ADA_INCLUDE_PATH += ${SOURCE_LIST}
  ADA_OBJECTS_PATH += ${OBJECT_LIST}
  export ADA_INCLUDE_PATH
  export ADA_OBJECTS_PATH

  all:
          gnatmake main_unit
