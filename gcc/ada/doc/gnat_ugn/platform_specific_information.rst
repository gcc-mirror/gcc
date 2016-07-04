.. -- Non-breaking space in running text
   -- E.g. Ada |nbsp| 95

.. |nbsp| unicode:: 0xA0
   :trim:

.. _Platform_Specific_Information:

*****************************
Platform-Specific Information
*****************************

This appendix contains information relating to the implementation
of run-time libraries on various platforms and also covers
topics related to the GNAT implementation on Windows and Mac OS.

.. _`Run_Time_Libraries`:

Run-Time Libraries
==================

.. index:: Tasking and threads libraries
.. index:: Threads libraries and tasking
.. index:: Run-time libraries (platform-specific information)

The GNAT run-time implementation may vary with respect to both the
underlying threads library and the exception handling scheme.
For threads support, one or more of the following are supplied:

* **native threads library**, a binding to the thread package from
  the underlying operating system

* **pthreads library** (Sparc Solaris only), a binding to the Solaris
  POSIX thread package

For exception handling, either or both of two models are supplied:

  .. index:: Zero-Cost Exceptions
  .. index:: ZCX (Zero-Cost Exceptions)

* **Zero-Cost Exceptions** ("ZCX"),
  which uses binder-generated tables that
  are interrogated at run time to locate a handler.

  .. index:: setjmp/longjmp Exception Model
  .. index:: SJLJ (setjmp/longjmp Exception Model)

* **setjmp / longjmp** ('SJLJ'),
  which uses dynamically-set data to establish
  the set of handlers

Most programs should experience a substantial speed improvement by
being compiled with a ZCX run-time.
This is especially true for
tasking applications or applications with many exception handlers.}

This section summarizes which combinations of threads and exception support
are supplied on various GNAT platforms.
It then shows how to select a particular library either
permanently or temporarily,
explains the properties of (and tradeoffs among) the various threads
libraries, and provides some additional
information about several specific platforms.

.. _Summary_of_Run-Time_Configurations:

Summary of Run-Time Configurations
----------------------------------

+-----------------+--------------+-------------------------+------------+
| Platform        | Run-Time     | Tasking                 | Exceptions |
+=================+==============+=========================+============+
| ppc-aix         | rts-native   | native AIX threads      | ZCX        |
|                 | (default)    |                         |            |
|                 +--------------+-------------------------+------------+
|                 | rts-sjlj     | native AIX threads      | SJLJ       |
+-----------------+--------------+-------------------------+------------+
| sparc-solaris   | rts-native   | native Solaris          | ZCX        |
|                 | (default)    | threads library         |            |
|                 +--------------+-------------------------+------------+
|                 | rts-pthread  | pthread library         | ZCX        |
|                 +--------------+-------------------------+------------+
|                 | rts-sjlj     | native Solaris          | SJLJ       |
|                 |              | threads library         |            |
+-----------------+--------------+-------------------------+------------+
| sparc64-solaris | rts-native   | native Solaris          | ZCX        |
|                 | (default)    | threads library         |            |
+-----------------+--------------+-------------------------+------------+
| x86-linux       | rts-native   | pthread library         | ZCX        |
|                 | (default)    |                         |            |
|                 +--------------+-------------------------+------------+
|                 | rts-sjlj     | pthread library         | SJLJ       |
+-----------------+--------------+-------------------------+------------+
| x86-lynx        | rts-native   | native LynxOS threads   | SJLJ       |
|                 | (default)    |                         |            |
+-----------------+--------------+-------------------------+------------+
| x86-solaris     | rts-native   | native Solaris          | ZCX        |
|                 | (default)    | threads library         |            |
|                 +--------------+-------------------------+------------+
|                 | rts-sjlj     | native Solaris          | SJLJ       |
|                 |              | threads library         |            |
+-----------------+--------------+-------------------------+------------+
| x86-windows     | rts-native   | native Win32 threads    | ZCX        |
|                 | (default)    |                         |            |
|                 +--------------+-------------------------+------------+
|                 | rts-sjlj     | native Win32 threads    | SJLJ       |
+-----------------+--------------+-------------------------+------------+
| x86_64-linux    | rts-native   | pthread library         | ZCX        |
|                 | (default)    |                         |            |
|                 +--------------+-------------------------+------------+
|                 | rts-sjlj     | pthread library         | SJLJ       |
+-----------------+--------------+-------------------------+------------+


.. _Specifying_a_Run-Time_Library:

Specifying a Run-Time Library
=============================

The :file:`adainclude` subdirectory containing the sources of the GNAT
run-time library, and the :file:`adalib` subdirectory containing the
:file:`ALI` files and the static and/or shared GNAT library, are located
in the gcc target-dependent area:


  ::

      target=$prefix/lib/gcc/gcc-*dumpmachine*/gcc-*dumpversion*/

As indicated above, on some platforms several run-time libraries are supplied.
These libraries are installed in the target dependent area and
contain a complete source and binary subdirectory. The detailed description
below explains the differences between the different libraries in terms of
their thread support.

The default run-time library (when GNAT is installed) is *rts-native*.
This default run time is selected by the means of soft links.
For example on x86-linux::

   --
   --  $(target-dir)
   --      |
   --      +--- adainclude----------+
   --      |                        |
   --      +--- adalib-----------+  |
   --      |                     |  |
   --      +--- rts-native       |  |
   --      |    |                |  |
   --      |    +--- adainclude <---+
   --      |    |                |
   --      |    +--- adalib <----+
   --      |
   --      +--- rts-sjlj
   --           |
   --           +--- adainclude
   --           |
   --           +--- adalib

.. only:: html or latex

  .. image:: rtlibrary-structure.png

.. only:: not (html or latex) 

   ::

                      $(target-dir)
                     __/ /      \ \___
             _______/   /        \    \_________________
            /          /          \                     \
           /          /            \                     \
       ADAINCLUDE  ADALIB      rts-native             rts-sjlj
          :          :            /    \                 /   \
          :          :           /      \               /     \
          :          :          /        \             /       \
          :          :         /          \           /         \
          +-------------> adainclude     adalib   adainclude   adalib
                     :                     ^
                     :                     :
                     +---------------------+

                     Run-Time Library Directory Structure
          (Upper-case names and dotted/dashed arrows represent soft links)

If the *rts-sjlj* library is to be selected on a permanent basis,
these soft links can be modified with the following commands:

  ::

    $ cd $target
    $ rm -f adainclude adalib
    $ ln -s rts-sjlj/adainclude adainclude
    $ ln -s rts-sjlj/adalib adalib

Alternatively, you can specify :file:`rts-sjlj/adainclude` in the file
:file:`$target/ada_source_path` and :file:`rts-sjlj/adalib` in
:file:`$target/ada_object_path`.

.. index:: --RTS option

Selecting another run-time library temporarily can be
achieved by using the *--RTS* switch, e.g., *--RTS=sjlj*


.. _Choosing_the_Scheduling_Policy:

.. index:: SCHED_FIFO scheduling policy
.. index:: SCHED_RR scheduling policy
.. index:: SCHED_OTHER scheduling policy

Choosing the Scheduling Policy
------------------------------

When using a POSIX threads implementation, you have a choice of several
scheduling policies: `SCHED_FIFO`, `SCHED_RR` and `SCHED_OTHER`.

Typically, the default is `SCHED_OTHER`, while using `SCHED_FIFO`
or `SCHED_RR` requires special (e.g., root) privileges.

.. index:: pragma Time_Slice
.. index:: -T0 option
.. index:: pragma Task_Dispatching_Policy


By default, GNAT uses the `SCHED_OTHER` policy. To specify
`SCHED_FIFO`,
you can use one of the following:

* `pragma Time_Slice (0.0)`
* the corresponding binder option *-T0*
* `pragma Task_Dispatching_Policy (FIFO_Within_Priorities)`


To specify `SCHED_RR`,
you should use `pragma Time_Slice` with a
value greater than 0.0, or else use the corresponding *-T*
binder option.


.. index:: Solaris Sparc threads libraries

.. _Solaris-Specific_Considerations:

Solaris-Specific Considerations
-------------------------------

This section addresses some topics related to the various threads libraries
on Sparc Solaris.

.. index:: rts-pthread threads library

.. _Solaris_Threads_Issues:

Solaris Threads Issues
----------------------

GNAT under Solaris/Sparc 32 bits comes with an alternate tasking run-time
library based on POSIX threads --- *rts-pthread*.

.. index:: PTHREAD_PRIO_INHERIT policy (under rts-pthread)
.. index:: PTHREAD_PRIO_PROTECT policy (under rts-pthread)
.. index:: pragma Locking_Policy (under rts-pthread)
.. index:: Inheritance_Locking (under rts-pthread)
.. index:: Ceiling_Locking (under rts-pthread)

This run-time library has the advantage of being mostly shared across all
POSIX-compliant thread implementations, and it also provides under
Solaris |nbsp| 8 the `PTHREAD_PRIO_INHERIT`
and `PTHREAD_PRIO_PROTECT`
semantics that can be selected using the predefined pragma
`Locking_Policy`
with respectively
`Inheritance_Locking` and `Ceiling_Locking` as the policy.

As explained above, the native run-time library is based on the Solaris thread
library (`libthread`) and is the default library.

.. index:: GNAT_PROCESSOR environment variable (on Sparc Solaris)

When the Solaris threads library is used (this is the default), programs
compiled with GNAT can automatically take advantage of
and can thus execute on multiple processors.
The user can alternatively specify a processor on which the program should run
to emulate a single-processor system. The multiprocessor / uniprocessor choice
is made by
setting the environment variable :envvar:`GNAT_PROCESSOR`
to one of the following:

  ========================= ===================================================================
  ``GNAT_PROCESSOR`` Value             Effect
  ========================= ===================================================================
  *-2*                      Use the default configuration (run the program on all
                            available processors) - this is the same as having `GNAT_PROCESSOR`
                            unset
  *-1*                      Let the run-time implementation choose one processor and run the
                            program on that processor
  *0 .. Last_Proc*          Run the program on the specified processor.
                            `Last_Proc` is equal to `_SC_NPROCESSORS_CONF - 1`
                            (where `_SC_NPROCESSORS_CONF` is a system variable).
  ========================= ===================================================================


.. _AIX-Specific_Considerations:

AIX-Specific Considerations
---------------------------

.. index:: AIX resolver library

On AIX, the resolver library initializes some internal structure on
the first call to `get*by*` functions, which are used to implement
`GNAT.Sockets.Get_Host_By_Name` and
`GNAT.Sockets.Get_Host_By_Address`.
If such initialization occurs within an Ada task, and the stack size for
the task is the default size, a stack overflow may occur.

To avoid this overflow, the user should either ensure that the first call
to `GNAT.Sockets.Get_Host_By_Name` or
`GNAT.Sockets.Get_Host_By_Addrss`
occurs in the environment task, or use `pragma Storage_Size` to
specify a sufficiently large size for the stack of the task that contains
this call.


.. index:: Windows NT
.. index:: Windows 95
.. index:: Windows 98

.. _Microsoft_Windows_Topics:

Microsoft Windows Topics
========================

This section describes topics that are specific to the Microsoft Windows
platforms.


.. only:: PRO or GPL

  .. rubric:: Installing from the Command Line

  By default the GNAT installers display a GUI that prompts you to enter
  the installation path and similar information, and then guides you through the
  installation process. It is also possible to perform silent installations
  using the command-line interface.

  In order to install one of the GNAT installers from the command
  line you should pass parameter `/S` (and, optionally,
  `/D=<directory>`) as command-line arguments.

.. only:: PRO

   For example, for an unattended installation of
   GNAT 7.0.2 into the default directory
   `C:\\GNATPRO\\7.0.2` you would run:

     ::

        gnatpro-7.0.2-i686-pc-mingw32-bin.exe /S

   To install into a custom directory, say, `C:\\TOOLS\\GNATPRO\\7.0.2`:

     ::

        gnatpro-7.0.2-i686-pc-mingw32-bin /S /D=C:\TOOLS\GNATPRO\7.0.2

.. only:: GPL

   For example, for an unattended installation of
   GNAT 2012 into `C:\\GNAT\\2012`:

     ::

        gnat-gpl-2012-i686-pc-mingw32-bin /S /D=C:\GNAT\2012

.. only:: PRO or GPL

   You can use the same syntax for all installers.

   Note that unattended installations don't modify system path, nor create file
   associations, so such activities need to be done by hand.



.. _Using_GNAT_on_Windows:

Using GNAT on Windows
---------------------

One of the strengths of the GNAT technology is that its tool set
(*gcc*, *gnatbind*, *gnatlink*, *gnatmake*, the
`gdb` debugger, etc.) is used in the same way regardless of the
platform.

On Windows this tool set is complemented by a number of Microsoft-specific
tools that have been provided to facilitate interoperability with Windows
when this is required. With these tools:


* You can build applications using the `CONSOLE` or `WINDOWS`
  subsystems.

* You can use any Dynamically Linked Library (DLL) in your Ada code (both
  relocatable and non-relocatable DLLs are supported).

* You can build Ada DLLs for use in other applications. These applications
  can be written in a language other than Ada (e.g., C, C++, etc). Again both
  relocatable and non-relocatable Ada DLLs are supported.

* You can include Windows resources in your Ada application.

* You can use or create COM/DCOM objects.

Immediately below are listed all known general GNAT-for-Windows restrictions.
Other restrictions about specific features like Windows Resources and DLLs
are listed in separate sections below.


* It is not possible to use `GetLastError` and `SetLastError`
  when tasking, protected records, or exceptions are used. In these
  cases, in order to implement Ada semantics, the GNAT run-time system
  calls certain Win32 routines that set the last error variable to 0 upon
  success. It should be possible to use `GetLastError` and
  `SetLastError` when tasking, protected record, and exception
  features are not used, but it is not guaranteed to work.

* It is not possible to link against Microsoft C++ libraries except for
  import libraries. Interfacing must be done by the mean of DLLs.

* It is possible to link against Microsoft C libraries. Yet the preferred
  solution is to use C/C++ compiler that comes with GNAT, since it
  doesn't require having two different development environments and makes the
  inter-language debugging experience smoother.

* When the compilation environment is located on FAT32 drives, users may
  experience recompilations of the source files that have not changed if
  Daylight Saving Time (DST) state has changed since the last time files
  were compiled. NTFS drives do not have this problem.

* No components of the GNAT toolset use any entries in the Windows
  registry. The only entries that can be created are file associations and
  PATH settings, provided the user has chosen to create them at installation
  time, as well as some minimal book-keeping information needed to correctly
  uninstall or integrate different GNAT products.


.. _Using_a_network_installation_of_GNAT:

Using a network installation of GNAT
------------------------------------

Make sure the system on which GNAT is installed is accessible from the
current machine, i.e., the install location is shared over the network.
Shared resources are accessed on Windows by means of UNC paths, which
have the format `\\\\server\\sharename\\path`

In order to use such a network installation, simply add the UNC path of the
:file:`bin` directory of your GNAT installation in front of your PATH. For
example, if GNAT is installed in :file:`\\GNAT` directory of a share location
called :file:`c-drive` on a machine :file:`LOKI`, the following command will
make it available:

  ::

      $ path \\loki\c-drive\gnat\bin;%path%`

Be aware that every compilation using the network installation results in the
transfer of large amounts of data across the network and will likely cause
serious performance penalty.

.. _CONSOLE_and_WINDOWS_subsystems:

CONSOLE and WINDOWS subsystems
------------------------------

.. index:: CONSOLE Subsystem
.. index:: WINDOWS Subsystem
.. index:: -mwindows

There are two main subsystems under Windows. The `CONSOLE` subsystem
(which is the default subsystem) will always create a console when
launching the application. This is not something desirable when the
application has a Windows GUI. To get rid of this console the
application must be using the `WINDOWS` subsystem. To do so
the *-mwindows* linker option must be specified.

   ::

      $ gnatmake winprog -largs -mwindows

.. _Temporary_Files:

Temporary Files
---------------

.. index:: Temporary files

It is possible to control where temporary files gets created by setting
the :envvar:`TMP` environment variable. The file will be created:

* Under the directory pointed to by the :envvar:`TMP` environment variable if
  this directory exists.

* Under :file:`c:\\temp`, if the :envvar:`TMP` environment variable is not
  set (or not pointing to a directory) and if this directory exists.

* Under the current working directory otherwise.

This allows you to determine exactly where the temporary
file will be created. This is particularly useful in networked
environments where you may not have write access to some
directories.

Disabling Command Line Argument Expansion
-----------------------------------------

.. index:: Command Line Argument Expansion

By default, an executable compiled for the **Windows** platform will do
the following postprocessing on the arguments passed on the command
line:

* If the argument contains the characters ``*`` and/or ``?``, then
  file expansion will be attempted. For example, if the current directory
  contains :file:`a.txt` and :file:`b.txt`, then when calling::

      $ my_ada_program *.txt

  The following arguments will effectively be passed to the main program
  (for example when using ``Ada.Command_Line.Argument``)::

      Ada.Command_Line.Argument (1) -> "a.txt"
      Ada.Command_Line.Argument (2) -> "b.txt"

* Filename expansion can be disabled for a given argument by using single
  quotes. Thus, calling::

      $ my_ada_program '*.txt'

  will result in::

      Ada.Command_Line.Argument (1) -> "*.txt"

Note that if the program is launched from a shell such as **Cygwin** **Bash**
then quote removal might be performed by the shell.

In some contexts it might be useful to disable this feature (for example if
the program performs its own argument expansion). In order to do this, a C
symbol needs to be defined and set to ``0``. You can do this by
adding the following code fragment in one of your **Ada** units:

.. code-block:: ada

   Do_Argv_Expansion : Integer := 0;
   pragma Export (C, Do_Argv_Expansion, "__gnat_do_argv_expansion");

The results of previous examples will be respectively::

   Ada.Command_Line.Argument (1) -> "*.txt"

and::

   Ada.Command_Line.Argument (1) -> "'*.txt'"

   
.. _Mixed-Language_Programming_on_Windows:

Mixed-Language Programming on Windows
-------------------------------------

Developing pure Ada applications on Windows is no different than on
other GNAT-supported platforms. However, when developing or porting an
application that contains a mix of Ada and C/C++, the choice of your
Windows C/C++ development environment conditions your overall
interoperability strategy.

If you use *gcc* or Microsoft C to compile the non-Ada part of
your application, there are no Windows-specific restrictions that
affect the overall interoperability with your Ada code. If you do want
to use the Microsoft tools for your C++ code, you have two choices:

* Encapsulate your C++ code in a DLL to be linked with your Ada
  application. In this case, use the Microsoft or whatever environment to
  build the DLL and use GNAT to build your executable
  (:ref:`Using_DLLs_with_GNAT`).

* Or you can encapsulate your Ada code in a DLL to be linked with the
  other part of your application. In this case, use GNAT to build the DLL
  (:ref:`Building_DLLs_with_GNAT_Project_files`) and use the Microsoft
  or whatever environment to build your executable.

In addition to the description about C main in
:ref:`Mixed_Language_Programming` section, if the C main uses a
stand-alone library it is required on x86-windows to
setup the SEH context. For this the C main must looks like this:


  .. code-block:: c

      /* main.c */
      extern void adainit (void);
      extern void adafinal (void);
      extern void __gnat_initialize(void*);
      extern void call_to_ada (void);

      int main (int argc, char *argv[])
      {
        int SEH [2];

        /* Initialize the SEH context */
        __gnat_initialize (&SEH);

        adainit();

        /* Then call Ada services in the stand-alone library */

        call_to_ada();

        adafinal();
      }

Note that this is not needed on x86_64-windows where the Windows
native SEH support is used.


.. _Windows_Calling_Conventions:

Windows Calling Conventions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Stdcall
.. index:: APIENTRY

This section pertain only to Win32. On Win64 there is a single native
calling convention. All convention specifiers are ignored on this
platform.

When a subprogram `F` (caller) calls a subprogram `G`
(callee), there are several ways to push `G`'s parameters on the
stack and there are several possible scenarios to clean up the stack
upon `G`'s return. A calling convention is an agreed upon software
protocol whereby the responsibilities between the caller (`F`) and
the callee (`G`) are clearly defined. Several calling conventions
are available for Windows:

* `C` (Microsoft defined)

* `Stdcall` (Microsoft defined)

* `Win32` (GNAT specific)

* `DLL` (GNAT specific)


.. _C_Calling_Convention:

`C` Calling Convention
""""""""""""""""""""""

This is the default calling convention used when interfacing to C/C++
routines compiled with either *gcc* or Microsoft Visual C++.

In the `C` calling convention subprogram parameters are pushed on the
stack by the caller from right to left. The caller itself is in charge of
cleaning up the stack after the call. In addition, the name of a routine
with `C` calling convention is mangled by adding a leading underscore.

The name to use on the Ada side when importing (or exporting) a routine
with `C` calling convention is the name of the routine. For
instance the C function:

   ::

       int get_val (long);

should be imported from Ada as follows:

  .. code-block:: ada

     function Get_Val (V : Interfaces.C.long) return Interfaces.C.int;
     pragma Import (C, Get_Val, External_Name => "get_val");

Note that in this particular case the `External_Name` parameter could
have been omitted since, when missing, this parameter is taken to be the
name of the Ada entity in lower case. When the `Link_Name` parameter
is missing, as in the above example, this parameter is set to be the
`External_Name` with a leading underscore.

When importing a variable defined in C, you should always use the `C`
calling convention unless the object containing the variable is part of a
DLL (in which case you should use the `Stdcall` calling
convention, :ref:`Stdcall_Calling_Convention`).


.. _Stdcall_Calling_Convention:

`Stdcall` Calling Convention
""""""""""""""""""""""""""""

This convention, which was the calling convention used for Pascal
programs, is used by Microsoft for all the routines in the Win32 API for
efficiency reasons. It must be used to import any routine for which this
convention was specified.

In the `Stdcall` calling convention subprogram parameters are pushed
on the stack by the caller from right to left. The callee (and not the
caller) is in charge of cleaning the stack on routine exit. In addition,
the name of a routine with `Stdcall` calling convention is mangled by
adding a leading underscore (as for the `C` calling convention) and a
trailing :samp:`@{nn}`, where `nn` is the overall size (in
bytes) of the parameters passed to the routine.

The name to use on the Ada side when importing a C routine with a
`Stdcall` calling convention is the name of the C routine. The leading
underscore and trailing :samp:`@{nn}` are added automatically by
the compiler. For instance the Win32 function:

  ::

      APIENTRY int get_val (long);

should be imported from Ada as follows:

  .. code-block:: ada

     function Get_Val (V : Interfaces.C.long) return Interfaces.C.int;
     pragma Import (Stdcall, Get_Val);
     --  On the x86 a long is 4 bytes, so the Link_Name is "_get_val@4"

As for the `C` calling convention, when the `External_Name`
parameter is missing, it is taken to be the name of the Ada entity in lower
case. If instead of writing the above import pragma you write:

  .. code-block:: ada

     function Get_Val (V : Interfaces.C.long) return Interfaces.C.int;
     pragma Import (Stdcall, Get_Val, External_Name => "retrieve_val");

then the imported routine is `_retrieve_val@4`. However, if instead
of specifying the `External_Name` parameter you specify the
`Link_Name` as in the following example:

  .. code-block:: ada

     function Get_Val (V : Interfaces.C.long) return Interfaces.C.int;
     pragma Import (Stdcall, Get_Val, Link_Name => "retrieve_val");

then the imported routine is `retrieve_val`, that is, there is no
decoration at all. No leading underscore and no Stdcall suffix
:samp:`@{nn}`.

This is especially important as in some special cases a DLL's entry
point name lacks a trailing :samp:`@{nn}` while the exported
name generated for a call has it.

It is also possible to import variables defined in a DLL by using an
import pragma for a variable. As an example, if a DLL contains a
variable defined as:

  .. code-block:: c

     int my_var;

then, to access this variable from Ada you should write:

  .. code-block:: ada

      My_Var : Interfaces.C.int;
      pragma Import (Stdcall, My_Var);

Note that to ease building cross-platform bindings this convention
will be handled as a `C` calling convention on non-Windows platforms.


.. _Win32_Calling_Convention:

`Win32` Calling Convention
""""""""""""""""""""""""""

This convention, which is GNAT-specific is fully equivalent to the
`Stdcall` calling convention described above.


.. _DLL_Calling_Convention:

`DLL` Calling Convention
""""""""""""""""""""""""

This convention, which is GNAT-specific is fully equivalent to the
`Stdcall` calling convention described above.


.. _Introduction_to_Dynamic_Link_Libraries_DLLs:

Introduction to Dynamic Link Libraries (DLLs)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: DLL

A Dynamically Linked Library (DLL) is a library that can be shared by
several applications running under Windows. A DLL can contain any number of
routines and variables.

One advantage of DLLs is that you can change and enhance them without
forcing all the applications that depend on them to be relinked or
recompiled. However, you should be aware than all calls to DLL routines are
slower since, as you will understand below, such calls are indirect.

To illustrate the remainder of this section, suppose that an application
wants to use the services of a DLL :file:`API.dll`. To use the services
provided by :file:`API.dll` you must statically link against the DLL or
an import library which contains a jump table with an entry for each
routine and variable exported by the DLL. In the Microsoft world this
import library is called :file:`API.lib`. When using GNAT this import
library is called either :file:`libAPI.dll.a`, :file:`libapi.dll.a`,
:file:`libAPI.a` or :file:`libapi.a` (names are case insensitive).

After you have linked your application with the DLL or the import library
and you run your application, here is what happens:

* Your application is loaded into memory.

* The DLL :file:`API.dll` is mapped into the address space of your
  application. This means that:

  - The DLL will use the stack of the calling thread.

  - The DLL will use the virtual address space of the calling process.

  - The DLL will allocate memory from the virtual address space of the calling
    process.

  - Handles (pointers) can be safely exchanged between routines in the DLL
    routines and routines in the application using the DLL.

* The entries in the jump table (from the import library :file:`libAPI.dll.a`
  or :file:`API.lib` or automatically created when linking against a DLL)
  which is part of your application are initialized with the addresses
  of the routines and variables in :file:`API.dll`.

* If present in :file:`API.dll`, routines `DllMain` or
  `DllMainCRTStartup` are invoked. These routines typically contain
  the initialization code needed for the well-being of the routines and
  variables exported by the DLL.

There is an additional point which is worth mentioning. In the Windows
world there are two kind of DLLs: relocatable and non-relocatable
DLLs. Non-relocatable DLLs can only be loaded at a very specific address
in the target application address space. If the addresses of two
non-relocatable DLLs overlap and these happen to be used by the same
application, a conflict will occur and the application will run
incorrectly. Hence, when possible, it is always preferable to use and
build relocatable DLLs. Both relocatable and non-relocatable DLLs are
supported by GNAT. Note that the *-s* linker option (see GNU Linker
User's Guide) removes the debugging symbols from the DLL but the DLL can
still be relocated.

As a side note, an interesting difference between Microsoft DLLs and
Unix shared libraries, is the fact that on most Unix systems all public
routines are exported by default in a Unix shared library, while under
Windows it is possible (but not required) to list exported routines in
a definition file (see :ref:`The Definition File <The_Definition_File>`).


.. _Using_DLLs_with_GNAT:

Using DLLs with GNAT
^^^^^^^^^^^^^^^^^^^^

To use the services of a DLL, say :file:`API.dll`, in your Ada application
you must have:

* The Ada spec for the routines and/or variables you want to access in
  :file:`API.dll`. If not available this Ada spec must be built from the C/C++
  header files provided with the DLL.

* The import library (:file:`libAPI.dll.a` or :file:`API.lib`). As previously
  mentioned an import library is a statically linked library containing the
  import table which will be filled at load time to point to the actual
  :file:`API.dll` routines. Sometimes you don't have an import library for the
  DLL you want to use. The following sections will explain how to build
  one. Note that this is optional.

* The actual DLL, :file:`API.dll`.

Once you have all the above, to compile an Ada application that uses the
services of :file:`API.dll` and whose main subprogram is `My_Ada_App`,
you simply issue the command

  ::

      $ gnatmake my_ada_app -largs -lAPI

The argument *-largs -lAPI* at the end of the *gnatmake* command
tells the GNAT linker to look for an import library. The linker will
look for a library name in this specific order:

* :file:`libAPI.dll.a`
* :file:`API.dll.a`
* :file:`libAPI.a`
* :file:`API.lib`
* :file:`libAPI.dll`
* :file:`API.dll`

The first three are the GNU style import libraries. The third is the
Microsoft style import libraries. The last two are the actual DLL names.

Note that if the Ada package spec for :file:`API.dll` contains the
following pragma

  .. code-block:: ada

      pragma Linker_Options ("-lAPI");

you do not have to add *-largs -lAPI* at the end of the
*gnatmake* command.

If any one of the items above is missing you will have to create it
yourself. The following sections explain how to do so using as an
example a fictitious DLL called :file:`API.dll`.


.. _Creating_an_Ada_Spec_for_the_DLL_Services:

Creating an Ada Spec for the DLL Services
"""""""""""""""""""""""""""""""""""""""""

A DLL typically comes with a C/C++ header file which provides the
definitions of the routines and variables exported by the DLL. The Ada
equivalent of this header file is a package spec that contains definitions
for the imported entities. If the DLL you intend to use does not come with
an Ada spec you have to generate one such spec yourself. For example if
the header file of :file:`API.dll` is a file :file:`api.h` containing the
following two definitions:

  .. code-block:: c

      int some_var;
      int get (char *);

then the equivalent Ada spec could be:

  .. code-block:: ada

      with Interfaces.C.Strings;
      package API is
         use Interfaces;

         Some_Var : C.int;
         function Get (Str : C.Strings.Chars_Ptr) return C.int;

      private
         pragma Import (C, Get);
         pragma Import (DLL, Some_Var);
      end API;


.. _Creating_an_Import_Library:

Creating an Import Library
""""""""""""""""""""""""""

.. index:: Import library

If a Microsoft-style import library :file:`API.lib` or a GNAT-style
import library :file:`libAPI.dll.a` or :file:`libAPI.a` is available
with :file:`API.dll` you can skip this section. You can also skip this
section if :file:`API.dll` or :file:`libAPI.dll` is built with GNU tools
as in this case it is possible to link directly against the
DLL. Otherwise read on.


.. index:: Definition file

.. _The_Definition_File:

.. rubric:: The Definition File

As previously mentioned, and unlike Unix systems, the list of symbols
that are exported from a DLL must be provided explicitly in Windows.
The main goal of a definition file is precisely that: list the symbols
exported by a DLL. A definition file (usually a file with a `.def`
suffix) has the following structure:

  ::

      [LIBRARY `name`]
      [DESCRIPTION `string`]
      EXPORTS
         `symbol1`
         `symbol2`
         ...

*LIBRARY `name`*
  This section, which is optional, gives the name of the DLL.


*DESCRIPTION `string`*
  This section, which is optional, gives a description string that will be
  embedded in the import library.


*EXPORTS*
  This section gives the list of exported symbols (procedures, functions or
  variables). For instance in the case of :file:`API.dll` the `EXPORTS`
  section of :file:`API.def` looks like:

  ::

      EXPORTS
         some_var
         get

Note that you must specify the correct suffix (:samp:`@{nn}`)
(see :ref:`Windows_Calling_Conventions`) for a Stdcall
calling convention function in the exported symbols list.

There can actually be other sections in a definition file, but these
sections are not relevant to the discussion at hand.


.. _Create_Def_File_Automatically:

.. rubric:: Creating a Definition File Automatically

You can automatically create the definition file :file:`API.def`
(see :ref:`The Definition File <The_Definition_File>`) from a DLL.
For that use the `dlltool` program as follows:

  ::

      $ dlltool API.dll -z API.def --export-all-symbols

  Note that if some routines in the DLL have the `Stdcall` convention
  (:ref:`Windows_Calling_Conventions`) with stripped :samp:`@{nn}`
  suffix then you'll have to edit :file:`api.def` to add it, and specify
  *-k* to *gnatdll* when creating the import library.

  Here are some hints to find the right :samp:`@{nn}` suffix.

  - If you have the Microsoft import library (.lib), it is possible to get
    the right symbols by using Microsoft `dumpbin` tool (see the
    corresponding Microsoft documentation for further details).

    ::

        $ dumpbin /exports api.lib

  - If you have a message about a missing symbol at link time the compiler
    tells you what symbol is expected. You just have to go back to the
    definition file and add the right suffix.


.. _GNAT-Style_Import_Library:

.. rubric:: GNAT-Style Import Library

To create a static import library from :file:`API.dll` with the GNAT tools
you should create the .def file, then use `gnatdll` tool
(see :ref:`Using_gnatdll`) as follows:

  ::

      $ gnatdll -e API.def -d API.dll

  `gnatdll` takes as input a definition file :file:`API.def` and the
  name of the DLL containing the services listed in the definition file
  :file:`API.dll`. The name of the static import library generated is
  computed from the name of the definition file as follows: if the
  definition file name is `xyz``.def`, the import library name will
  be `lib``xyz``.a`. Note that in the previous example option
  *-e* could have been removed because the name of the definition
  file (before the '`.def`' suffix) is the same as the name of the
  DLL (:ref:`Using_gnatdll` for more information about `gnatdll`).


.. _MSVS-Style_Import_Library:

.. rubric:: Microsoft-Style Import Library

A Microsoft import library is needed only if you plan to make an
Ada DLL available to applications developed with Microsoft
tools (:ref:`Mixed-Language_Programming_on_Windows`).

To create a Microsoft-style import library for :file:`API.dll` you
should create the .def file, then build the actual import library using
Microsoft's `lib` utility:

  ::

      $ lib -machine:IX86 -def:API.def -out:API.lib

  If you use the above command the definition file :file:`API.def` must
  contain a line giving the name of the DLL:

  ::

      LIBRARY      "API"

  See the Microsoft documentation for further details about the usage of
  `lib`.


.. _Building_DLLs_with_GNAT_Project_files:

Building DLLs with GNAT Project files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: DLLs, building

There is nothing specific to Windows in the build process.
See the *Library Projects* section in the *GNAT Project Manager*
chapter of the *GPRbuild User's Guide*.

Due to a system limitation, it is not possible under Windows to create threads
when inside the `DllMain` routine which is used for auto-initialization
of shared libraries, so it is not possible to have library level tasks in SALs.


.. _Building_DLLs_with_GNAT:

Building DLLs with GNAT
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: DLLs, building

This section explain how to build DLLs using the GNAT built-in DLL
support. With the following procedure it is straight forward to build
and use DLLs with GNAT.


* Building object files.
  The first step is to build all objects files that are to be included
  into the DLL. This is done by using the standard *gnatmake* tool.

* Building the DLL.
  To build the DLL you must use *gcc*'s *-shared* and
  *-shared-libgcc* options. It is quite simple to use this method:

  ::

      $ gcc -shared -shared-libgcc -o api.dll obj1.o obj2.o ...

  It is important to note that in this case all symbols found in the
  object files are automatically exported. It is possible to restrict
  the set of symbols to export by passing to *gcc* a definition
  file (see :ref:`The Definition File <The_Definition_File>`).
  For example:

  ::

      $ gcc -shared -shared-libgcc -o api.dll api.def obj1.o obj2.o ...

  If you use a definition file you must export the elaboration procedures
  for every package that required one. Elaboration procedures are named
  using the package name followed by "_E".

* Preparing DLL to be used.
  For the DLL to be used by client programs the bodies must be hidden
  from it and the .ali set with read-only attribute. This is very important
  otherwise GNAT will recompile all packages and will not actually use
  the code in the DLL. For example:

  ::

      $ mkdir apilib
      $ copy *.ads *.ali api.dll apilib
      $ attrib +R apilib\\*.ali

At this point it is possible to use the DLL by directly linking
against it. Note that you must use the GNAT shared runtime when using
GNAT shared libraries. This is achieved by using *-shared* binder's
option.

  ::

     $ gnatmake main -Iapilib -bargs -shared -largs -Lapilib -lAPI


.. _Building_DLLs_with_gnatdll:

Building DLLs with gnatdll
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: DLLs, building

Note that it is preferred to use GNAT Project files
(:ref:`Building_DLLs_with_GNAT_Project_files`) or the built-in GNAT
DLL support (:ref:`Building_DLLs_with_GNAT`) or to build DLLs.

This section explains how to build DLLs containing Ada code using
`gnatdll`. These DLLs will be referred to as Ada DLLs in the
remainder of this section.

The steps required to build an Ada DLL that is to be used by Ada as well as
non-Ada applications are as follows:

* You need to mark each Ada *entity* exported by the DLL with a `C` or
  `Stdcall` calling convention to avoid any Ada name mangling for the
  entities exported by the DLL
  (see :ref:`Exporting Ada Entities <Exporting_Ada_Entities>`). You can
  skip this step if you plan to use the Ada DLL only from Ada applications.

* Your Ada code must export an initialization routine which calls the routine
  `adainit` generated by *gnatbind* to perform the elaboration of
  the Ada code in the DLL (:ref:`Ada_DLLs_and_Elaboration`). The initialization
  routine exported by the Ada DLL must be invoked by the clients of the DLL
  to initialize the DLL.

* When useful, the DLL should also export a finalization routine which calls
  routine `adafinal` generated by *gnatbind* to perform the
  finalization of the Ada code in the DLL (:ref:`Ada_DLLs_and_Finalization`).
  The finalization routine exported by the Ada DLL must be invoked by the
  clients of the DLL when the DLL services are no further needed.

* You must provide a spec for the services exported by the Ada DLL in each
  of the programming languages to which you plan to make the DLL available.

* You must provide a definition file listing the exported entities
  (:ref:`The Definition File <The_Definition_File>`).

* Finally you must use `gnatdll` to produce the DLL and the import
  library (:ref:`Using_gnatdll`).

Note that a relocatable DLL stripped using the `strip`
binutils tool will not be relocatable anymore. To build a DLL without
debug information pass `-largs -s` to `gnatdll`. This
restriction does not apply to a DLL built using a Library Project.
See the *Library Projects* section in the *GNAT Project Manager*
chapter of the *GPRbuild User's Guide*.


.. Limitations_When_Using_Ada_DLLs_from Ada:

Limitations When Using Ada DLLs from Ada
""""""""""""""""""""""""""""""""""""""""

When using Ada DLLs from Ada applications there is a limitation users
should be aware of. Because on Windows the GNAT run time is not in a DLL of
its own, each Ada DLL includes a part of the GNAT run time. Specifically,
each Ada DLL includes the services of the GNAT run time that are necessary
to the Ada code inside the DLL. As a result, when an Ada program uses an
Ada DLL there are two independent GNAT run times: one in the Ada DLL and
one in the main program.

It is therefore not possible to exchange GNAT run-time objects between the
Ada DLL and the main Ada program. Example of GNAT run-time objects are file
handles (e.g., `Text_IO.File_Type`), tasks types, protected objects
types, etc.

It is completely safe to exchange plain elementary, array or record types,
Windows object handles, etc.


.. _Exporting_Ada_Entities:

Exporting Ada Entities
""""""""""""""""""""""

.. index:: Export table

Building a DLL is a way to encapsulate a set of services usable from any
application. As a result, the Ada entities exported by a DLL should be
exported with the `C` or `Stdcall` calling conventions to avoid
any Ada name mangling. As an example here is an Ada package
`API`, spec and body, exporting two procedures, a function, and a
variable:


  .. code-block:: ada

     with Interfaces.C; use Interfaces;
     package API is
        Count : C.int := 0;
        function Factorial (Val : C.int) return C.int;

        procedure Initialize_API;
        procedure Finalize_API;
        --  Initialization & Finalization routines. More in the next section.
     private
        pragma Export (C, Initialize_API);
        pragma Export (C, Finalize_API);
        pragma Export (C, Count);
        pragma Export (C, Factorial);
     end API;

  .. code-block:: ada

     package body API is
        function Factorial (Val : C.int) return C.int is
           Fact : C.int := 1;
        begin
           Count := Count + 1;
           for K in 1 .. Val loop
              Fact := Fact * K;
           end loop;
           return Fact;
        end Factorial;

        procedure Initialize_API is
           procedure Adainit;
           pragma Import (C, Adainit);
        begin
           Adainit;
        end Initialize_API;

        procedure Finalize_API is
           procedure Adafinal;
           pragma Import (C, Adafinal);
        begin
           Adafinal;
        end Finalize_API;
     end API;

If the Ada DLL you are building will only be used by Ada applications
you do not have to export Ada entities with a `C` or `Stdcall`
convention. As an example, the previous package could be written as
follows:

  .. code-block:: ada

     package API is
        Count : Integer := 0;
        function Factorial (Val : Integer) return Integer;

        procedure Initialize_API;
        procedure Finalize_API;
        --  Initialization and Finalization routines.
     end API;

  .. code-block:: ada

     package body API is
        function Factorial (Val : Integer) return Integer is
           Fact : Integer := 1;
        begin
           Count := Count + 1;
           for K in 1 .. Val loop
              Fact := Fact * K;
           end loop;
           return Fact;
        end Factorial;

        ...
        --  The remainder of this package body is unchanged.
     end API;

Note that if you do not export the Ada entities with a `C` or
`Stdcall` convention you will have to provide the mangled Ada names
in the definition file of the Ada DLL
(:ref:`Creating_the_Definition_File`).


.. _Ada_DLLs_and_Elaboration:

Ada DLLs and Elaboration
""""""""""""""""""""""""

.. index:: DLLs and elaboration

The DLL that you are building contains your Ada code as well as all the
routines in the Ada library that are needed by it. The first thing a
user of your DLL must do is elaborate the Ada code
(:ref:`Elaboration_Order_Handling_in_GNAT`).

To achieve this you must export an initialization routine
(`Initialize_API` in the previous example), which must be invoked
before using any of the DLL services. This elaboration routine must call
the Ada elaboration routine `adainit` generated by the GNAT binder
(:ref:`Binding_with_Non-Ada_Main_Programs`). See the body of
`Initialize_Api` for an example. Note that the GNAT binder is
automatically invoked during the DLL build process by the `gnatdll`
tool (:ref:`Using_gnatdll`).

When a DLL is loaded, Windows systematically invokes a routine called
`DllMain`. It would therefore be possible to call `adainit`
directly from `DllMain` without having to provide an explicit
initialization routine. Unfortunately, it is not possible to call
`adainit` from the `DllMain` if your program has library level
tasks because access to the `DllMain` entry point is serialized by
the system (that is, only a single thread can execute 'through' it at a
time), which means that the GNAT run time will deadlock waiting for the
newly created task to complete its initialization.


.. _Ada_DLLs_and_Finalization:

Ada DLLs and Finalization
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: DLLs and finalization

When the services of an Ada DLL are no longer needed, the client code should
invoke the DLL finalization routine, if available. The DLL finalization
routine is in charge of releasing all resources acquired by the DLL. In the
case of the Ada code contained in the DLL, this is achieved by calling
routine `adafinal` generated by the GNAT binder
(:ref:`Binding_with_Non-Ada_Main_Programs`).
See the body of `Finalize_Api` for an
example. As already pointed out the GNAT binder is automatically invoked
during the DLL build process by the `gnatdll` tool
(:ref:`Using_gnatdll`).


.. _Creating_a_Spec_for_Ada_DLLs:

Creating a Spec for Ada DLLs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To use the services exported by the Ada DLL from another programming
language (e.g., C), you have to translate the specs of the exported Ada
entities in that language. For instance in the case of `API.dll`,
the corresponding C header file could look like:

  .. code-block:: c

     extern int *_imp__count;
     #define count (*_imp__count)
     int factorial (int);

It is important to understand that when building an Ada DLL to be used by
other Ada applications, you need two different specs for the packages
contained in the DLL: one for building the DLL and the other for using
the DLL. This is because the `DLL` calling convention is needed to
use a variable defined in a DLL, but when building the DLL, the variable
must have either the `Ada` or `C` calling convention. As an
example consider a DLL comprising the following package `API`:

  .. code-block:: ada

     package API is
        Count : Integer := 0;
        ...
        --  Remainder of the package omitted.
     end API;

After producing a DLL containing package `API`, the spec that
must be used to import `API.Count` from Ada code outside of the
DLL is:

  .. code-block:: ada

     package API is
        Count : Integer;
        pragma Import (DLL, Count);
     end API;


.. _Creating_the_Definition_File:

Creating the Definition File
""""""""""""""""""""""""""""

The definition file is the last file needed to build the DLL. It lists
the exported symbols. As an example, the definition file for a DLL
containing only package `API` (where all the entities are exported
with a `C` calling convention) is:

  ::

    EXPORTS
        count
        factorial
        finalize_api
        initialize_api

If the `C` calling convention is missing from package `API`,
then the definition file contains the mangled Ada names of the above
entities, which in this case are:

  ::

    EXPORTS
        api__count
        api__factorial
        api__finalize_api
        api__initialize_api


.. _Using_gnatdll:

Using `gnatdll`
"""""""""""""""

.. index:: gnatdll

`gnatdll` is a tool to automate the DLL build process once all the Ada
and non-Ada sources that make up your DLL have been compiled.
`gnatdll` is actually in charge of two distinct tasks: build the
static import library for the DLL and the actual DLL. The form of the
`gnatdll` command is

  ::

      $ gnatdll [`switches`] `list-of-files` [-largs `opts`]

where `list-of-files` is a list of ALI and object files. The object
file list must be the exact list of objects corresponding to the non-Ada
sources whose services are to be included in the DLL. The ALI file list
must be the exact list of ALI files for the corresponding Ada sources
whose services are to be included in the DLL. If `list-of-files` is
missing, only the static import library is generated.

You may specify any of the following switches to `gnatdll`:


  .. index:: -a (gnatdll)

:samp:`-a[{address}]`
  Build a non-relocatable DLL at `address`. If `address` is not
  specified the default address `0x11000000` will be used. By default,
  when this switch is missing, `gnatdll` builds relocatable DLL. We
  advise the reader to build relocatable DLL.


  .. index:: -b (gnatdll)

:samp:`-b {address}`
  Set the relocatable DLL base address. By default the address is
  `0x11000000`.


  .. index:: -bargs (gnatdll)

:samp:`-bargs {opts}`
  Binder options. Pass `opts` to the binder.


  .. index:: -d (gnatdll)

:samp:`-d {dllfile}`
  `dllfile` is the name of the DLL. This switch must be present for
  `gnatdll` to do anything. The name of the generated import library is
  obtained algorithmically from `dllfile` as shown in the following
  example: if `dllfile` is `xyz.dll`, the import library name is
  `libxyz.dll.a`. The name of the definition file to use (if not specified
  by option *-e*) is obtained algorithmically from `dllfile`
  as shown in the following example:
  if `dllfile` is `xyz.dll`, the definition
  file used is `xyz.def`.


  .. index:: -e (gnatdll)

:samp:`-e {deffile}`
  `deffile` is the name of the definition file.


  .. index:: -g (gnatdll)

:samp:`-g`
  Generate debugging information. This information is stored in the object
  file and copied from there to the final DLL file by the linker,
  where it can be read by the debugger. You must use the
  *-g* switch if you plan on using the debugger or the symbolic
  stack traceback.


  .. index:: -h (gnatdll)

:samp:`-h`
  Help mode. Displays `gnatdll` switch usage information.


  .. index:: -I (gnatdll)

:samp:`-I{dir}`
  Direct `gnatdll` to search the `dir` directory for source and
  object files needed to build the DLL.
  (:ref:`Search_Paths_and_the_Run-Time_Library_RTL`).


  .. index:: -k (gnatdll)

:samp:`-k`
  Removes the :samp:`@{nn}` suffix from the import library's exported
  names, but keeps them for the link names. You must specify this
  option if you want to use a `Stdcall` function in a DLL for which
  the :samp:`@{nn}` suffix has been removed. This is the case for most
  of the Windows NT DLL for example. This option has no effect when
  *-n* option is specified.


  .. index:: -l (gnatdll)

:samp:`-l {file}`
  The list of ALI and object files used to build the DLL are listed in
  `file`, instead of being given in the command line. Each line in
  `file` contains the name of an ALI or object file.


  .. index:: -n (gnatdll)

:samp:`-n`
  No Import. Do not create the import library.


  .. index:: -q (gnatdll)

:samp:`-q`
  Quiet mode. Do not display unnecessary messages.


  .. index:: -v (gnatdll)

:samp:`-v`
  Verbose mode. Display extra information.


  .. index:: -largs (gnatdll)

:samp:`-largs {opts}`
  Linker options. Pass `opts` to the linker.


.. rubric:: `gnatdll` Example

As an example the command to build a relocatable DLL from :file:`api.adb`
once :file:`api.adb` has been compiled and :file:`api.def` created is

  ::

     $ gnatdll -d api.dll api.ali

The above command creates two files: :file:`libapi.dll.a` (the import
library) and :file:`api.dll` (the actual DLL). If you want to create
only the DLL, just type:

  ::

     $ gnatdll -d api.dll -n api.ali

Alternatively if you want to create just the import library, type:

  ::

     $ gnatdll -d api.dll


.. rubric:: `gnatdll` behind the Scenes

This section details the steps involved in creating a DLL. `gnatdll`
does these steps for you. Unless you are interested in understanding what
goes on behind the scenes, you should skip this section.

We use the previous example of a DLL containing the Ada package `API`,
to illustrate the steps necessary to build a DLL. The starting point is a
set of objects that will make up the DLL and the corresponding ALI
files. In the case of this example this means that :file:`api.o` and
:file:`api.ali` are available. To build a relocatable DLL, `gnatdll` does
the following:

* `gnatdll` builds the base file (:file:`api.base`). A base file gives
  the information necessary to generate relocation information for the
  DLL.

  ::

      $ gnatbind -n api
      $ gnatlink api -o api.jnk -mdll -Wl,--base-file,api.base

  In addition to the base file, the *gnatlink* command generates an
  output file :file:`api.jnk` which can be discarded. The *-mdll* switch
  asks *gnatlink* to generate the routines `DllMain` and
  `DllMainCRTStartup` that are called by the Windows loader when the DLL
  is loaded into memory.

* `gnatdll` uses `dlltool` (see :ref:`Using dlltool <Using_dlltool>`) to build the
  export table (:file:`api.exp`). The export table contains the relocation
  information in a form which can be used during the final link to ensure
  that the Windows loader is able to place the DLL anywhere in memory.

  ::

      $ dlltool --dllname api.dll --def api.def --base-file api.base \\
                --output-exp api.exp

* `gnatdll` builds the base file using the new export table. Note that
  *gnatbind* must be called once again since the binder generated file
  has been deleted during the previous call to *gnatlink*.

  ::

      $ gnatbind -n api
      $ gnatlink api -o api.jnk api.exp -mdll
            -Wl,--base-file,api.base


* `gnatdll` builds the new export table using the new base file and
  generates the DLL import library :file:`libAPI.dll.a`.


  ::

      $ dlltool --dllname api.dll --def api.def --base-file api.base \\
                --output-exp api.exp --output-lib libAPI.a

* Finally `gnatdll` builds the relocatable DLL using the final export
  table.

  ::

      $ gnatbind -n api
      $ gnatlink api api.exp -o api.dll -mdll


.. _Using_dlltool:

.. rubric:: Using `dlltool`

`dlltool` is the low-level tool used by `gnatdll` to build
DLLs and static import libraries. This section summarizes the most
common `dlltool` switches. The form of the `dlltool` command
is

  ::

    $ dlltool [`switches`]

`dlltool` switches include:


.. index:: --base-file (dlltool)

:samp:`--base-file {basefile}`
  Read the base file `basefile` generated by the linker. This switch
  is used to create a relocatable DLL.


.. index:: --def (dlltool)

:samp:`--def {deffile}`
  Read the definition file.


.. index:: --dllname (dlltool)

:samp:`--dllname {name}`
  Gives the name of the DLL. This switch is used to embed the name of the
  DLL in the static import library generated by `dlltool` with switch
  *--output-lib*.


.. index:: -k (dlltool)

:samp:`-k`
  Kill :samp:`@{nn}` from exported names
  (:ref:`Windows_Calling_Conventions`
  for a discussion about `Stdcall`-style symbols.


.. index:: --help (dlltool)

:samp:`--help`
  Prints the `dlltool` switches with a concise description.


.. index:: --output-exp (dlltool)

:samp:`--output-exp {exportfile}`
  Generate an export file `exportfile`. The export file contains the
  export table (list of symbols in the DLL) and is used to create the DLL.


.. index:: --output-lib (dlltool)

:samp:`--output-lib {libfile}`
  Generate a static import library `libfile`.


.. index:: -v (dlltool)

:samp:`-v`
  Verbose mode.


.. index:: --as (dlltool)

:samp:`--as {assembler-name}`
  Use `assembler-name` as the assembler. The default is `as`.


.. _GNAT_and_Windows_Resources:

GNAT and Windows Resources
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Resources, windows

Resources are an easy way to add Windows specific objects to your
application. The objects that can be added as resources include:

* menus

* accelerators

* dialog boxes

* string tables

* bitmaps

* cursors

* icons

* fonts

* version information

For example, a version information resource can be defined as follow and
embedded into an executable or DLL:

A version information resource can be used to embed information into an
executable or a DLL. These information can be viewed using the file properties
from the Windows Explorer. Here is an example of a version information
resource:

  ::

     1 VERSIONINFO
     FILEVERSION     1,0,0,0
     PRODUCTVERSION  1,0,0,0
     BEGIN
       BLOCK "StringFileInfo"
       BEGIN
         BLOCK "080904E4"
         BEGIN
           VALUE "CompanyName", "My Company Name"
           VALUE "FileDescription", "My application"
           VALUE "FileVersion", "1.0"
           VALUE "InternalName", "my_app"
           VALUE "LegalCopyright", "My Name"
           VALUE "OriginalFilename", "my_app.exe"
           VALUE "ProductName", "My App"
           VALUE "ProductVersion", "1.0"
         END
       END

       BLOCK "VarFileInfo"
       BEGIN
         VALUE "Translation", 0x809, 1252
       END
     END

The value `0809` (langID) is for the U.K English language and
`04E4` (charsetID), which is equal to `1252` decimal, for
multilingual.

This section explains how to build, compile and use resources. Note that this
section does not cover all resource objects, for a complete description see
the corresponding Microsoft documentation.


.. _Building_Resources:

Building Resources
""""""""""""""""""

.. index:: Resources, building

A resource file is an ASCII file. By convention resource files have an
:file:`.rc` extension.
The easiest way to build a resource file is to use Microsoft tools
such as `imagedit.exe` to build bitmaps, icons and cursors and
`dlgedit.exe` to build dialogs.
It is always possible to build an :file:`.rc` file yourself by writing a
resource script.

It is not our objective to explain how to write a resource file. A
complete description of the resource script language can be found in the
Microsoft documentation.


.. _Compiling_Resources:

Compiling Resources
"""""""""""""""""""

.. index:: rc
.. index:: windres
.. index:: Resources, compiling

This section describes how to build a GNAT-compatible (COFF) object file
containing the resources. This is done using the Resource Compiler
`windres` as follows:

  ::

     $ windres -i myres.rc -o myres.o

By default `windres` will run *gcc* to preprocess the :file:`.rc`
file. You can specify an alternate preprocessor (usually named
:file:`cpp.exe`) using the `windres` *--preprocessor*
parameter. A list of all possible options may be obtained by entering
the command `windres` *--help*.

It is also possible to use the Microsoft resource compiler `rc.exe`
to produce a :file:`.res` file (binary resource file). See the
corresponding Microsoft documentation for further details. In this case
you need to use `windres` to translate the :file:`.res` file to a
GNAT-compatible object file as follows:

  ::

     $ windres -i myres.res -o myres.o


.. _Using_Resources:

Using Resources
"""""""""""""""

.. index:: Resources, using

To include the resource file in your program just add the
GNAT-compatible object file for the resource(s) to the linker
arguments. With *gnatmake* this is done by using the *-largs*
option:

  ::

    $ gnatmake myprog -largs myres.o


.. _Using_GNAT_DLL_from_MSVS:

Using GNAT DLLs from Microsoft Visual Studio Applications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Microsoft Visual Studio, use with GNAT DLLs

This section describes a common case of mixed GNAT/Microsoft Visual Studio
application development, where the main program is developed using MSVS, and
is linked with a DLL developed using GNAT. Such a mixed application should
be developed following the general guidelines outlined above; below is the
cookbook-style sequence of steps to follow:

1. First develop and build the GNAT shared library using a library project
   (let's assume the project is `mylib.gpr`, producing the library `libmylib.dll`):

  ::

     $ gprbuild -p mylib.gpr

2. Produce a .def file for the symbols you need to interface with, either by
   hand or automatically with possibly some manual adjustments 
   (see :ref:`Creating Definition File Automatically <Create_Def_File_Automatically>`):

  ::

     $ dlltool libmylib.dll -z libmylib.def --export-all-symbols

3. Make sure that MSVS command-line tools are accessible on the path.

4. Create the Microsoft-style import library (see :ref:`MSVS-Style Import Library <MSVS-Style_Import_Library>`):

  ::

     $ lib -machine:IX86 -def:libmylib.def -out:libmylib.lib

If you are using a 64-bit toolchain, the above becomes...

  ::

     $ lib -machine:X64 -def:libmylib.def -out:libmylib.lib

5. Build the C main

  ::

     $ cl /O2 /MD main.c libmylib.lib

6. Before running the executable, make sure you have set the PATH to the DLL,
   or copy the DLL into into the directory containing the .exe.


.. _Debugging_a_DLL:

Debugging a DLL
^^^^^^^^^^^^^^^

.. index:: DLL debugging

Debugging a DLL is similar to debugging a standard program. But
we have to deal with two different executable parts: the DLL and the
program that uses it. We have the following four possibilities:

* The program and the DLL are built with `GCC/GNAT`.
* The program is built with foreign tools and the DLL is built with
  `GCC/GNAT`.
* The program is built with `GCC/GNAT` and the DLL is built with
  foreign tools.

In this section we address only cases one and two above.
There is no point in trying to debug
a DLL with `GNU/GDB`, if there is no GDB-compatible debugging
information in it. To do so you must use a debugger compatible with the
tools suite used to build the DLL.

.. _Program_and_DLL_Both_Built_with_GCC/GNAT:

Program and DLL Both Built with GCC/GNAT
""""""""""""""""""""""""""""""""""""""""

This is the simplest case. Both the DLL and the program have `GDB`
compatible debugging information. It is then possible to break anywhere in
the process. Let's suppose here that the main procedure is named
`ada_main` and that in the DLL there is an entry point named
`ada_dll`.

The DLL (:ref:`Introduction_to_Dynamic_Link_Libraries_DLLs`) and
program must have been built with the debugging information (see GNAT -g
switch). Here are the step-by-step instructions for debugging it:

* Launch `GDB` on the main program.

  ::

     $ gdb -nw ada_main

* Start the program and stop at the beginning of the main procedure

  ::

      (gdb) start

  This step is required to be able to set a breakpoint inside the DLL. As long
  as the program is not run, the DLL is not loaded. This has the
  consequence that the DLL debugging information is also not loaded, so it is not
  possible to set a breakpoint in the DLL.

* Set a breakpoint inside the DLL

  ::

      (gdb) break ada_dll
      (gdb) cont

At this stage a breakpoint is set inside the DLL. From there on
you can use the standard approach to debug the whole program
(:ref:`Running_and_Debugging_Ada_Programs`).


.. _Program_Built_with_Foreign_Tools_and_DLL_Built_with_GCC/GNAT:

Program Built with Foreign Tools and DLL Built with GCC/GNAT
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

In this case things are slightly more complex because it is not possible to
start the main program and then break at the beginning to load the DLL and the
associated DLL debugging information. It is not possible to break at the
beginning of the program because there is no `GDB` debugging information,
and therefore there is no direct way of getting initial control. This
section addresses this issue by describing some methods that can be used
to break somewhere in the DLL to debug it.

First suppose that the main procedure is named `main` (this is for
example some C code built with Microsoft Visual C) and that there is a
DLL named `test.dll` containing an Ada entry point named
`ada_dll`.

The DLL (see :ref:`Introduction_to_Dynamic_Link_Libraries_DLLs`) must have
been built with debugging information (see GNAT `-g` option).


.. rubric:: Debugging the DLL Directly

* Find out the executable starting address

  ::

      $ objdump --file-header main.exe

  The starting address is reported on the last line. For example:

  ::

      main.exe:     file format pei-i386
      architecture: i386, flags 0x0000010a:
      EXEC_P, HAS_DEBUG, D_PAGED
      start address 0x00401010

* Launch the debugger on the executable.

  ::

      $ gdb main.exe

* Set a breakpoint at the starting address, and launch the program.

  ::

      $ (gdb) break *0x00401010
      $ (gdb) run

  The program will stop at the given address.

* Set a breakpoint on a DLL subroutine.

  ::

    (gdb) break ada_dll.adb:45

  Or if you want to break using a symbol on the DLL, you need first to
  select the Ada language (language used by the DLL).

  ::

      (gdb) set language ada
      (gdb) break ada_dll

* Continue the program.

  ::

      (gdb) cont

  This will run the program until it reaches the breakpoint that has been
  set. From that point you can use the standard way to debug a program
  as described in (:ref:`Running_and_Debugging_Ada_Programs`).

It is also possible to debug the DLL by attaching to a running process.


.. rubric:: Attaching to a Running Process

.. index:: DLL debugging, attach to process

With `GDB` it is always possible to debug a running process by
attaching to it. It is possible to debug a DLL this way. The limitation
of this approach is that the DLL must run long enough to perform the
attach operation. It may be useful for instance to insert a time wasting
loop in the code of the DLL to meet this criterion.

* Launch the main program :file:`main.exe`.

  ::

      $ main

* Use the Windows *Task Manager* to find the process ID. Let's say
  that the process PID for :file:`main.exe` is 208.

* Launch gdb.

  ::

      $ gdb

* Attach to the running process to be debugged.

  ::

      (gdb) attach 208

* Load the process debugging information.

  ::

      (gdb) symbol-file main.exe

* Break somewhere in the DLL.

  ::

      (gdb) break ada_dll

* Continue process execution.

  ::

      (gdb) cont

This last step will resume the process execution, and stop at
the breakpoint we have set. From there you can use the standard
approach to debug a program as described in
:ref:`Running_and_Debugging_Ada_Programs`.


.. _Setting_Stack_Size_from_gnatlink:

Setting Stack Size from *gnatlink*
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible to specify the program stack size at link time. On modern
versions of Windows, starting with XP, this is mostly useful to set the size of
the main stack (environment task). The other task stacks are set with pragma
Storage_Size or with the *gnatbind -d* command.

Since older versions of Windows (2000, NT4, etc.) do not allow setting the
reserve size of individual tasks, the link-time stack size applies to all
tasks, and pragma Storage_Size has no effect.
In particular, Stack Overflow checks are made against this
link-time specified size.

This setting can be done with *gnatlink* using either of the following:


* *-Xlinker* linker option

  ::

      $ gnatlink hello -Xlinker --stack=0x10000,0x1000


  This sets the stack reserve size to 0x10000 bytes and the stack commit
  size to 0x1000 bytes.

* *-Wl* linker option

  ::

    $ gnatlink hello -Wl,--stack=0x1000000

  This sets the stack reserve size to 0x1000000 bytes. Note that with
  *-Wl* option it is not possible to set the stack commit size
  because the coma is a separator for this option.


.. _Setting_Heap_Size_from_gnatlink:

Setting Heap Size from *gnatlink*
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Under Windows systems, it is possible to specify the program heap size from
*gnatlink* using either of the following:

* *-Xlinker* linker option

  ::

      $ gnatlink hello -Xlinker --heap=0x10000,0x1000

  This sets the heap reserve size to 0x10000 bytes and the heap commit
  size to 0x1000 bytes.

* *-Wl* linker option

  ::

      $ gnatlink hello -Wl,--heap=0x1000000


  This sets the heap reserve size to 0x1000000 bytes. Note that with
  *-Wl* option it is not possible to set the heap commit size
  because the coma is a separator for this option.


.. _Win32_Specific_Addons:

Windows Specific Add-Ons
-------------------------

This section describes the Windows specific add-ons.

.. _Win32Ada:

Win32Ada
^^^^^^^^

Win32Ada is a binding for the Microsoft Win32 API. This binding can be
easily installed from the provided installer. To use the Win32Ada
binding you need to use a project file, and adding a single with_clause
will give you full access to the Win32Ada binding sources and ensure
that the proper libraries are passed to the linker.

  .. code-block:: gpr

      with "win32ada";
      project P is
         for Sources use ...;
      end P;

To build the application you just need to call gprbuild for the
application's project, here p.gpr:

  .. code-block:: sh

      gprbuild p.gpr

.. _wPOSIX:

wPOSIX
^^^^^^

wPOSIX is a minimal POSIX binding whose goal is to help with building
cross-platforms applications. This binding is not complete though, as
the Win32 API does not provide the necessary support for all POSIX APIs.

To use the wPOSIX binding you need to use a project file, and adding
a single with_clause will give you full access to the wPOSIX binding
sources and ensure that the proper libraries are passed to the linker.

  .. code-block:: gpr

      with "wposix";
      project P is
         for Sources use ...;
      end P;

To build the application you just need to call gprbuild for the
application's project, here p.gpr:

  .. code-block:: sh

      gprbuild p.gpr


.. _Mac_OS_Topics:

Mac OS Topics
=============

.. index:: OS X

This section describes topics that are specific to Apple's OS X
platform.

Codesigning the Debugger
------------------------

The Darwin Kernel requires the debugger to have special permissions
before it is allowed to control other processes. These permissions
are granted by codesigning the GDB executable. Without these
permissions, the debugger will report error messages such as::

   Starting program: /x/y/foo
   Unable to find Mach task port for process-id 28885: (os/kern) failure (0x5).
   (please check gdb is codesigned - see taskgated(8))

Codesigning requires a certificate.  The following procedure explains
how to create one:

* Start the Keychain Access application (in
  /Applications/Utilities/Keychain Access.app)

* Select the Keychain Access -> Certificate Assistant ->
  Create a Certificate... menu

* Then:

  * Choose a name for the new certificate (this procedure will use
    "gdb-cert" as an example)

  * Set "Identity Type" to "Self Signed Root"

  * Set "Certificate Type" to "Code Signing"

  * Activate the "Let me override defaults" option


* Click several times on "Continue" until the "Specify a Location
  For The Certificate" screen appears, then set "Keychain" to "System"

* Click on "Continue" until the certificate is created

* Finally, in the view, double-click on the new certificate,
  and set "When using this certificate" to "Always Trust"

* Exit the Keychain Access application and restart the computer
  (this is unfortunately required)


Once a certificate has been created, the debugger can be codesigned
as follow. In a Terminal, run the following command:

  ::

     $ codesign -f -s  "gdb-cert"  <gnat_install_prefix>/bin/gdb

where "gdb-cert" should be replaced by the actual certificate
name chosen above, and <gnat_install_prefix> should be replaced by
the location where you installed GNAT.  Also, be sure that users are
in the Unix group ``_developer``.
