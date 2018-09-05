.. role:: switch(samp)

.. |with| replace:: *with*
.. |withs| replace:: *with*\ s
.. |withed| replace:: *with*\ ed
.. |withing| replace:: *with*\ ing

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit


.. _GNAT_and_Program_Execution:

**************************
GNAT and Program Execution
**************************

This chapter covers several topics:

* `Running and Debugging Ada Programs`_
* `Profiling`_
* `Improving Performance`_
* `Overflow Check Handling in GNAT`_
* `Performing Dimensionality Analysis in GNAT`_
* `Stack Related Facilities`_
* `Memory Management Issues`_

.. _Running_and_Debugging_Ada_Programs:

Running and Debugging Ada Programs
==================================

.. index:: Debugging

This section discusses how to debug Ada programs.

An incorrect Ada program may be handled in three ways by the GNAT compiler:

* The illegality may be a violation of the static semantics of Ada. In
  that case GNAT diagnoses the constructs in the program that are illegal.
  It is then a straightforward matter for the user to modify those parts of
  the program.

* The illegality may be a violation of the dynamic semantics of Ada. In
  that case the program compiles and executes, but may generate incorrect
  results, or may terminate abnormally with some exception.

* When presented with a program that contains convoluted errors, GNAT
  itself may terminate abnormally without providing full diagnostics on
  the incorrect user program.

.. index:: Debugger

.. index:: !  gdb

.. _The_GNAT_Debugger_GDB:

The GNAT Debugger GDB
---------------------

``GDB`` is a general purpose, platform-independent debugger that
can be used to debug mixed-language programs compiled with ``gcc``,
and in particular is capable of debugging Ada programs compiled with
GNAT. The latest versions of ``GDB`` are Ada-aware and can handle
complex Ada data structures.

See :title:`Debugging with GDB`,
for full details on the usage of ``GDB``, including a section on
its usage on programs. This manual should be consulted for full
details. The section that follows is a brief introduction to the
philosophy and use of ``GDB``.

When GNAT programs are compiled, the compiler optionally writes debugging
information into the generated object file, including information on
line numbers, and on declared types and variables. This information is
separate from the generated code. It makes the object files considerably
larger, but it does not add to the size of the actual executable that
will be loaded into memory, and has no impact on run-time performance. The
generation of debug information is triggered by the use of the
:switch:`-g` switch in the ``gcc`` or ``gnatmake`` command
used to carry out the compilations. It is important to emphasize that
the use of these options does not change the generated code.

The debugging information is written in standard system formats that
are used by many tools, including debuggers and profilers. The format
of the information is typically designed to describe C types and
semantics, but GNAT implements a translation scheme which allows full
details about Ada types and variables to be encoded into these
standard C formats. Details of this encoding scheme may be found in
the file exp_dbug.ads in the GNAT source distribution. However, the
details of this encoding are, in general, of no interest to a user,
since ``GDB`` automatically performs the necessary decoding.

When a program is bound and linked, the debugging information is
collected from the object files, and stored in the executable image of
the program. Again, this process significantly increases the size of
the generated executable file, but it does not increase the size of
the executable program itself. Furthermore, if this program is run in
the normal manner, it runs exactly as if the debug information were
not present, and takes no more actual memory.

However, if the program is run under control of ``GDB``, the
debugger is activated.  The image of the program is loaded, at which
point it is ready to run.  If a run command is given, then the program
will run exactly as it would have if ``GDB`` were not present. This
is a crucial part of the ``GDB`` design philosophy.  ``GDB`` is
entirely non-intrusive until a breakpoint is encountered.  If no
breakpoint is ever hit, the program will run exactly as it would if no
debugger were present. When a breakpoint is hit, ``GDB`` accesses
the debugging information and can respond to user commands to inspect
variables, and more generally to report on the state of execution.

.. _Running_GDB:

Running GDB
-----------

This section describes how to initiate the debugger.

The debugger can be launched from a ``GPS`` menu or
directly from the command line. The description below covers the latter use.
All the commands shown can be used in the ``GPS`` debug console window,
but there are usually more GUI-based ways to achieve the same effect.

The command to run ``GDB`` is

  ::

     $ gdb program

where ``program`` is the name of the executable file. This
activates the debugger and results in a prompt for debugger commands.
The simplest command is simply ``run``, which causes the program to run
exactly as if the debugger were not present. The following section
describes some of the additional commands that can be given to ``GDB``.


.. _Introduction_to_GDB_Commands:

Introduction to GDB Commands
----------------------------

``GDB`` contains a large repertoire of commands.
See :title:`Debugging with GDB` for extensive documentation on the use
of these commands, together with examples of their use. Furthermore,
the command *help* invoked from within GDB activates a simple help
facility which summarizes the available commands and their options.
In this section we summarize a few of the most commonly
used commands to give an idea of what ``GDB`` is about. You should create
a simple program with debugging information and experiment with the use of
these ``GDB`` commands on the program as you read through the
following section.

* :samp:`set args {arguments}`
    The *arguments* list above is a list of arguments to be passed to
    the program on a subsequent run command, just as though the arguments
    had been entered on a normal invocation of the program. The ``set args``
    command is not needed if the program does not require arguments.


* :samp:`run`
    The ``run`` command causes execution of the program to start from
    the beginning. If the program is already running, that is to say if
    you are currently positioned at a breakpoint, then a prompt will ask
    for confirmation that you want to abandon the current execution and
    restart.


* :samp:`breakpoint {location}`
    The breakpoint command sets a breakpoint, that is to say a point at which
    execution will halt and ``GDB`` will await further
    commands. *location* is
    either a line number within a file, given in the format ``file:linenumber``,
    or it is the name of a subprogram. If you request that a breakpoint be set on
    a subprogram that is overloaded, a prompt will ask you to specify on which of
    those subprograms you want to breakpoint. You can also
    specify that all of them should be breakpointed. If the program is run
    and execution encounters the breakpoint, then the program
    stops and ``GDB`` signals that the breakpoint was encountered by
    printing the line of code before which the program is halted.


* :samp:`catch exception {name}`
    This command causes the program execution to stop whenever exception
    ``name`` is raised.  If ``name`` is omitted, then the execution is
    suspended when any exception is raised.


* :samp:`print {expression}`
    This will print the value of the given expression. Most simple
    Ada expression formats are properly handled by ``GDB``, so the expression
    can contain function calls, variables, operators, and attribute references.


* :samp:`continue`
    Continues execution following a breakpoint, until the next breakpoint or the
    termination of the program.


* :samp:`step`
    Executes a single line after a breakpoint. If the next statement
    is a subprogram call, execution continues into (the first statement of)
    the called subprogram.


* :samp:`next`
    Executes a single line. If this line is a subprogram call, executes and
    returns from the call.


* :samp:`list`
    Lists a few lines around the current source location. In practice, it
    is usually more convenient to have a separate edit window open with the
    relevant source file displayed. Successive applications of this command
    print subsequent lines. The command can be given an argument which is a
    line number, in which case it displays a few lines around the specified one.


* :samp:`backtrace`
    Displays a backtrace of the call chain. This command is typically
    used after a breakpoint has occurred, to examine the sequence of calls that
    leads to the current breakpoint. The display includes one line for each
    activation record (frame) corresponding to an active subprogram.


* :samp:`up`
    At a breakpoint, ``GDB`` can display the values of variables local
    to the current frame. The command ``up`` can be used to
    examine the contents of other active frames, by moving the focus up
    the stack, that is to say from callee to caller, one frame at a time.


* :samp:`down`
    Moves the focus of ``GDB`` down from the frame currently being
    examined to the frame of its callee (the reverse of the previous command),


* :samp:`frame {n}`
    Inspect the frame with the given number. The value 0 denotes the frame
    of the current breakpoint, that is to say the top of the call stack.


* :samp:`kill`
    Kills the child process in which the program is running under GDB.
    This may be useful for several purposes:

    * It allows you to recompile and relink your program, since on many systems
      you cannot regenerate an executable file while it is running in a process.

    * You can run your program outside the debugger, on systems that do not
      permit executing a program outside GDB while breakpoints are set
      within GDB.

    * It allows you to debug a core dump rather than a running process.

The above list is a very short introduction to the commands that
``GDB`` provides. Important additional capabilities, including conditional
breakpoints, the ability to execute command sequences on a breakpoint,
the ability to debug at the machine instruction level and many other
features are described in detail in :title:`Debugging with GDB`.
Note that most commands can be abbreviated
(for example, c for continue, bt for backtrace).


.. _Using_Ada_Expressions:

Using Ada Expressions
---------------------

.. index:: Ada expressions (in gdb)

``GDB`` supports a fairly large subset of Ada expression syntax, with some
extensions. The philosophy behind the design of this subset is

  * That ``GDB`` should provide basic literals and access to operations for
    arithmetic, dereferencing, field selection, indexing, and subprogram calls,
    leaving more sophisticated computations to subprograms written into the
    program (which therefore may be called from ``GDB``).

  * That type safety and strict adherence to Ada language restrictions
    are not particularly relevant in a debugging context.

  * That brevity is important to the ``GDB`` user.

Thus, for brevity, the debugger acts as if there were
implicit ``with`` and ``use`` clauses in effect for all user-written
packages, thus making it unnecessary to fully qualify most names with
their packages, regardless of context. Where this causes ambiguity,
``GDB`` asks the user's intent.

For details on the supported Ada syntax, see :title:`Debugging with GDB`.


.. _Calling_User-Defined_Subprograms:

Calling User-Defined Subprograms
--------------------------------

An important capability of ``GDB`` is the ability to call user-defined
subprograms while debugging. This is achieved simply by entering
a subprogram call statement in the form:

  ::

     call subprogram-name (parameters)

The keyword ``call`` can be omitted in the normal case where the
``subprogram-name`` does not coincide with any of the predefined
``GDB`` commands.

The effect is to invoke the given subprogram, passing it the
list of parameters that is supplied. The parameters can be expressions and
can include variables from the program being debugged. The
subprogram must be defined
at the library level within your program, and ``GDB`` will call the
subprogram within the environment of your program execution (which
means that the subprogram is free to access or even modify variables
within your program).

The most important use of this facility is in allowing the inclusion of
debugging routines that are tailored to particular data structures
in your program. Such debugging routines can be written to provide a suitably
high-level description of an abstract type, rather than a low-level dump
of its physical layout. After all, the standard
``GDB print`` command only knows the physical layout of your
types, not their abstract meaning. Debugging routines can provide information
at the desired semantic level and are thus enormously useful.

For example, when debugging GNAT itself, it is crucial to have access to
the contents of the tree nodes used to represent the program internally.
But tree nodes are represented simply by an integer value (which in turn
is an index into a table of nodes).
Using the ``print`` command on a tree node would simply print this integer
value, which is not very useful. But the PN routine (defined in file
treepr.adb in the GNAT sources) takes a tree node as input, and displays
a useful high level representation of the tree node, which includes the
syntactic category of the node, its position in the source, the integers
that denote descendant nodes and parent node, as well as varied
semantic information. To study this example in more detail, you might want to
look at the body of the PN procedure in the stated file.

Another useful application of this capability is to deal with situations of
complex data which are not handled suitably by GDB. For example, if you specify
Convention Fortran for a multi-dimensional array, GDB does not know that
the ordering of array elements has been switched and will not properly
address the array elements. In such a case, instead of trying to print the
elements directly from GDB, you can write a callable procedure that prints
the elements in the desired format.


.. _Using_the_Next_Command_in_a_Function:

Using the *next* Command in a Function
--------------------------------------

When you use the ``next`` command in a function, the current source
location will advance to the next statement as usual. A special case
arises in the case of a ``return`` statement.

Part of the code for a return statement is the 'epilogue' of the function.
This is the code that returns to the caller. There is only one copy of
this epilogue code, and it is typically associated with the last return
statement in the function if there is more than one return. In some
implementations, this epilogue is associated with the first statement
of the function.

The result is that if you use the ``next`` command from a return
statement that is not the last return statement of the function you
may see a strange apparent jump to the last return statement or to
the start of the function. You should simply ignore this odd jump.
The value returned is always that from the first return statement
that was stepped through.


.. _Stopping_When_Ada_Exceptions_Are_Raised:

Stopping When Ada Exceptions Are Raised
---------------------------------------

.. index:: Exceptions (in gdb)

You can set catchpoints that stop the program execution when your program
raises selected exceptions.


* :samp:`catch exception`
    Set a catchpoint that stops execution whenever (any task in the) program
    raises any exception.


* :samp:`catch exception {name}`
    Set a catchpoint that stops execution whenever (any task in the) program
    raises the exception *name*.


* :samp:`catch exception unhandled`
    Set a catchpoint that stops executing whenever (any task in the) program
    raises an exception for which there is no handler.


* :samp:`info exceptions`, :samp:`info exceptions {regexp}`
    The ``info exceptions`` command permits the user to examine all defined
    exceptions within Ada programs. With a regular expression, *regexp*, as
    argument, prints out only those exceptions whose name matches *regexp*.


.. index:: Tasks (in gdb)

.. _Ada_Tasks:

Ada Tasks
---------

``GDB`` allows the following task-related commands:


* :samp:`info tasks`
    This command shows a list of current Ada tasks, as in the following example:

    ::

       (gdb) info tasks
         ID       TID P-ID   Thread Pri State                 Name
          1   8088000   0   807e000  15 Child Activation Wait main_task
          2   80a4000   1   80ae000  15 Accept/Select Wait    b
          3   809a800   1   80a4800  15 Child Activation Wait a
       *  4   80ae800   3   80b8000  15 Running               c


    In this listing, the asterisk before the first task indicates it to be the
    currently running task. The first column lists the task ID that is used
    to refer to tasks in the following commands.


.. index:: Breakpoints and tasks

* ``break``*linespec* ``task`` *taskid*, ``break`` *linespec* ``task`` *taskid* ``if`` ...

    These commands are like the ``break ... thread ...``.
    *linespec* specifies source lines.

    Use the qualifier :samp:`task {taskid}` with a breakpoint command
    to specify that you only want ``GDB`` to stop the program when a
    particular Ada task reaches this breakpoint. *taskid* is one of the
    numeric task identifiers assigned by ``GDB``, shown in the first
    column of the ``info tasks`` display.

    If you do not specify :samp:`task {taskid}` when you set a
    breakpoint, the breakpoint applies to *all* tasks of your
    program.

    You can use the ``task`` qualifier on conditional breakpoints as
    well; in this case, place :samp:`task {taskid}` before the
    breakpoint condition (before the ``if``).

.. index:: Task switching (in gdb)

* :samp:`task {taskno}`

    This command allows switching to the task referred by *taskno*. In
    particular, this allows browsing of the backtrace of the specified
    task. It is advisable to switch back to the original task before
    continuing execution otherwise the scheduling of the program may be
    perturbed.

For more detailed information on the tasking support,
see :title:`Debugging with GDB`.


.. index:: Debugging Generic Units
.. index:: Generics

.. _Debugging_Generic_Units:

Debugging Generic Units
-----------------------

GNAT always uses code expansion for generic instantiation. This means that
each time an instantiation occurs, a complete copy of the original code is
made, with appropriate substitutions of formals by actuals.

It is not possible to refer to the original generic entities in
``GDB``, but it is always possible to debug a particular instance of
a generic, by using the appropriate expanded names. For example, if we have

  .. code-block:: ada

     procedure g is

        generic package k is
           procedure kp (v1 : in out integer);
        end k;

        package body k is
           procedure kp (v1 : in out integer) is
           begin
              v1 := v1 + 1;
           end kp;
        end k;

        package k1 is new k;
        package k2 is new k;

        var : integer := 1;

     begin
        k1.kp (var);
        k2.kp (var);
        k1.kp (var);
        k2.kp (var);
     end;

Then to break on a call to procedure kp in the k2 instance, simply
use the command:

  ::

     (gdb) break g.k2.kp

When the breakpoint occurs, you can step through the code of the
instance in the normal manner and examine the values of local variables, as for
other units.


.. index:: Remote Debugging with gdbserver

.. _Remote_Debugging_with_gdbserver:

Remote Debugging with gdbserver
-------------------------------

On platforms where gdbserver is supported, it is possible to use this tool
to debug your application remotely.  This can be useful in situations
where the program needs to be run on a target host that is different
from the host used for development, particularly when the target has
a limited amount of resources (either CPU and/or memory).

To do so, start your program using gdbserver on the target machine.
gdbserver then automatically suspends the execution of your program
at its entry point, waiting for a debugger to connect to it.  The
following commands starts an application and tells gdbserver to
wait for a connection with the debugger on localhost port 4444.


  ::

     $ gdbserver localhost:4444 program
     Process program created; pid = 5685
     Listening on port 4444

Once gdbserver has started listening, we can tell the debugger to establish
a connection with this gdbserver, and then start the same debugging session
as if the program was being debugged on the same host, directly under
the control of GDB.

  ::

     $ gdb program
     (gdb) target remote targethost:4444
     Remote debugging using targethost:4444
     0x00007f29936d0af0 in ?? () from /lib64/ld-linux-x86-64.so.
     (gdb) b foo.adb:3
     Breakpoint 1 at 0x401f0c: file foo.adb, line 3.
     (gdb) continue
     Continuing.

     Breakpoint 1, foo () at foo.adb:4
     4       end foo;

It is also possible to use gdbserver to attach to an already running
program, in which case the execution of that program is simply suspended
until the connection between the debugger and gdbserver is established.

For more information on how to use gdbserver, see the *Using the gdbserver Program*
section in :title:`Debugging with GDB`.
GNAT provides support for gdbserver on x86-linux, x86-windows and x86_64-linux.


.. index:: Abnormal Termination or Failure to Terminate

.. _GNAT_Abnormal_Termination_or_Failure_to_Terminate:

GNAT Abnormal Termination or Failure to Terminate
-------------------------------------------------

When presented with programs that contain serious errors in syntax
or semantics,
GNAT may on rare occasions  experience problems in operation, such
as aborting with a
segmentation fault or illegal memory access, raising an internal
exception, terminating abnormally, or failing to terminate at all.
In such cases, you can activate
various features of GNAT that can help you pinpoint the construct in your
program that is the likely source of the problem.

The following strategies are presented in increasing order of
difficulty, corresponding to your experience in using GNAT and your
familiarity with compiler internals.

* Run ``gcc`` with the :switch:`-gnatf`. This first
  switch causes all errors on a given line to be reported. In its absence,
  only the first error on a line is displayed.

  The :switch:`-gnatdO` switch causes errors to be displayed as soon as they
  are encountered, rather than after compilation is terminated. If GNAT
  terminates prematurely or goes into an infinite loop, the last error
  message displayed may help to pinpoint the culprit.

* Run ``gcc`` with the :switch:`-v` (verbose) switch. In this
  mode, ``gcc`` produces ongoing information about the progress of the
  compilation and provides the name of each procedure as code is
  generated. This switch allows you to find which Ada procedure was being
  compiled when it encountered a code generation problem.

.. index:: -gnatdc switch

* Run ``gcc`` with the :switch:`-gnatdc` switch. This is a GNAT specific
  switch that does for the front-end what :switch:`-v` does
  for the back end. The system prints the name of each unit,
  either a compilation unit or nested unit, as it is being analyzed.

* Finally, you can start
  ``gdb`` directly on the ``gnat1`` executable. ``gnat1`` is the
  front-end of GNAT, and can be run independently (normally it is just
  called from ``gcc``). You can use ``gdb`` on ``gnat1`` as you
  would on a C program (but :ref:`The_GNAT_Debugger_GDB` for caveats). The
  ``where`` command is the first line of attack; the variable
  ``lineno`` (seen by ``print lineno``), used by the second phase of
  ``gnat1`` and by the ``gcc`` backend, indicates the source line at
  which the execution stopped, and ``input_file name`` indicates the name of
  the source file.


.. _Naming_Conventions_for_GNAT_Source_Files:

Naming Conventions for GNAT Source Files
----------------------------------------

In order to examine the workings of the GNAT system, the following
brief description of its organization may be helpful:

* Files with prefix :file:`sc` contain the lexical scanner.

* All files prefixed with :file:`par` are components of the parser. The
  numbers correspond to chapters of the Ada Reference Manual. For example,
  parsing of select statements can be found in :file:`par-ch9.adb`.

* All files prefixed with :file:`sem` perform semantic analysis. The
  numbers correspond to chapters of the Ada standard. For example, all
  issues involving context clauses can be found in :file:`sem_ch10.adb`. In
  addition, some features of the language require sufficient special processing
  to justify their own semantic files: sem_aggr for aggregates, sem_disp for
  dynamic dispatching, etc.

* All files prefixed with :file:`exp` perform normalization and
  expansion of the intermediate representation (abstract syntax tree, or AST).
  these files use the same numbering scheme as the parser and semantics files.
  For example, the construction of record initialization procedures is done in
  :file:`exp_ch3.adb`.

* The files prefixed with :file:`bind` implement the binder, which
  verifies the consistency of the compilation, determines an order of
  elaboration, and generates the bind file.

* The files :file:`atree.ads` and :file:`atree.adb` detail the low-level
  data structures used by the front-end.

* The files :file:`sinfo.ads` and :file:`sinfo.adb` detail the structure of
  the abstract syntax tree as produced by the parser.

* The files :file:`einfo.ads` and :file:`einfo.adb` detail the attributes of
  all entities, computed during semantic analysis.

* Library management issues are dealt with in files with prefix
  :file:`lib`.

  .. index:: Annex A (in Ada Reference Manual)

* Ada files with the prefix :file:`a-` are children of ``Ada``, as
  defined in Annex A.

  .. index:: Annex B (in Ada reference Manual)

* Files with prefix :file:`i-` are children of ``Interfaces``, as
  defined in Annex B.

  .. index::  System (package in Ada Reference Manual)

* Files with prefix :file:`s-` are children of ``System``. This includes
  both language-defined children and GNAT run-time routines.

  .. index:: GNAT (package)

* Files with prefix :file:`g-` are children of ``GNAT``. These are useful
  general-purpose packages, fully documented in their specs. All
  the other :file:`.c` files are modifications of common ``gcc`` files.


.. _Getting_Internal_Debugging_Information:

Getting Internal Debugging Information
--------------------------------------

Most compilers have internal debugging switches and modes. GNAT
does also, except GNAT internal debugging switches and modes are not
secret. A summary and full description of all the compiler and binder
debug flags are in the file :file:`debug.adb`. You must obtain the
sources of the compiler to see the full detailed effects of these flags.

The switches that print the source of the program (reconstructed from
the internal tree) are of general interest for user programs, as are the
options to print
the full internal tree, and the entity table (the symbol table
information). The reconstructed source provides a readable version of the
program after the front-end has completed analysis and  expansion,
and is useful when studying the performance of specific constructs.
For example, constraint checks are indicated, complex aggregates
are replaced with loops and assignments, and tasking primitives
are replaced with run-time calls.


.. index:: traceback
.. index:: stack traceback
.. index:: stack unwinding

.. _Stack_Traceback:

Stack Traceback
---------------

Traceback is a mechanism to display the sequence of subprogram calls that
leads to a specified execution point in a program. Often (but not always)
the execution point is an instruction at which an exception has been raised.
This mechanism is also known as *stack unwinding* because it obtains
its information by scanning the run-time stack and recovering the activation
records of all active subprograms. Stack unwinding is one of the most
important tools for program debugging.

The first entry stored in traceback corresponds to the deepest calling level,
that is to say the subprogram currently executing the instruction
from which we want to obtain the traceback.

Note that there is no runtime performance penalty when stack traceback
is enabled, and no exception is raised during program execution.

.. index:: traceback, non-symbolic

.. _Non-Symbolic_Traceback:

Non-Symbolic Traceback
^^^^^^^^^^^^^^^^^^^^^^

Note: this feature is not supported on all platforms. See
:samp:`GNAT.Traceback` spec in :file:`g-traceb.ads`
for a complete list of supported platforms.

.. rubric:: Tracebacks From an Unhandled Exception

A runtime non-symbolic traceback is a list of addresses of call instructions.
To enable this feature you must use the :switch:`-E`
``gnatbind`` option. With this option a stack traceback is stored as part
of exception information. You can retrieve this information using the
``addr2line`` tool.

Here is a simple example:

  .. code-block:: ada

     procedure STB is

        procedure P1 is
        begin
           raise Constraint_Error;
        end P1;

        procedure P2 is
        begin
           P1;
        end P2;

     begin
        P2;
     end STB;

  ::

     $ gnatmake stb -bargs -E
     $ stb

     Execution terminated by unhandled exception
     Exception name: CONSTRAINT_ERROR
     Message: stb.adb:5
     Call stack traceback locations:
     0x401373 0x40138b 0x40139c 0x401335 0x4011c4 0x4011f1 0x77e892a4

As we see the traceback lists a sequence of addresses for the unhandled
exception ``CONSTRAINT_ERROR`` raised in procedure P1. It is easy to
guess that this exception come from procedure P1. To translate these
addresses into the source lines where the calls appear, the
``addr2line`` tool, described below, is invaluable. The use of this tool
requires the program to be compiled with debug information.

  ::

     $ gnatmake -g stb -bargs -E
     $ stb

     Execution terminated by unhandled exception
     Exception name: CONSTRAINT_ERROR
     Message: stb.adb:5
     Call stack traceback locations:
     0x401373 0x40138b 0x40139c 0x401335 0x4011c4 0x4011f1 0x77e892a4

     $ addr2line --exe=stb 0x401373 0x40138b 0x40139c 0x401335 0x4011c4
        0x4011f1 0x77e892a4

     00401373 at d:/stb/stb.adb:5
     0040138B at d:/stb/stb.adb:10
     0040139C at d:/stb/stb.adb:14
     00401335 at d:/stb/b~stb.adb:104
     004011C4 at /build/.../crt1.c:200
     004011F1 at /build/.../crt1.c:222
     77E892A4 in ?? at ??:0

The ``addr2line`` tool has several other useful options:

  ======================== ========================================================
  :samp:`--functions`      to get the function name corresponding to any location
  :samp:`--demangle=gnat`  to use the gnat decoding mode for the function names.
                           Note that for binutils version 2.9.x the option is
                           simply :samp:`--demangle`.
  ======================== ========================================================

  ::

     $ addr2line --exe=stb --functions --demangle=gnat 0x401373 0x40138b
        0x40139c 0x401335 0x4011c4 0x4011f1

     00401373 in stb.p1 at d:/stb/stb.adb:5
     0040138B in stb.p2 at d:/stb/stb.adb:10
     0040139C in stb at d:/stb/stb.adb:14
     00401335 in main at d:/stb/b~stb.adb:104
     004011C4 in <__mingw_CRTStartup> at /build/.../crt1.c:200
     004011F1 in <mainCRTStartup> at /build/.../crt1.c:222

From this traceback we can see that the exception was raised in
:file:`stb.adb` at line 5, which was reached from a procedure call in
:file:`stb.adb` at line 10, and so on. The :file:`b~std.adb` is the binder file,
which contains the call to the main program.
:ref:`Running_gnatbind`. The remaining entries are assorted runtime routines,
and the output will vary from platform to platform.

It is also possible to use ``GDB`` with these traceback addresses to debug
the program. For example, we can break at a given code location, as reported
in the stack traceback:

  ::

     $ gdb -nw stb

Furthermore, this feature is not implemented inside Windows DLL. Only
the non-symbolic traceback is reported in this case.

  ::

     (gdb) break *0x401373
     Breakpoint 1 at 0x401373: file stb.adb, line 5.

It is important to note that the stack traceback addresses
do not change when debug information is included. This is particularly useful
because it makes it possible to release software without debug information (to
minimize object size), get a field report that includes a stack traceback
whenever an internal bug occurs, and then be able to retrieve the sequence
of calls with the same program compiled with debug information.


.. rubric:: Tracebacks From Exception Occurrences

Non-symbolic tracebacks are obtained by using the :switch:`-E` binder argument.
The stack traceback is attached to the exception information string, and can
be retrieved in an exception handler within the Ada program, by means of the
Ada facilities defined in ``Ada.Exceptions``. Here is a simple example:

  .. code-block:: ada

      with Ada.Text_IO;
      with Ada.Exceptions;

      procedure STB is

         use Ada;
         use Ada.Exceptions;

         procedure P1 is
            K : Positive := 1;
         begin
            K := K - 1;
         exception
            when E : others =>
               Text_IO.Put_Line (Exception_Information (E));
         end P1;

         procedure P2 is
         begin
            P1;
         end P2;

      begin
         P2;
      end STB;

This program will output:

  ::

     $ stb

     Exception name: CONSTRAINT_ERROR
     Message: stb.adb:12
     Call stack traceback locations:
     0x4015e4 0x401633 0x401644 0x401461 0x4011c4 0x4011f1 0x77e892a4


.. rubric:: Tracebacks From Anywhere in a Program

It is also possible to retrieve a stack traceback from anywhere in a
program. For this you need to
use the ``GNAT.Traceback`` API. This package includes a procedure called
``Call_Chain`` that computes a complete stack traceback, as well as useful
display procedures described below. It is not necessary to use the
:switch:`-E` ``gnatbind`` option in this case, because the stack traceback mechanism
is invoked explicitly.

In the following example we compute a traceback at a specific location in
the program, and we display it using ``GNAT.Debug_Utilities.Image`` to
convert addresses to strings:


  .. code-block:: ada

      with Ada.Text_IO;
      with GNAT.Traceback;
      with GNAT.Debug_Utilities;

      procedure STB is

         use Ada;
         use GNAT;
         use GNAT.Traceback;

         procedure P1 is
            TB  : Tracebacks_Array (1 .. 10);
            --  We are asking for a maximum of 10 stack frames.
            Len : Natural;
            --  Len will receive the actual number of stack frames returned.
         begin
            Call_Chain (TB, Len);

            Text_IO.Put ("In STB.P1 : ");

            for K in 1 .. Len loop
               Text_IO.Put (Debug_Utilities.Image (TB (K)));
               Text_IO.Put (' ');
            end loop;

            Text_IO.New_Line;
         end P1;

         procedure P2 is
         begin
            P1;
         end P2;

      begin
         P2;
      end STB;

  ::

     $ gnatmake -g stb
     $ stb

     In STB.P1 : 16#0040_F1E4# 16#0040_14F2# 16#0040_170B# 16#0040_171C#
     16#0040_1461# 16#0040_11C4# 16#0040_11F1# 16#77E8_92A4#


You can then get further information by invoking the ``addr2line``
tool as described earlier (note that the hexadecimal addresses
need to be specified in C format, with a leading '0x').

.. index:: traceback, symbolic

.. _Symbolic_Traceback:

Symbolic Traceback
^^^^^^^^^^^^^^^^^^

A symbolic traceback is a stack traceback in which procedure names are
associated with each code location.

Note that this feature is not supported on all platforms. See
:samp:`GNAT.Traceback.Symbolic` spec in :file:`g-trasym.ads` for a complete
list of currently supported platforms.

Note that the symbolic traceback requires that the program be compiled
with debug information. If it is not compiled with debug information
only the non-symbolic information will be valid.


.. rubric:: Tracebacks From Exception Occurrences

Here is an example:

  .. code-block:: ada

      with Ada.Text_IO;
      with GNAT.Traceback.Symbolic;

      procedure STB is

         procedure P1 is
         begin
            raise Constraint_Error;
         end P1;

         procedure P2 is
         begin
            P1;
         end P2;

         procedure P3 is
         begin
            P2;
         end P3;

      begin
         P3;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end STB;

  ::

      $ gnatmake -g .\stb -bargs -E
      $ stb

      0040149F in stb.p1 at stb.adb:8
      004014B7 in stb.p2 at stb.adb:13
      004014CF in stb.p3 at stb.adb:18
      004015DD in ada.stb at stb.adb:22
      00401461 in main at b~stb.adb:168
      004011C4 in __mingw_CRTStartup at crt1.c:200
      004011F1 in mainCRTStartup at crt1.c:222
      77E892A4 in ?? at ??:0

In the above example the ``.\`` syntax in the ``gnatmake`` command
is currently required by ``addr2line`` for files that are in
the current working directory.
Moreover, the exact sequence of linker options may vary from platform
to platform.
The above :switch:`-largs` section is for Windows platforms. By contrast,
under Unix there is no need for the :switch:`-largs` section.
Differences across platforms are due to details of linker implementation.


.. rubric:: Tracebacks From Anywhere in a Program

It is possible to get a symbolic stack traceback
from anywhere in a program, just as for non-symbolic tracebacks.
The first step is to obtain a non-symbolic
traceback, and then call ``Symbolic_Traceback`` to compute the symbolic
information. Here is an example:

  .. code-block:: ada

      with Ada.Text_IO;
      with GNAT.Traceback;
      with GNAT.Traceback.Symbolic;

      procedure STB is

         use Ada;
         use GNAT.Traceback;
         use GNAT.Traceback.Symbolic;

         procedure P1 is
            TB  : Tracebacks_Array (1 .. 10);
            --  We are asking for a maximum of 10 stack frames.
            Len : Natural;
            --  Len will receive the actual number of stack frames returned.
         begin
            Call_Chain (TB, Len);
            Text_IO.Put_Line (Symbolic_Traceback (TB (1 .. Len)));
         end P1;

         procedure P2 is
         begin
            P1;
         end P2;

      begin
         P2;
      end STB;


.. rubric:: Automatic Symbolic Tracebacks

Symbolic tracebacks may also be enabled by using the -Es switch to gnatbind (as
in ``gprbuild -g ... -bargs -Es``).
This will cause the Exception_Information to contain a symbolic traceback,
which will also be printed if an unhandled exception terminates the
program.


.. _Pretty-Printers_For_The_GNAT_Runtime:

Pretty-Printers for the GNAT runtime
------------------------------------

As discussed in :title:`Calling User-Defined Subprograms`, GDB's
``print`` command only knows about the physical layout of program data
structures and therefore normally displays only low-level dumps, which
are often hard to understand.

An example of this is when trying to display the contents of an Ada
standard container, such as ``Ada.Containers.Ordered_Maps.Map``:

  .. code-block:: ada

      with Ada.Containers.Ordered_Maps;

      procedure PP is
         package Int_To_Nat is
            new Ada.Containers.Ordered_Maps (Integer, Natural);

         Map : Int_To_Nat.Map;
      begin
         Map.Insert (1, 10);
         Map.Insert (2, 20);
         Map.Insert (3, 30);

         Map.Clear; --  BREAK HERE
      end PP;

When this program is built with debugging information and run under
GDB up to the ``Map.Clear`` statement, trying to print ``Map`` will
yield information that is only relevant to the developers of our standard
containers:

  ::

      (gdb) print map
      $1 = (
        tree => (
          first => 0x64e010,
          last => 0x64e070,
          root => 0x64e040,
          length => 3,
          tc => (
            busy => 0,
            lock => 0
          )
        )
      )

Fortunately, GDB has a feature called `pretty-printers
<http://docs.adacore.com/gdb-docs/html/gdb.html#Pretty_002dPrinter-Introduction>`_,
which allows customizing how GDB displays data structures. The GDB
shipped with GNAT embeds such pretty-printers for the most common
containers in the standard library.  To enable them, either run the
following command manually under GDB or add it to your ``.gdbinit`` file:

  ::

      python import gnatdbg; gnatdbg.setup()

Once this is done, GDB's ``print`` command will automatically use
these pretty-printers when appropriate. Using the previous example:

  ::

      (gdb) print map
      $1 = pp.int_to_nat.map of length 3 = {
        [1] = 10,
        [2] = 20,
        [3] = 30
      }

Pretty-printers are invoked each time GDB tries to display a value,
including when displaying the arguments of a called subprogram (in
GDB's ``backtrace`` command) or when printing the value returned by a
function (in GDB's ``finish`` command).

To display a value without involving pretty-printers, ``print`` can be
invoked with its ``/r`` option:

  ::

      (gdb) print/r map
      $1 = (
        tree => (...

Finer control of pretty-printers is also possible: see `GDB's online
documentation
<http://docs.adacore.com/gdb-docs/html/gdb.html#Pretty_002dPrinter-Commands>`_
for more information.


.. index:: Profiling


.. _Profiling:

Profiling
=========

This section describes how to use the the ``gprof`` profiler tool on Ada
programs.

.. index:: !  gprof
.. index:: Profiling

.. _Profiling_an_Ada_Program_with_gprof:

Profiling an Ada Program with gprof
-----------------------------------

This section is not meant to be an exhaustive documentation of ``gprof``.
Full documentation for it can be found in the :title:`GNU Profiler User's Guide`
documentation that is part of this GNAT distribution.

Profiling a program helps determine the parts of a program that are executed
most often, and are therefore the most time-consuming.

``gprof`` is the standard GNU profiling tool; it has been enhanced to
better handle Ada programs and multitasking.
It is currently supported on the following platforms

* linux x86/x86_64
* windows x86

In order to profile a program using ``gprof``, several steps are needed:

#. Instrument the code, which requires a full recompilation of the project with the
   proper switches.

#. Execute the program under the analysis conditions, i.e. with the desired
   input.

#. Analyze the results using the ``gprof`` tool.

The following sections detail the different steps, and indicate how
to interpret the results.


.. _Compilation_for_profiling:

Compilation for profiling
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: -pg (gcc), for profiling
.. index:: -pg (gnatlink), for profiling

In order to profile a program the first step is to tell the compiler
to generate the necessary profiling information. The compiler switch to be used
is ``-pg``, which must be added to other compilation switches. This
switch needs to be specified both during compilation and link stages, and can
be specified once when using gnatmake:

  ::

     $ gnatmake -f -pg -P my_project

Note that only the objects that were compiled with the ``-pg`` switch will
be profiled; if you need to profile your whole project, use the ``-f``
gnatmake switch to force full recompilation.

.. _Program_execution:


Program execution
^^^^^^^^^^^^^^^^^

Once the program has been compiled for profiling, you can run it as usual.

The only constraint imposed by profiling is that the program must terminate
normally. An interrupted program (via a Ctrl-C, kill, etc.) will not be
properly analyzed.

Once the program completes execution, a data file called :file:`gmon.out` is
generated in the directory where the program was launched from. If this file
already exists, it will be overwritten.


.. _Running_gprof:

Running gprof
^^^^^^^^^^^^^

The ``gprof`` tool is called as follow:

  ::

     $ gprof my_prog gmon.out

or simply:

  ::

    $  gprof my_prog

The complete form of the gprof command line is the following:

  ::

     $ gprof [switches] [executable [data-file]]

``gprof`` supports numerous switches. The order of these
switch does not matter. The full list of options can be found in
the GNU Profiler User's Guide documentation that comes with this documentation.

The following is the subset of those switches that is most relevant:

.. index:: --demangle (gprof)

:samp:`--demangle[={style}]`, :samp:`--no-demangle`
  These options control whether symbol names should be demangled when
  printing output.  The default is to demangle C++ symbols.  The
  ``--no-demangle`` option may be used to turn off demangling. Different
  compilers have different mangling styles.  The optional demangling style
  argument can be used to choose an appropriate demangling style for your
  compiler, in particular Ada symbols generated by GNAT can be demangled using
  ``--demangle=gnat``.


.. index:: -e (gprof)

:samp:`-e {function_name}`
  The :samp:`-e {function}` option tells ``gprof`` not to print
  information about the function ``function_name`` (and its
  children...) in the call graph.  The function will still be listed
  as a child of any functions that call it, but its index number will be
  shown as ``[not printed]``.  More than one ``-e`` option may be
  given; only one ``function_name`` may be indicated with each ``-e``
  option.


.. index:: -E (gprof)

:samp:`-E {function_name}`
  The :samp:`-E {function}` option works like the ``-e`` option, but
  execution time spent in the function (and children who were not called from
  anywhere else), will not be used to compute the percentages-of-time for
  the call graph.  More than one :switch:`-E` option may be given; only one
  ``function_name`` may be indicated with each :switch:`-E`` option.


.. index:: -f (gprof)

:samp:`-f {function_name}`
  The :samp:`-f {function}` option causes ``gprof`` to limit the
  call graph to the function ``function_name`` and its children (and
  their children...).  More than one ``-f`` option may be given;
  only one ``function_name`` may be indicated with each ``-f``
  option.


.. index:: -F (gprof)

:samp:`-F {function_name}`
  The :samp:`-F {function}` option works like the ``-f`` option, but
  only time spent in the function and its children (and their
  children...) will be used to determine total-time and
  percentages-of-time for the call graph.  More than one ``-F`` option
  may be given; only one ``function_name`` may be indicated with each
  ``-F`` option.  The ``-F`` option overrides the ``-E`` option.


.. _Interpretation_of_profiling_results:

Interpretation of profiling results
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The results of the profiling analysis are represented by two arrays: the
'flat profile' and the 'call graph'. Full documentation of those outputs
can be found in the GNU Profiler User's Guide.

The flat profile shows the time spent in each function of the program, and how
many time it has been called. This allows you to locate easily the most
time-consuming functions.

The call graph shows, for each subprogram, the subprograms that call it,
and the subprograms that it calls. It also provides an estimate of the time
spent in each of those callers/called subprograms.



.. _Improving_Performance:

Improving Performance
=====================

.. index:: Improving performance

This section presents several topics related to program performance.
It first describes some of the tradeoffs that need to be considered
and some of the techniques for making your program run faster.

.. only:: PRO or GPL

   It then documents the unused subprogram/data elimination feature
   and the ``gnatelim`` tool,
   which can reduce the size of program executables.


.. only:: FSF

   It then documents the unused subprogram/data elimination feature,
   which can reduce the size of program executables.


.. _Performance_Considerations:

Performance Considerations
--------------------------

The GNAT system provides a number of options that allow a trade-off
between

* performance of the generated code

* speed of compilation

* minimization of dependences and recompilation

* the degree of run-time checking.

The defaults (if no options are selected) aim at improving the speed
of compilation and minimizing dependences, at the expense of performance
of the generated code:

* no optimization

* no inlining of subprogram calls

* all run-time checks enabled except overflow and elaboration checks

These options are suitable for most program development purposes. This
section describes how you can modify these choices, and also provides
some guidelines on debugging optimized code.


.. _Controlling_Run-Time_Checks:

Controlling Run-Time Checks
^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, GNAT generates all run-time checks, except stack overflow
checks, and checks for access before elaboration on subprogram
calls. The latter are not required in default mode, because all
necessary checking is done at compile time.

.. index:: -gnatp (gcc)
.. index:: -gnato (gcc)

The gnat switch, :switch:`-gnatp` allows this default to be modified. See
:ref:`Run-Time_Checks`.

Our experience is that the default is suitable for most development
purposes.

Elaboration checks are off by default, and also not needed by default, since
GNAT uses a static elaboration analysis approach that avoids the need for
run-time checking. This manual contains a full chapter discussing the issue
of elaboration checks, and if the default is not satisfactory for your use,
you should read this chapter.

For validity checks, the minimal checks required by the Ada Reference
Manual (for case statements and assignments to array elements) are on
by default. These can be suppressed by use of the :switch:`-gnatVn` switch.
Note that in Ada 83, there were no validity checks, so if the Ada 83 mode
is acceptable (or when comparing GNAT performance with an Ada 83 compiler),
it may be reasonable to routinely use :switch:`-gnatVn`. Validity checks
are also suppressed entirely if :switch:`-gnatp` is used.

.. index:: Overflow checks
.. index:: Checks, overflow

.. index:: Suppress
.. index:: Unsuppress
.. index:: pragma Suppress
.. index:: pragma Unsuppress

Note that the setting of the switches controls the default setting of
the checks. They may be modified using either ``pragma Suppress`` (to
remove checks) or ``pragma Unsuppress`` (to add back suppressed
checks) in the program source.


.. _Use_of_Restrictions:

Use of Restrictions
^^^^^^^^^^^^^^^^^^^

The use of pragma Restrictions allows you to control which features are
permitted in your program. Apart from the obvious point that if you avoid
relatively expensive features like finalization (enforceable by the use
of pragma Restrictions (No_Finalization), the use of this pragma does not
affect the generated code in most cases.

One notable exception to this rule is that the possibility of task abort
results in some distributed overhead, particularly if finalization or
exception handlers are used. The reason is that certain sections of code
have to be marked as non-abortable.

If you use neither the ``abort`` statement, nor asynchronous transfer
of control (``select ... then abort``), then this distributed overhead
is removed, which may have a general positive effect in improving
overall performance.  Especially code involving frequent use of tasking
constructs and controlled types will show much improved performance.
The relevant restrictions pragmas are

  .. code-block:: ada

      pragma Restrictions (No_Abort_Statements);
      pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

It is recommended that these restriction pragmas be used if possible. Note
that this also means that you can write code without worrying about the
possibility of an immediate abort at any point.


.. _Optimization_Levels:

Optimization Levels
^^^^^^^^^^^^^^^^^^^

.. index:: -O (gcc)

Without any optimization option,
the compiler's goal is to reduce the cost of
compilation and to make debugging produce the expected results.
Statements are independent: if you stop the program with a breakpoint between
statements, you can then assign a new value to any variable or change
the program counter to any other statement in the subprogram and get exactly
the results you would expect from the source code.

Turning on optimization makes the compiler attempt to improve the
performance and/or code size at the expense of compilation time and
possibly the ability to debug the program.

If you use multiple
-O options, with or without level numbers,
the last such option is the one that is effective.

The default is optimization off. This results in the fastest compile
times, but GNAT makes absolutely no attempt to optimize, and the
generated programs are considerably larger and slower than when
optimization is enabled. You can use the
:switch:`-O` switch (the permitted forms are :switch:`-O0`, :switch:`-O1`
:switch:`-O2`, :switch:`-O3`, and :switch:`-Os`)
to ``gcc`` to control the optimization level:


* :switch:`-O0`
    No optimization (the default);
    generates unoptimized code but has
    the fastest compilation time.

    Note that many other compilers do substantial optimization even
    if 'no optimization' is specified. With gcc, it is very unusual
    to use :switch:`-O0` for production if execution time is of any concern,
    since :switch:`-O0` means (almost) no optimization. This difference
    between gcc and other compilers should be kept in mind when
    doing performance comparisons.

* :switch:`-O1`
    Moderate optimization;
    optimizes reasonably well but does not
    degrade compilation time significantly.

* :switch:`-O2`
    Full optimization;
    generates highly optimized code and has
    the slowest compilation time.

* :switch:`-O3`
    Full optimization as in :switch:`-O2`;
    also uses more aggressive automatic inlining of subprograms within a unit
    (:ref:`Inlining_of_Subprograms`) and attempts to vectorize loops.


* :switch:`-Os`
    Optimize space usage (code and data) of resulting program.

Higher optimization levels perform more global transformations on the
program and apply more expensive analysis algorithms in order to generate
faster and more compact code. The price in compilation time, and the
resulting improvement in execution time,
both depend on the particular application and the hardware environment.
You should experiment to find the best level for your application.

Since the precise set of optimizations done at each level will vary from
release to release (and sometime from target to target), it is best to think
of the optimization settings in general terms.
See the *Options That Control Optimization* section in
:title:`Using the GNU Compiler Collection (GCC)`
for details about
the :switch:`-O` settings and a number of :switch:`-f` options that
individually enable or disable specific optimizations.

Unlike some other compilation systems, ``gcc`` has
been tested extensively at all optimization levels. There are some bugs
which appear only with optimization turned on, but there have also been
bugs which show up only in *unoptimized* code. Selecting a lower
level of optimization does not improve the reliability of the code
generator, which in practice is highly reliable at all optimization
levels.

Note regarding the use of :switch:`-O3`: The use of this optimization level
ought not to be automatically preferred over that of level :switch:`-O2`,
since it often results in larger executables which may run more slowly.
See further discussion of this point in :ref:`Inlining_of_Subprograms`.


.. _Debugging_Optimized_Code:

Debugging Optimized Code
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Debugging optimized code
.. index:: Optimization and debugging

Although it is possible to do a reasonable amount of debugging at
nonzero optimization levels,
the higher the level the more likely that
source-level constructs will have been eliminated by optimization.
For example, if a loop is strength-reduced, the loop
control variable may be completely eliminated and thus cannot be
displayed in the debugger.
This can only happen at :switch:`-O2` or :switch:`-O3`.
Explicit temporary variables that you code might be eliminated at
level :switch:`-O1` or higher.

.. index:: -g (gcc)

The use of the :switch:`-g` switch,
which is needed for source-level debugging,
affects the size of the program executable on disk,
and indeed the debugging information can be quite large.
However, it has no effect on the generated code (and thus does not
degrade performance)

Since the compiler generates debugging tables for a compilation unit before
it performs optimizations, the optimizing transformations may invalidate some
of the debugging data.  You therefore need to anticipate certain
anomalous situations that may arise while debugging optimized code.
These are the most common cases:

* *The 'hopping Program Counter':*  Repeated ``step`` or ``next``
  commands show
  the PC bouncing back and forth in the code.  This may result from any of
  the following optimizations:

  - *Common subexpression elimination:* using a single instance of code for a
    quantity that the source computes several times.  As a result you
    may not be able to stop on what looks like a statement.

  - *Invariant code motion:* moving an expression that does not change within a
    loop, to the beginning of the loop.

  - *Instruction scheduling:* moving instructions so as to
    overlap loads and stores (typically) with other code, or in
    general to move computations of values closer to their uses. Often
    this causes you to pass an assignment statement without the assignment
    happening and then later bounce back to the statement when the
    value is actually needed.  Placing a breakpoint on a line of code
    and then stepping over it may, therefore, not always cause all the
    expected side-effects.

* *The 'big leap':* More commonly known as *cross-jumping*, in which
  two identical pieces of code are merged and the program counter suddenly
  jumps to a statement that is not supposed to be executed, simply because
  it (and the code following) translates to the same thing as the code
  that *was* supposed to be executed.  This effect is typically seen in
  sequences that end in a jump, such as a ``goto``, a ``return``, or
  a ``break`` in a C ``switch`` statement.

* *The 'roving variable':* The symptom is an unexpected value in a variable.
  There are various reasons for this effect:

  - In a subprogram prologue, a parameter may not yet have been moved to its
    'home'.

  - A variable may be dead, and its register re-used.  This is
    probably the most common cause.

  - As mentioned above, the assignment of a value to a variable may
    have been moved.

  - A variable may be eliminated entirely by value propagation or
    other means.  In this case, GCC may incorrectly generate debugging
    information for the variable

  In general, when an unexpected value appears for a local variable or parameter
  you should first ascertain if that value was actually computed by
  your program, as opposed to being incorrectly reported by the debugger.
  Record fields or
  array elements in an object designated by an access value
  are generally less of a problem, once you have ascertained that the access
  value is sensible.
  Typically, this means checking variables in the preceding code and in the
  calling subprogram to verify that the value observed is explainable from other
  values (one must apply the procedure recursively to those
  other values); or re-running the code and stopping a little earlier
  (perhaps before the call) and stepping to better see how the variable obtained
  the value in question; or continuing to step *from* the point of the
  strange value to see if code motion had simply moved the variable's
  assignments later.

In light of such anomalies, a recommended technique is to use :switch:`-O0`
early in the software development cycle, when extensive debugging capabilities
are most needed, and then move to :switch:`-O1` and later :switch:`-O2` as
the debugger becomes less critical.
Whether to use the :switch:`-g` switch in the release version is
a release management issue.
Note that if you use :switch:`-g` you can then use the ``strip`` program
on the resulting executable,
which removes both debugging information and global symbols.


.. _Inlining_of_Subprograms:

Inlining of Subprograms
^^^^^^^^^^^^^^^^^^^^^^^

A call to a subprogram in the current unit is inlined if all the
following conditions are met:

* The optimization level is at least :switch:`-O1`.

* The called subprogram is suitable for inlining: It must be small enough
  and not contain something that ``gcc`` cannot support in inlined
  subprograms.

  .. index:: pragma Inline
  .. index:: Inline

* Any one of the following applies: ``pragma Inline`` is applied to the
  subprogram; the subprogram is local to the unit and called once from
  within it; the subprogram is small and optimization level :switch:`-O2` is
  specified; optimization level :switch:`-O3` is specified.

Calls to subprograms in |withed| units are normally not inlined.
To achieve actual inlining (that is, replacement of the call by the code
in the body of the subprogram), the following conditions must all be true:

* The optimization level is at least :switch:`-O1`.

* The called subprogram is suitable for inlining: It must be small enough
  and not contain something that ``gcc`` cannot support in inlined
  subprograms.

* There is a ``pragma Inline`` for the subprogram.

* The :switch:`-gnatn` switch is used on the command line.

Even if all these conditions are met, it may not be possible for
the compiler to inline the call, due to the length of the body,
or features in the body that make it impossible for the compiler
to do the inlining.

Note that specifying the :switch:`-gnatn` switch causes additional
compilation dependencies. Consider the following:

  .. code-block:: ada

      package R is
         procedure Q;
         pragma Inline (Q);
      end R;
      package body R is
         ...
      end R;

      with R;
      procedure Main is
      begin
         ...
         R.Q;
      end Main;

With the default behavior (no :switch:`-gnatn` switch specified), the
compilation of the ``Main`` procedure depends only on its own source,
:file:`main.adb`, and the spec of the package in file :file:`r.ads`. This
means that editing the body of ``R`` does not require recompiling
``Main``.

On the other hand, the call ``R.Q`` is not inlined under these
circumstances. If the :switch:`-gnatn` switch is present when ``Main``
is compiled, the call will be inlined if the body of ``Q`` is small
enough, but now ``Main`` depends on the body of ``R`` in
:file:`r.adb` as well as on the spec. This means that if this body is edited,
the main program must be recompiled. Note that this extra dependency
occurs whether or not the call is in fact inlined by ``gcc``.

The use of front end inlining with :switch:`-gnatN` generates similar
additional dependencies.

.. index:: -fno-inline (gcc)

Note: The :switch:`-fno-inline` switch overrides all other conditions and ensures that
no inlining occurs, unless requested with pragma Inline_Always for ``gcc``
back-ends. The extra dependences resulting from :switch:`-gnatn` will still be active,
even if this switch is used to suppress the resulting inlining actions.

.. index:: -fno-inline-functions (gcc)

Note: The :switch:`-fno-inline-functions` switch can be used to prevent
automatic inlining of subprograms if :switch:`-O3` is used.

.. index:: -fno-inline-small-functions (gcc)

Note: The :switch:`-fno-inline-small-functions` switch can be used to prevent
automatic inlining of small subprograms if :switch:`-O2` is used.

.. index:: -fno-inline-functions-called-once (gcc)

Note: The :switch:`-fno-inline-functions-called-once` switch
can be used to prevent inlining of subprograms local to the unit
and called once from within it if :switch:`-O1` is used.

Note regarding the use of :switch:`-O3`: :switch:`-gnatn` is made up of two
sub-switches :switch:`-gnatn1` and :switch:`-gnatn2` that can be directly
specified in lieu of it, :switch:`-gnatn` being translated into one of them
based on the optimization level. With :switch:`-O2` or below, :switch:`-gnatn`
is equivalent to :switch:`-gnatn1` which activates pragma ``Inline`` with
moderate inlining across modules. With :switch:`-O3`, :switch:`-gnatn` is
equivalent to :switch:`-gnatn2` which activates pragma ``Inline`` with
full inlining across modules. If you have used pragma ``Inline`` in
appropriate cases, then it is usually much better to use :switch:`-O2`
and :switch:`-gnatn` and avoid the use of :switch:`-O3` which has the additional
effect of inlining subprograms you did not think should be inlined. We have
found that the use of :switch:`-O3` may slow down the compilation and increase
the code size by performing excessive inlining, leading to increased
instruction cache pressure from the increased code size and thus minor
performance improvements. So the bottom line here is that you should not
automatically assume that :switch:`-O3` is better than :switch:`-O2`, and
indeed you should use :switch:`-O3` only if tests show that it actually
improves performance for your program.

.. _Floating_Point_Operations:

Floating_Point_Operations
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Floating-Point Operations

On almost all targets, GNAT maps Float and Long_Float to the 32-bit and
64-bit standard IEEE floating-point representations, and operations will
use standard IEEE arithmetic as provided by the processor. On most, but
not all, architectures, the attribute Machine_Overflows is False for these
types, meaning that the semantics of overflow is implementation-defined.
In the case of GNAT, these semantics correspond to the normal IEEE
treatment of infinities and NaN (not a number) values. For example,
1.0 / 0.0 yields plus infinitiy and 0.0 / 0.0 yields a NaN. By
avoiding explicit overflow checks, the performance is greatly improved
on many targets. However, if required, floating-point overflow can be
enabled by the use of the pragma Check_Float_Overflow.

Another consideration that applies specifically to x86 32-bit
architectures is which form of floating-point arithmetic is used.
By default the operations use the old style x86 floating-point,
which implements an 80-bit extended precision form (on these
architectures the type Long_Long_Float corresponds to that form).
In addition, generation of efficient code in this mode means that
the extended precision form will be used for intermediate results.
This may be helpful in improving the final precision of a complex
expression. However it means that the results obtained on the x86
will be different from those on other architectures, and for some
algorithms, the extra intermediate precision can be detrimental.

In addition to this old-style floating-point, all modern x86 chips
implement an alternative floating-point operation model referred
to as SSE2. In this model there is no extended form, and furthermore
execution performance is significantly enhanced. To force GNAT to use
this more modern form, use both of the switches:

   -msse2 -mfpmath=sse

A unit compiled with these switches will automatically use the more
efficient SSE2 instruction set for Float and Long_Float operations.
Note that the ABI has the same form for both floating-point models,
so it is permissible to mix units compiled with and without these
switches.





.. _Vectorization_of_loops:

Vectorization of loops
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Optimization Switches

You can take advantage of the auto-vectorizer present in the ``gcc``
back end to vectorize loops with GNAT.  The corresponding command line switch
is :switch:`-ftree-vectorize` but, as it is enabled by default at :switch:`-O3`
and other aggressive optimizations helpful for vectorization also are enabled
by default at this level, using :switch:`-O3` directly is recommended.

You also need to make sure that the target architecture features a supported
SIMD instruction set.  For example, for the x86 architecture, you should at
least specify :switch:`-msse2` to get significant vectorization (but you don't
need to specify it for x86-64 as it is part of the base 64-bit architecture).
Similarly, for the PowerPC architecture, you should specify :switch:`-maltivec`.

The preferred loop form for vectorization is the ``for`` iteration scheme.
Loops with a ``while`` iteration scheme can also be vectorized if they are
very simple, but the vectorizer will quickly give up otherwise.  With either
iteration scheme, the flow of control must be straight, in particular no
``exit`` statement may appear in the loop body.  The loop may however
contain a single nested loop, if it can be vectorized when considered alone:

  .. code-block:: ada

       A : array (1..4, 1..4) of Long_Float;
       S : array (1..4) of Long_Float;

       procedure Sum is
       begin
          for I in A'Range(1) loop
             for J in A'Range(2) loop
                S (I) := S (I) + A (I, J);
             end loop;
          end loop;
       end Sum;

The vectorizable operations depend on the targeted SIMD instruction set, but
the adding and some of the multiplying operators are generally supported, as
well as the logical operators for modular types. Note that compiling
with :switch:`-gnatp` might well reveal cases where some checks do thwart
vectorization.

Type conversions may also prevent vectorization if they involve semantics that
are not directly supported by the code generator or the SIMD instruction set.
A typical example is direct conversion from floating-point to integer types.
The solution in this case is to use the following idiom:

  .. code-block:: ada

       Integer (S'Truncation (F))

if ``S`` is the subtype of floating-point object ``F``.

In most cases, the vectorizable loops are loops that iterate over arrays.
All kinds of array types are supported, i.e. constrained array types with
static bounds:

  .. code-block:: ada

       type Array_Type is array (1 .. 4) of Long_Float;

constrained array types with dynamic bounds:


  .. code-block:: ada

     type Array_Type is array (1 .. Q.N) of Long_Float;

     type Array_Type is array (Q.K .. 4) of Long_Float;

     type Array_Type is array (Q.K .. Q.N) of Long_Float;

or unconstrained array types:

  .. code-block:: ada

      type Array_Type is array (Positive range <>) of Long_Float;

The quality of the generated code decreases when the dynamic aspect of the
array type increases, the worst code being generated for unconstrained array
types.  This is so because, the less information the compiler has about the
bounds of the array, the more fallback code it needs to generate in order to
fix things up at run time.

It is possible to specify that a given loop should be subject to vectorization
preferably to other optimizations by means of pragma ``Loop_Optimize``:

  .. code-block:: ada

      pragma Loop_Optimize (Vector);

placed immediately within the loop will convey the appropriate hint to the
compiler for this loop.

It is also possible to help the compiler generate better vectorized code
for a given loop by asserting that there are no loop-carried dependencies
in the loop.  Consider for example the procedure:

  .. code-block:: ada

      type Arr is array (1 .. 4) of Long_Float;

      procedure Add (X, Y : not null access Arr; R : not null access Arr) is
      begin
        for I in Arr'Range loop
          R(I) := X(I) + Y(I);
        end loop;
      end;

By default, the compiler cannot unconditionally vectorize the loop because
assigning to a component of the array designated by R in one iteration could
change the value read from the components of the array designated by X or Y
in a later iteration.  As a result, the compiler will generate two versions
of the loop in the object code, one vectorized and the other not vectorized,
as well as a test to select the appropriate version at run time.  This can
be overcome by another hint:

  .. code-block:: ada

     pragma Loop_Optimize (Ivdep);

placed immediately within the loop will tell the compiler that it can safely
omit the non-vectorized version of the loop as well as the run-time test.


.. _Other_Optimization_Switches:

Other Optimization Switches
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Optimization Switches

Since GNAT uses the ``gcc`` back end, all the specialized
``gcc`` optimization switches are potentially usable. These switches
have not been extensively tested with GNAT but can generally be expected
to work. Examples of switches in this category are :switch:`-funroll-loops`
and the various target-specific :switch:`-m` options (in particular, it has
been observed that :switch:`-march=xxx` can significantly improve performance
on appropriate machines). For full details of these switches, see
the *Submodel Options* section in the *Hardware Models and Configurations*
chapter of :title:`Using the GNU Compiler Collection (GCC)`.


.. _Optimization_and_Strict_Aliasing:

Optimization and Strict Aliasing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Aliasing
.. index:: Strict Aliasing
.. index:: No_Strict_Aliasing

The strong typing capabilities of Ada allow an optimizer to generate
efficient code in situations where other languages would be forced to
make worst case assumptions preventing such optimizations. Consider
the following example:

  .. code-block:: ada

     procedure R is
        type Int1 is new Integer;
        type Int2 is new Integer;
        type Int1A is access Int1;
        type Int2A is access Int2;
        Int1V : Int1A;
        Int2V : Int2A;
        ...

     begin
        ...
        for J in Data'Range loop
           if Data (J) = Int1V.all then
              Int2V.all := Int2V.all + 1;
           end if;
        end loop;
        ...
     end R;

In this example, since the variable ``Int1V`` can only access objects
of type ``Int1``, and ``Int2V`` can only access objects of type
``Int2``, there is no possibility that the assignment to
``Int2V.all`` affects the value of ``Int1V.all``. This means that
the compiler optimizer can "know" that the value ``Int1V.all`` is constant
for all iterations of the loop and avoid the extra memory reference
required to dereference it each time through the loop.

This kind of optimization, called strict aliasing analysis, is
triggered by specifying an optimization level of :switch:`-O2` or
higher or :switch:`-Os` and allows GNAT to generate more efficient code
when access values are involved.

However, although this optimization is always correct in terms of
the formal semantics of the Ada Reference Manual, difficulties can
arise if features like ``Unchecked_Conversion`` are used to break
the typing system. Consider the following complete program example:

  .. code-block:: ada

      package p1 is
         type int1 is new integer;
         type int2 is new integer;
         type a1 is access int1;
         type a2 is access int2;
      end p1;

      with p1; use p1;
      package p2 is
         function to_a2 (Input : a1) return a2;
      end p2;

      with Unchecked_Conversion;
      package body p2 is
         function to_a2 (Input : a1) return a2 is
            function to_a2u is
              new Unchecked_Conversion (a1, a2);
         begin
            return to_a2u (Input);
         end to_a2;
      end p2;

      with p2; use p2;
      with p1; use p1;
      with Text_IO; use Text_IO;
      procedure m is
         v1 : a1 := new int1;
         v2 : a2 := to_a2 (v1);
      begin
         v1.all := 1;
         v2.all := 0;
         put_line (int1'image (v1.all));
      end;

This program prints out 0 in :switch:`-O0` or :switch:`-O1`
mode, but it prints out 1 in :switch:`-O2` mode. That's
because in strict aliasing mode, the compiler can and
does assume that the assignment to ``v2.all`` could not
affect the value of ``v1.all``, since different types
are involved.

This behavior is not a case of non-conformance with the standard, since
the Ada RM specifies that an unchecked conversion where the resulting
bit pattern is not a correct value of the target type can result in an
abnormal value and attempting to reference an abnormal value makes the
execution of a program erroneous.  That's the case here since the result
does not point to an object of type ``int2``.  This means that the
effect is entirely unpredictable.

However, although that explanation may satisfy a language
lawyer, in practice an applications programmer expects an
unchecked conversion involving pointers to create true
aliases and the behavior of printing 1 seems plain wrong.
In this case, the strict aliasing optimization is unwelcome.

Indeed the compiler recognizes this possibility, and the
unchecked conversion generates a warning:

  ::

     p2.adb:5:07: warning: possible aliasing problem with type "a2"
     p2.adb:5:07: warning: use -fno-strict-aliasing switch for references
     p2.adb:5:07: warning:  or use "pragma No_Strict_Aliasing (a2);"

Unfortunately the problem is recognized when compiling the body of
package ``p2``, but the actual "bad" code is generated while
compiling the body of ``m`` and this latter compilation does not see
the suspicious ``Unchecked_Conversion``.

As implied by the warning message, there are approaches you can use to
avoid the unwanted strict aliasing optimization in a case like this.

One possibility is to simply avoid the use of :switch:`-O2`, but
that is a bit drastic, since it throws away a number of useful
optimizations that do not involve strict aliasing assumptions.

A less drastic approach is to compile the program using the
option :switch:`-fno-strict-aliasing`. Actually it is only the
unit containing the dereferencing of the suspicious pointer
that needs to be compiled. So in this case, if we compile
unit ``m`` with this switch, then we get the expected
value of zero printed. Analyzing which units might need
the switch can be painful, so a more reasonable approach
is to compile the entire program with options :switch:`-O2`
and :switch:`-fno-strict-aliasing`. If the performance is
satisfactory with this combination of options, then the
advantage is that the entire issue of possible "wrong"
optimization due to strict aliasing is avoided.

To avoid the use of compiler switches, the configuration
pragma ``No_Strict_Aliasing`` with no parameters may be
used to specify that for all access types, the strict
aliasing optimization should be suppressed.

However, these approaches are still overkill, in that they causes
all manipulations of all access values to be deoptimized. A more
refined approach is to concentrate attention on the specific
access type identified as problematic.

First, if a careful analysis of uses of the pointer shows
that there are no possible problematic references, then
the warning can be suppressed by bracketing the
instantiation of ``Unchecked_Conversion`` to turn
the warning off:

  .. code-block:: ada

     pragma Warnings (Off);
     function to_a2u is
       new Unchecked_Conversion (a1, a2);
     pragma Warnings (On);

Of course that approach is not appropriate for this particular
example, since indeed there is a problematic reference. In this
case we can take one of two other approaches.

The first possibility is to move the instantiation of unchecked
conversion to the unit in which the type is declared. In
this example, we would move the instantiation of
``Unchecked_Conversion`` from the body of package
``p2`` to the spec of package ``p1``. Now the
warning disappears. That's because any use of the
access type knows there is a suspicious unchecked
conversion, and the strict aliasing optimization
is automatically suppressed for the type.

If it is not practical to move the unchecked conversion to the same unit
in which the destination access type is declared (perhaps because the
source type is not visible in that unit), you may use pragma
``No_Strict_Aliasing`` for the type. This pragma must occur in the
same declarative sequence as the declaration of the access type:

  .. code-block:: ada

     type a2 is access int2;
     pragma No_Strict_Aliasing (a2);

Here again, the compiler now knows that the strict aliasing optimization
should be suppressed for any reference to type ``a2`` and the
expected behavior is obtained.

Finally, note that although the compiler can generate warnings for
simple cases of unchecked conversions, there are tricker and more
indirect ways of creating type incorrect aliases which the compiler
cannot detect. Examples are the use of address overlays and unchecked
conversions involving composite types containing access types as
components. In such cases, no warnings are generated, but there can
still be aliasing problems. One safe coding practice is to forbid the
use of address clauses for type overlaying, and to allow unchecked
conversion only for primitive types. This is not really a significant
restriction since any possible desired effect can be achieved by
unchecked conversion of access values.

The aliasing analysis done in strict aliasing mode can certainly
have significant benefits. We have seen cases of large scale
application code where the time is increased by up to 5% by turning
this optimization off. If you have code that includes significant
usage of unchecked conversion, you might want to just stick with
:switch:`-O1` and avoid the entire issue. If you get adequate
performance at this level of optimization level, that's probably
the safest approach. If tests show that you really need higher
levels of optimization, then you can experiment with :switch:`-O2`
and :switch:`-O2 -fno-strict-aliasing` to see how much effect this
has on size and speed of the code. If you really need to use
:switch:`-O2` with strict aliasing in effect, then you should
review any uses of unchecked conversion of access types,
particularly if you are getting the warnings described above.


.. _Aliased_Variables_and_Optimization:

Aliased Variables and Optimization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Aliasing

There are scenarios in which programs may
use low level techniques to modify variables
that otherwise might be considered to be unassigned. For example,
a variable can be passed to a procedure by reference, which takes
the address of the parameter and uses the address to modify the
variable's value, even though it is passed as an IN parameter.
Consider the following example:

  .. code-block:: ada

     procedure P is
        Max_Length : constant Natural := 16;
        type Char_Ptr is access all Character;

        procedure Get_String(Buffer: Char_Ptr; Size : Integer);
        pragma Import (C, Get_String, "get_string");

        Name : aliased String (1 .. Max_Length) := (others => ' ');
        Temp : Char_Ptr;

        function Addr (S : String) return Char_Ptr is
           function To_Char_Ptr is
             new Ada.Unchecked_Conversion (System.Address, Char_Ptr);
        begin
           return To_Char_Ptr (S (S'First)'Address);
        end;

     begin
        Temp := Addr (Name);
        Get_String (Temp, Max_Length);
     end;

where Get_String is a C function that uses the address in Temp to
modify the variable ``Name``. This code is dubious, and arguably
erroneous, and the compiler would be entitled to assume that
``Name`` is never modified, and generate code accordingly.

However, in practice, this would cause some existing code that
seems to work with no optimization to start failing at high
levels of optimzization.

What the compiler does for such cases is to assume that marking
a variable as aliased indicates that some "funny business" may
be going on. The optimizer recognizes the aliased keyword and
inhibits optimizations that assume the value cannot be assigned.
This means that the above example will in fact "work" reliably,
that is, it will produce the expected results.


.. _Atomic_Variables_and_Optimization:

Atomic Variables and Optimization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Atomic

There are two considerations with regard to performance when
atomic variables are used.

First, the RM only guarantees that access to atomic variables
be atomic, it has nothing to say about how this is achieved,
though there is a strong implication that this should not be
achieved by explicit locking code. Indeed GNAT will never
generate any locking code for atomic variable access (it will
simply reject any attempt to make a variable or type atomic
if the atomic access cannot be achieved without such locking code).

That being said, it is important to understand that you cannot
assume that the entire variable will always be accessed. Consider
this example:

  .. code-block:: ada

     type R is record
        A,B,C,D : Character;
     end record;
     for R'Size use 32;
     for R'Alignment use 4;

     RV : R;
     pragma Atomic (RV);
     X : Character;
     ...
     X := RV.B;

You cannot assume that the reference to ``RV.B``
will read the entire 32-bit
variable with a single load instruction. It is perfectly legitimate if
the hardware allows it to do a byte read of just the B field. This read
is still atomic, which is all the RM requires. GNAT can and does take
advantage of this, depending on the architecture and optimization level.
Any assumption to the contrary is non-portable and risky. Even if you
examine the assembly language and see a full 32-bit load, this might
change in a future version of the compiler.

If your application requires that all accesses to ``RV`` in this
example be full 32-bit loads, you need to make a copy for the access
as in:

  .. code-block:: ada

     declare
        RV_Copy : constant R := RV;
     begin
        X := RV_Copy.B;
     end;

Now the reference to RV must read the whole variable.
Actually one can imagine some compiler which figures
out that the whole copy is not required (because only
the B field is actually accessed), but GNAT
certainly won't do that, and we don't know of any
compiler that would not handle this right, and the
above code will in practice work portably across
all architectures (that permit the Atomic declaration).

The second issue with atomic variables has to do with
the possible requirement of generating synchronization
code. For more details on this, consult the sections on
the pragmas Enable/Disable_Atomic_Synchronization in the
GNAT Reference Manual. If performance is critical, and
such synchronization code is not required, it may be
useful to disable it.


.. _Passive_Task_Optimization:

Passive Task Optimization
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Passive Task

A passive task is one which is sufficiently simple that
in theory a compiler could recognize it an implement it
efficiently without creating a new thread. The original design
of Ada 83 had in mind this kind of passive task optimization, but
only a few Ada 83 compilers attempted it. The problem was that
it was difficult to determine the exact conditions under which
the optimization was possible. The result is a very fragile
optimization where a very minor change in the program can
suddenly silently make a task non-optimizable.

With the revisiting of this issue in Ada 95, there was general
agreement that this approach was fundamentally flawed, and the
notion of protected types was introduced. When using protected
types, the restrictions are well defined, and you KNOW that the
operations will be optimized, and furthermore this optimized
performance is fully portable.

Although it would theoretically be possible for GNAT to attempt to
do this optimization, but it really doesn't make sense in the
context of Ada 95, and none of the Ada 95 compilers implement
this optimization as far as we know. In particular GNAT never
attempts to perform this optimization.

In any new Ada 95 code that is written, you should always
use protected types in place of tasks that might be able to
be optimized in this manner.
Of course this does not help if you have legacy Ada 83 code
that depends on this optimization, but it is unusual to encounter
a case where the performance gains from this optimization
are significant.

Your program should work correctly without this optimization. If
you have performance problems, then the most practical
approach is to figure out exactly where these performance problems
arise, and update those particular tasks to be protected types. Note
that typically clients of the tasks who call entries, will not have
to be modified, only the task definition itself.


.. _Text_IO_Suggestions:

``Text_IO`` Suggestions
-----------------------

.. index:: Text_IO and performance

The ``Ada.Text_IO`` package has fairly high overheads due in part to
the requirement of maintaining page and line counts. If performance
is critical, a recommendation is to use ``Stream_IO`` instead of
``Text_IO`` for volume output, since this package has less overhead.

If ``Text_IO`` must be used, note that by default output to the standard
output and standard error files is unbuffered (this provides better
behavior when output statements are used for debugging, or if the
progress of a program is observed by tracking the output, e.g. by
using the Unix *tail -f* command to watch redirected output.

If you are generating large volumes of output with ``Text_IO`` and
performance is an important factor, use a designated file instead
of the standard output file, or change the standard output file to
be buffered using ``Interfaces.C_Streams.setvbuf``.


.. _Reducing_Size_of_Executables_with_Unused_Subprogram/Data_Elimination:

Reducing Size of Executables with Unused Subprogram/Data Elimination
--------------------------------------------------------------------

.. index:: Uunused subprogram/data elimination

This section describes how you can eliminate unused subprograms and data from
your executable just by setting options at compilation time.

.. _About_unused_subprogram/data_elimination:

About unused subprogram/data elimination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, an executable contains all code and data of its composing objects
(directly linked or coming from statically linked libraries), even data or code
never used by this executable.

This feature will allow you to eliminate such unused code from your
executable, making it smaller (in disk and in memory).

This functionality is available on all Linux platforms except for the IA-64
architecture and on all cross platforms using the ELF binary file format.
In both cases GNU binutils version 2.16 or later are required to enable it.

.. _Compilation_options:

Compilation options
^^^^^^^^^^^^^^^^^^^

The operation of eliminating the unused code and data from the final executable
is directly performed by the linker.

.. index:: -ffunction-sections (gcc)
.. index:: -fdata-sections (gcc)

In order to do this, it has to work with objects compiled with the
following options:
:switch:`-ffunction-sections` :switch:`-fdata-sections`.

These options are usable with C and Ada files.
They will place respectively each
function or data in a separate section in the resulting object file.

Once the objects and static libraries are created with these options, the
linker can perform the dead code elimination. You can do this by setting
the :switch:`-Wl,--gc-sections` option to gcc command or in the
:switch:`-largs` section of ``gnatmake``. This will perform a
garbage collection of code and data never referenced.

If the linker performs a partial link (:switch:`-r` linker option), then you
will need to provide the entry point using the :switch:`-e` / :switch:`--entry`
linker option.

Note that objects compiled without the :switch:`-ffunction-sections` and
:switch:`-fdata-sections` options can still be linked with the executable.
However, no dead code elimination will be performed on those objects (they will
be linked as is).

The GNAT static library is now compiled with -ffunction-sections and
-fdata-sections on some platforms. This allows you to eliminate the unused code
and data of the GNAT library from your executable.


.. _Example_of_unused_subprogram/data_elimination:

Example of unused subprogram/data elimination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here is a simple example:

  .. code-block:: ada

     with Aux;

     procedure Test is
     begin
        Aux.Used (10);
     end Test;

     package Aux is
        Used_Data   : Integer;
        Unused_Data : Integer;

        procedure Used   (Data : Integer);
        procedure Unused (Data : Integer);
     end Aux;

     package body Aux is
        procedure Used (Data : Integer) is
        begin
           Used_Data := Data;
        end Used;

        procedure Unused (Data : Integer) is
        begin
           Unused_Data := Data;
        end Unused;
     end Aux;

``Unused`` and ``Unused_Data`` are never referenced in this code
excerpt, and hence they may be safely removed from the final executable.

  ::

     $ gnatmake test

     $ nm test | grep used
     020015f0 T aux__unused
     02005d88 B aux__unused_data
     020015cc T aux__used
     02005d84 B aux__used_data

     $ gnatmake test -cargs -fdata-sections -ffunction-sections \\
          -largs -Wl,--gc-sections

     $ nm test | grep used
     02005350 T aux__used
     0201ffe0 B aux__used_data

It can be observed that the procedure ``Unused`` and the object
``Unused_Data`` are removed by the linker when using the
appropriate options.

.. only:: PRO or GPL

  .. _Reducing_Size_of_Ada_Executables_with_gnatelim:

  Reducing Size of Ada Executables with ``gnatelim``
  --------------------------------------------------

  .. index:: gnatelim

  This section describes ``gnatelim``, a tool which detects unused
  subprograms and helps the compiler to create a smaller executable for your
  program.

  ``gnatelim`` is a project-aware tool.
  (See :ref:`Using_Project_Files_with_GNAT_Tools` for a description of
  the project-related switches but note that ``gnatelim`` does not support
  the :samp:`-U {main_unit}`, :samp:`--subdirs={dir}`, or
  :samp:`--no_objects_dir` switches.)
  The project file package that can specify
  ``gnatelim`` switches is named ``Eliminate``.

  .. _About_gnatelim:

  About ``gnatelim``
  ^^^^^^^^^^^^^^^^^^

  When a program shares a set of Ada
  packages with other programs, it may happen that this program uses
  only a fraction of the subprograms defined in these packages. The code
  created for these unused subprograms increases the size of the executable.

  ``gnatelim`` tracks unused subprograms in an Ada program and
  outputs a list of GNAT-specific pragmas ``Eliminate`` marking all the
  subprograms that are declared but never called. By placing the list of
  ``Eliminate`` pragmas in the GNAT configuration file :file:`gnat.adc` and
  recompiling your program, you may decrease the size of its executable,
  because the compiler will not generate the code for 'eliminated' subprograms.
  See ``Pragma_Eliminate`` in the :title:`GNAT_Reference_Manual` for more
  information about this pragma.

  ``gnatelim`` needs as its input data the name of the main subprogram.

  If a set of source files is specified as ``gnatelim`` arguments, it
  treats these files as a complete set of sources making up a program to
  analyse, and analyses only these sources.

  If ``gnatelim`` is called with a project file and :samp:`-U` option is
  used, then in process all the files from the argument project but
  not just the closure of the main subprogram.

  In all the other cases (that are typical cases of ``gnatelim`` usage, when
  the only ``gnatelim`` parameter is the name of the source file containing
  the main subprogram) gnatelim needs the full closure of the main subprogram.
  When called with a project file, gnatelim computes this closure itself.
  Otherwise it assumes that it can reuse the results of the previous
  build of the main subprogram.

  If the set of sources to be processed by ``gnatelim`` contains sources with
  preprocessing directives
  then the needed options should be provided to run preprocessor as a part of
  the ``gnatelim`` call, and the generated set of pragmas ``Eliminate``
  will correspond to preprocessed sources.


  .. _Running_gnatelim:

  Running ``gnatelim``
  ^^^^^^^^^^^^^^^^^^^^

  ``gnatelim`` has the following command-line interface:


    ::

        $ gnatelim [switches] -main=`main_unit_name {filename} [-cargs gcc_switches]

  ``main_unit_name`` should be a name of a source file that contains the main
  subprogram of a program (partition).

  Each ``filename`` is the name (including the extension) of a source
  file to process. 'Wildcards' are allowed, and
  the file name may contain path information.

  ``gcc_switches`` is a list of switches for
  ``gcc``. They will be passed on to all compiler invocations made by
  ``gnatelim`` to generate the ASIS trees. Here you can provide
  :switch:`-I` switches to form the source search path,
  use the :switch:`-gnatec` switch to set the configuration file,
  use the :switch:`-gnat05` switch if sources should be compiled in
  Ada 2005 mode etc.

  ``gnatelim`` has the following switches:


  .. index:: --version (gnatelim)

  :samp:`--version`
    Display Copyright and version, then exit disregarding all other options.


  .. index:: --help (gnatelim)

  :samp:`--help`
    Display usage, then exit disregarding all other options.


  .. index:: -P (gnatelim)

  :samp:`-P {file}`
    Indicates the name of the project file that describes the set of sources
    to be processed.


  .. index:: -X (gnatelim)

  :samp:`-X{name}={value}`
    Indicates that external variable ``name`` in the argument project
    has the value ``value``. Has no effect if no project is specified as
    tool argument.


  .. index:: --RTS (gnatelim)

  :samp:`--RTS={rts-path}`
    Specifies the default location of the runtime library. Same meaning as the
    equivalent ``gnatmake`` flag (:ref:`Switches_for_gnatmake`).


  .. index:: -U (gnatelim)

  :samp:`-U`
    Process all the sources from the argument project. If no project file
    is specified, this option has no effect. If this option is used with the
    project file, ``gnatelim`` does not require the preliminary build of the
    argument main subprogram.


  .. index:: -files (gnatelim)

  :samp:`-files={filename}`
    Take the argument source files from the specified file. This file should be an
    ordinary text file containing file names separated by spaces or
    line breaks. You can use this switch more than once in the same call to
    ``gnatelim``. You also can combine this switch with
    an explicit list of files.


  .. index:: -log (gnatelim)

  :samp:`-log`
    Duplicate all the output sent to :file:`stderr` into a log file. The log file
    is named :file:`gnatelim.log` and is located in the current directory.

    .. index:: --no-elim-dispatch (gnatelim)

  :samp:`--no-elim-dispatch`
    Do not generate pragmas for dispatching operations.


  .. index:: --ignore (gnatelim)

  :samp:`--ignore={filename}`
    Do not generate pragmas for subprograms declared in the sources
    listed in a specified file

  .. index:: -o (gnatelim)


  :samp:`-o={report_file}`
    Put ``gnatelim`` output into a specified file. If this file already exists,
    it is overridden. If this switch is not used, ``gnatelim`` outputs its results
    into :file:`stderr`


  .. index:: -j (gnatelim)

  :samp:`-j{n}`
    Use ``n`` processes to carry out the tree creations (internal representations
    of the argument sources). On a multiprocessor machine this speeds up processing
    of big sets of argument sources. If ``n`` is 0, then the maximum number of
    parallel tree creations is the number of core processors on the platform.
    This possibility is disabled if ``gnatelim`` has to compute the closure
    of the main unit.


  .. index:: -q (gnatelim)

  :samp:`-q`
    Quiet mode: by default ``gnatelim`` outputs to the standard error
    stream the number of program units left to be processed. This option turns
    this trace off.

  .. index:: -t (gnatelim)


  :samp:`-t`
    Print out execution time.


  .. index:: -v (gnatelim)

  :samp:`-v`
    Verbose mode: ``gnatelim`` version information is printed as Ada
    comments to the standard output stream. Also, in addition to the number of
    program units left ``gnatelim`` will output the name of the current unit
    being processed.


  .. index:: -wq (gnatelim)

  :samp:`-wq`
    Quiet warning mode - some warnings are suppressed. In particular warnings that
    indicate that the analysed set of sources is incomplete to make up a
    partition and that some subprogram bodies are missing are not generated.



  .. _Processing_Precompiled_Libraries:

  Processing Precompiled Libraries
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  If some program uses a precompiled Ada library, it can be processed by
  ``gnatelim`` in a usual way. ``gnatelim`` will newer generate an
  Eliminate pragma for a subprogram if the body of this subprogram has not
  been analysed, this is a typical case for subprograms from precompiled
  libraries. Switch :switch:`-wq` may be used to suppress
  warnings about missing source files and non-analyzed subprogram bodies
  that can be generated when processing precompiled Ada libraries.


  .. _Correcting_the_List_of_Eliminate_Pragmas:

  Correcting the List of Eliminate Pragmas
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  In some rare cases ``gnatelim`` may try to eliminate
  subprograms that are actually called in the program. In this case, the
  compiler will generate an error message of the form:

    ::

        main.adb:4:08: cannot reference subprogram "P" eliminated at elim.out:5

  You will need to manually remove the wrong ``Eliminate`` pragmas from
  the configuration file indicated in the error message. You should recompile
  your program from scratch after that, because you need a consistent
  configuration file(s) during the entire compilation.

  If ``gnatelim`` is called with a project file and with ``-U`` option
  the generated set of pragmas may contain pragmas for subprograms that
  does not belong to the closure of the argument main subprogram. These
  pragmas has no effect when the set of pragmas is used to reduce the size
  of executable.


.. index:: Overflow checks
.. index:: Checks (overflow)

.. _Overflow_Check_Handling_in_GNAT:

Overflow Check Handling in GNAT
===============================

This section explains how to control the handling of overflow checks.

.. _Background:

Background
----------

Overflow checks are checks that the compiler may make to ensure
that intermediate results are not out of range. For example:

  .. code-block:: ada

     A : Integer;
     ...
     A := A + 1;

If ``A`` has the value ``Integer'Last``, then the addition may cause
overflow since the result is out of range of the type ``Integer``.
In this case ``Constraint_Error`` will be raised if checks are
enabled.

A trickier situation arises in examples like the following:

  .. code-block:: ada

     A, C : Integer;
     ...
     A := (A + 1) + C;

where ``A`` is ``Integer'Last`` and ``C`` is ``-1``.
Now the final result of the expression on the right hand side is
``Integer'Last`` which is in range, but the question arises whether the
intermediate addition of ``(A + 1)`` raises an overflow error.

The (perhaps surprising) answer is that the Ada language
definition does not answer this question. Instead it leaves
it up to the implementation to do one of two things if overflow
checks are enabled.

* raise an exception (``Constraint_Error``), or

* yield the correct mathematical result which is then used in
  subsequent operations.

If the compiler chooses the first approach, then the assignment of this
example will indeed raise ``Constraint_Error`` if overflow checking is
enabled, or result in erroneous execution if overflow checks are suppressed.

But if the compiler
chooses the second approach, then it can perform both additions yielding
the correct mathematical result, which is in range, so no exception
will be raised, and the right result is obtained, regardless of whether
overflow checks are suppressed.

Note that in the first example an
exception will be raised in either case, since if the compiler
gives the correct mathematical result for the addition, it will
be out of range of the target type of the assignment, and thus
fails the range check.

This lack of specified behavior in the handling of overflow for
intermediate results is a source of non-portability, and can thus
be problematic when programs are ported. Most typically this arises
in a situation where the original compiler did not raise an exception,
and then the application is moved to a compiler where the check is
performed on the intermediate result and an unexpected exception is
raised.

Furthermore, when using Ada 2012's preconditions and other
assertion forms, another issue arises. Consider:

  .. code-block:: ada

       procedure P (A, B : Integer) with
         Pre => A + B <= Integer'Last;

One often wants to regard arithmetic in a context like this from
a mathematical point of view. So for example, if the two actual parameters
for a call to ``P`` are both ``Integer'Last``, then
the precondition should be regarded as False. If we are executing
in a mode with run-time checks enabled for preconditions, then we would
like this precondition to fail, rather than raising an exception
because of the intermediate overflow.

However, the language definition leaves the specification of
whether the above condition fails (raising ``Assert_Error``) or
causes an intermediate overflow (raising ``Constraint_Error``)
up to the implementation.

The situation is worse in a case such as the following:

  .. code-block:: ada

       procedure Q (A, B, C : Integer) with
         Pre => A + B + C <= Integer'Last;

Consider the call

  .. code-block:: ada

       Q (A => Integer'Last, B => 1, C => -1);

From a mathematical point of view the precondition
is True, but at run time we may (but are not guaranteed to) get an
exception raised because of the intermediate overflow (and we really
would prefer this precondition to be considered True at run time).


.. _Management_of_Overflows_in_GNAT:

Management of Overflows in GNAT
-------------------------------

To deal with the portability issue, and with the problem of
mathematical versus run-time interpretation of the expressions in
assertions, GNAT provides comprehensive control over the handling
of intermediate overflow. GNAT can operate in three modes, and
furthemore, permits separate selection of operating modes for
the expressions within assertions (here the term 'assertions'
is used in the technical sense, which includes preconditions and so forth)
and for expressions appearing outside assertions.

The three modes are:

* *Use base type for intermediate operations* (``STRICT``)

  In this mode, all intermediate results for predefined arithmetic
  operators are computed using the base type, and the result must
  be in range of the base type. If this is not the
  case then either an exception is raised (if overflow checks are
  enabled) or the execution is erroneous (if overflow checks are suppressed).
  This is the normal default mode.

* *Most intermediate overflows avoided* (``MINIMIZED``)

  In this mode, the compiler attempts to avoid intermediate overflows by
  using a larger integer type, typically ``Long_Long_Integer``,
  as the type in which arithmetic is
  performed for predefined arithmetic operators. This may be slightly more
  expensive at
  run time (compared to suppressing intermediate overflow checks), though
  the cost is negligible on modern 64-bit machines. For the examples given
  earlier, no intermediate overflows would have resulted in exceptions,
  since the intermediate results are all in the range of
  ``Long_Long_Integer`` (typically 64-bits on nearly all implementations
  of GNAT). In addition, if checks are enabled, this reduces the number of
  checks that must be made, so this choice may actually result in an
  improvement in space and time behavior.

  However, there are cases where ``Long_Long_Integer`` is not large
  enough, consider the following example:

    .. code-block:: ada

           procedure R (A, B, C, D : Integer) with
             Pre => (A**2 * B**2) / (C**2 * D**2) <= 10;

  where ``A`` = ``B`` = ``C`` = ``D`` = ``Integer'Last``.
  Now the intermediate results are
  out of the range of ``Long_Long_Integer`` even though the final result
  is in range and the precondition is True (from a mathematical point
  of view). In such a case, operating in this mode, an overflow occurs
  for the intermediate computation (which is why this mode
  says *most* intermediate overflows are avoided). In this case,
  an exception is raised if overflow checks are enabled, and the
  execution is erroneous if overflow checks are suppressed.

* *All intermediate overflows avoided* (``ELIMINATED``)

  In this mode, the compiler  avoids all intermediate overflows
  by using arbitrary precision arithmetic as required. In this
  mode, the above example with ``A**2 * B**2`` would
  not cause intermediate overflow, because the intermediate result
  would be evaluated using sufficient precision, and the result
  of evaluating the precondition would be True.

  This mode has the advantage of avoiding any intermediate
  overflows, but at the expense of significant run-time overhead,
  including the use of a library (included automatically in this
  mode) for multiple-precision arithmetic.

  This mode provides cleaner semantics for assertions, since now
  the run-time behavior emulates true arithmetic behavior for the
  predefined arithmetic operators, meaning that there is never a
  conflict between the mathematical view of the assertion, and its
  run-time behavior.

  Note that in this mode, the behavior is unaffected by whether or
  not overflow checks are suppressed, since overflow does not occur.
  It is possible for gigantic intermediate expressions to raise
  ``Storage_Error`` as a result of attempting to compute the
  results of such expressions (e.g. ``Integer'Last ** Integer'Last``)
  but overflow is impossible.


Note that these modes apply only to the evaluation of predefined
arithmetic, membership, and comparison operators for signed integer
arithmetic.

For fixed-point arithmetic, checks can be suppressed. But if checks
are enabled
then fixed-point values are always checked for overflow against the
base type for intermediate expressions (that is such checks always
operate in the equivalent of ``STRICT`` mode).

For floating-point, on nearly all architectures, ``Machine_Overflows``
is False, and IEEE infinities are generated, so overflow exceptions
are never raised. If you want to avoid infinities, and check that
final results of expressions are in range, then you can declare a
constrained floating-point type, and range checks will be carried
out in the normal manner (with infinite values always failing all
range checks).


.. _Specifying_the_Desired_Mode:

Specifying the Desired Mode
---------------------------

.. index:: pragma Overflow_Mode

The desired mode of for handling intermediate overflow can be specified using
either the ``Overflow_Mode`` pragma or an equivalent compiler switch.
The pragma has the form

  .. code-block:: ada

      pragma Overflow_Mode ([General =>] MODE [, [Assertions =>] MODE]);

where ``MODE`` is one of

* ``STRICT``:  intermediate overflows checked (using base type)
* ``MINIMIZED``: minimize intermediate overflows
* ``ELIMINATED``: eliminate intermediate overflows

The case is ignored, so ``MINIMIZED``, ``Minimized`` and
``minimized`` all have the same effect.

If only the ``General`` parameter is present, then the given ``MODE`` applies
to expressions both within and outside assertions. If both arguments
are present, then ``General`` applies to expressions outside assertions,
and ``Assertions`` applies to expressions within assertions. For example:

  .. code-block:: ada

     pragma Overflow_Mode
       (General => Minimized, Assertions => Eliminated);

specifies that general expressions outside assertions be evaluated
in 'minimize intermediate overflows' mode, and expressions within
assertions be evaluated in 'eliminate intermediate overflows' mode.
This is often a reasonable choice, avoiding excessive overhead
outside assertions, but assuring a high degree of portability
when importing code from another compiler, while incurring
the extra overhead for assertion expressions to ensure that
the behavior at run time matches the expected mathematical
behavior.

The ``Overflow_Mode`` pragma has the same scoping and placement
rules as pragma ``Suppress``, so it can occur either as a
configuration pragma, specifying a default for the whole
program, or in a declarative scope, where it applies to the
remaining declarations and statements in that scope.

Note that pragma ``Overflow_Mode`` does not affect whether
overflow checks are enabled or suppressed. It only controls the
method used to compute intermediate values. To control whether
overflow checking is enabled or suppressed, use pragma ``Suppress``
or ``Unsuppress`` in the usual manner.


.. index:: -gnato? (gcc)
.. index:: -gnato?? (gcc)

Additionally, a compiler switch :switch:`-gnato?` or :switch:`-gnato??`
can be used to control the checking mode default (which can be subsequently
overridden using pragmas).

Here ``?`` is one of the digits ``1`` through ``3``:

  ====== ======================================================
  ``1``  use base type for intermediate operations (``STRICT``)
  ``2``  minimize intermediate overflows (``MINIMIZED``)
  ``3``  eliminate intermediate overflows (``ELIMINATED``)
  ====== ======================================================

As with the pragma, if only one digit appears then it applies to all
cases; if two digits are given, then the first applies outside
assertions, and the second within assertions. Thus the equivalent
of the example pragma above would be
:switch:`-gnato23`.

If no digits follow the :switch:`-gnato`, then it is equivalent to
:switch:`-gnato11`,
causing all intermediate operations to be computed using the base
type (``STRICT`` mode).


.. _Default_Settings:

Default Settings
----------------

The default mode for overflow checks is

  ::

      General => Strict

which causes all computations both inside and outside assertions to use
the base type.

This retains compatibility with previous versions of
GNAT which suppressed overflow checks by default and always
used the base type for computation of intermediate results.

.. Sphinx allows no emphasis within :index: role. As a workaround we
   point the index to "switch" and use emphasis for "-gnato".

The :index:`switch <-gnato (gcc)>` :switch:`-gnato` (with no digits following)
is equivalent to

  ::

      General => Strict

which causes overflow checking of all intermediate overflows
both inside and outside assertions against the base type.

The pragma ``Suppress (Overflow_Check)`` disables overflow
checking, but it has no effect on the method used for computing
intermediate results.

The pragma ``Unsuppress (Overflow_Check)`` enables overflow
checking, but it has no effect on the method used for computing
intermediate results.


.. _Implementation_Notes:

Implementation Notes
--------------------

In practice on typical 64-bit machines, the ``MINIMIZED`` mode is
reasonably efficient, and can be generally used. It also helps
to ensure compatibility with code imported from some other
compiler to GNAT.

Setting all intermediate overflows checking (``CHECKED`` mode)
makes sense if you want to
make sure that your code is compatible with any other possible
Ada implementation. This may be useful in ensuring portability
for code that is to be exported to some other compiler than GNAT.

The Ada standard allows the reassociation of expressions at
the same precedence level if no parentheses are present. For
example, ``A+B+C`` parses as though it were ``(A+B)+C``, but
the compiler can reintepret this as ``A+(B+C)``, possibly
introducing or eliminating an overflow exception. The GNAT
compiler never takes advantage of this freedom, and the
expression ``A+B+C`` will be evaluated as ``(A+B)+C``.
If you need the other order, you can write the parentheses
explicitly ``A+(B+C)`` and GNAT will respect this order.

The use of ``ELIMINATED`` mode will cause the compiler to
automatically include an appropriate arbitrary precision
integer arithmetic package. The compiler will make calls
to this package, though only in cases where it cannot be
sure that ``Long_Long_Integer`` is sufficient to guard against
intermediate overflows. This package does not use dynamic
alllocation, but it does use the secondary stack, so an
appropriate secondary stack package must be present (this
is always true for standard full Ada, but may require
specific steps for restricted run times such as ZFP).

Although ``ELIMINATED`` mode causes expressions to use arbitrary
precision arithmetic, avoiding overflow, the final result
must be in an appropriate range. This is true even if the
final result is of type ``[Long_[Long_]]Integer'Base``, which
still has the same bounds as its associated constrained
type at run-time.

Currently, the ``ELIMINATED`` mode is only available on target
platforms for which ``Long_Long_Integer`` is 64-bits (nearly all GNAT
platforms).



.. _Performing_Dimensionality_Analysis_in_GNAT:

Performing Dimensionality Analysis in GNAT
==========================================

.. index:: Dimensionality analysis

The GNAT compiler supports dimensionality checking. The user can
specify physical units for objects, and the compiler will verify that uses
of these objects are compatible with their dimensions, in a fashion that is
familiar to engineering practice. The dimensions of algebraic expressions
(including powers with static exponents) are computed from their constituents.

.. index:: Dimension_System aspect
.. index:: Dimension aspect

This feature depends on Ada 2012 aspect specifications, and is available from
version 7.0.1 of GNAT onwards.
The GNAT-specific aspect ``Dimension_System``
allows you to define a system of units; the aspect ``Dimension``
then allows the user to declare dimensioned quantities within a given system.
(These aspects are described in the *Implementation Defined Aspects*
chapter of the *GNAT Reference Manual*).

The major advantage of this model is that it does not require the declaration of
multiple operators for all possible combinations of types: it is only necessary
to use the proper subtypes in object declarations.

.. index:: System.Dim.Mks package (GNAT library)
.. index:: MKS_Type type

The simplest way to impose dimensionality checking on a computation is to make
use of one of the instantiations of the package ``System.Dim.Generic_Mks``, which
are part of the GNAT library. This generic package defines a floating-point
type ``MKS_Type``, for which a sequence of dimension names are specified,
together with their conventional abbreviations.  The following should be read
together with the full specification of the package, in file
:file:`s-digemk.ads`.

  .. index:: s-digemk.ads file

  .. code-block:: ada

     type Mks_Type is new Float_Type
       with
        Dimension_System => (
          (Unit_Name => Meter,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),
          (Unit_Name => Kilogram, Unit_Symbol => "kg",  Dim_Symbol => 'M'),
          (Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),
          (Unit_Name => Ampere,   Unit_Symbol => 'A',   Dim_Symbol => 'I'),
          (Unit_Name => Kelvin,   Unit_Symbol => 'K',   Dim_Symbol => "Theta"),
          (Unit_Name => Mole,     Unit_Symbol => "mol", Dim_Symbol => 'N'),
          (Unit_Name => Candela,  Unit_Symbol => "cd",  Dim_Symbol => 'J'));

The package then defines a series of subtypes that correspond to these
conventional units. For example:

  .. code-block:: ada

     subtype Length is Mks_Type
       with
        Dimension => (Symbol => 'm', Meter  => 1, others => 0);

and similarly for ``Mass``, ``Time``, ``Electric_Current``,
``Thermodynamic_Temperature``, ``Amount_Of_Substance``, and
``Luminous_Intensity`` (the standard set of units of the SI system).

The package also defines conventional names for values of each unit, for
example:

  .. code-block:: ada

     m   : constant Length           := 1.0;
     kg  : constant Mass             := 1.0;
     s   : constant Time             := 1.0;
     A   : constant Electric_Current := 1.0;

as well as useful multiples of these units:

  .. code-block:: ada

     cm  : constant Length := 1.0E-02;
     g   : constant Mass   := 1.0E-03;
     min : constant Time   := 60.0;
     day : constant Time   := 60.0 * 24.0 * min;
    ...

There are three instantiations of ``System.Dim.Generic_Mks`` defined in the
GNAT library:

* ``System.Dim.Float_Mks`` based on ``Float`` defined in :file:`s-diflmk.ads`.
* ``System.Dim.Long_Mks`` based on ``Long_Float`` defined in :file:`s-dilomk.ads`.
* ``System.Dim.Mks`` based on ``Long_Long_Float`` defined in :file:`s-dimmks.ads`.

Using one of these packages, you can then define a derived unit by providing
the aspect that specifies its dimensions within the MKS system, as well as the
string to be used for output of a value of that unit:

  .. code-block:: ada

     subtype Acceleration is Mks_Type
       with Dimension => ("m/sec^2",
                          Meter => 1,
                          Second => -2,
                          others => 0);

Here is a complete example of use:

  .. code-block:: ada

     with System.Dim.MKS; use System.Dim.Mks;
     with System.Dim.Mks_IO; use System.Dim.Mks_IO;
     with Text_IO; use Text_IO;
     procedure Free_Fall is
       subtype Acceleration is Mks_Type
         with Dimension => ("m/sec^2", 1, 0, -2, others => 0);
       G : constant acceleration := 9.81 * m / (s ** 2);
       T : Time := 10.0*s;
       Distance : Length;

     begin
       Put ("Gravitational constant: ");
       Put (G, Aft => 2, Exp => 0); Put_Line ("");
       Distance := 0.5 * G * T ** 2;
       Put ("distance travelled in 10 seconds of free fall ");
       Put (Distance, Aft => 2, Exp => 0);
       Put_Line ("");
     end Free_Fall;

Execution of this program yields:

  ::

     Gravitational constant:  9.81 m/sec^2
     distance travelled in 10 seconds of free fall 490.50 m

However, incorrect assignments such as:

  .. code-block:: ada

       Distance := 5.0;
       Distance := 5.0 * kg;

are rejected with the following diagnoses:

 ::

     Distance := 5.0;
        >>> dimensions mismatch in assignment
        >>> left-hand side has dimension [L]
        >>> right-hand side is dimensionless

     Distance := 5.0 * kg:
        >>> dimensions mismatch in assignment
        >>> left-hand side has dimension [L]
        >>> right-hand side has dimension [M]

The dimensions of an expression are properly displayed, even if there is
no explicit subtype for it. If we add to the program:

  .. code-block:: ada

        Put ("Final velocity: ");
        Put (G * T, Aft =>2, Exp =>0);
        Put_Line ("");

then the output includes:

  ::

       Final velocity: 98.10 m.s**(-1)


  .. index:: Dimensionable type
  .. index:: Dimensioned subtype

The type ``Mks_Type`` is said to be a *dimensionable type* since it has a
``Dimension_System`` aspect, and the subtypes ``Length``, ``Mass``, etc.,
are said to be *dimensioned subtypes* since each one has a ``Dimension``
aspect.

  .. index:: Dimension Vector (for a dimensioned subtype)
  .. index:: Dimension aspect
  .. index:: Dimension_System aspect

The ``Dimension`` aspect of a dimensioned subtype ``S`` defines a mapping
from the base type's Unit_Names to integer (or, more generally, rational)
values. This mapping is the *dimension vector* (also referred to as the
*dimensionality*) for that subtype, denoted by ``DV(S)``, and thus for each
object of that subtype. Intuitively, the value specified for each
``Unit_Name`` is the exponent associated with that unit; a zero value
means that the unit is not used. For example:

   .. code-block:: ada

      declare
         Acc : Acceleration;
         ...
      begin
         ...
      end;

Here ``DV(Acc)`` = ``DV(Acceleration)`` =
``(Meter=>1, Kilogram=>0, Second=>-2, Ampere=>0, Kelvin=>0, Mole=>0, Candela=>0)``.
Symbolically, we can express this as ``Meter / Second**2``.

The dimension vector of an arithmetic expression is synthesized from the
dimension vectors of its components, with compile-time dimensionality checks
that help prevent mismatches such as using an ``Acceleration`` where a
``Length`` is required.

The dimension vector of the result of an arithmetic expression *expr*, or
:samp:`DV({expr})`, is defined as follows, assuming conventional
mathematical definitions for the vector operations that are used:

* If *expr* is of the type *universal_real*, or is not of a dimensioned subtype,
  then *expr* is dimensionless; :samp:`DV({expr})` is the empty vector.

* :samp:`DV({op expr})`, where *op* is a unary operator, is :samp:`DV({expr})`

* :samp:`DV({expr1 op expr2})` where *op* is "+" or "-" is :samp:`DV({expr1})`
  provided that :samp:`DV({expr1})` = :samp:`DV({expr2})`.
  If this condition is not met then the construct is illegal.

* :samp:`DV({expr1} * {expr2})` is :samp:`DV({expr1})` + :samp:`DV({expr2})`,
  and :samp:`DV({expr1} / {expr2})` = :samp:`DV({expr1})` - :samp:`DV({expr2})`.
  In this context if one of the *expr*\ s is dimensionless then its empty
  dimension vector is treated as ``(others => 0)``.

* :samp:`DV({expr} ** {power})` is *power* * :samp:`DV({expr})`,
  provided that *power* is a static rational value. If this condition is not
  met then the construct is illegal.

Note that, by the above rules, it is illegal to use binary "+" or "-" to
combine a dimensioned and dimensionless value.  Thus an expression such as
``acc-10.0`` is illegal, where ``acc`` is an object of subtype
``Acceleration``.

The dimensionality checks for relationals use the same rules as
for "+" and "-", except when comparing to a literal; thus

  .. code-block:: ada

        acc > len

is equivalent to

  .. code-block:: ada

       acc-len > 0.0

and is thus illegal, but

  .. code-block:: ada

        acc > 10.0

is accepted with a warning. Analogously a conditional expression requires the
same dimension vector for each branch (with no exception for literals).

The dimension vector of a type conversion :samp:`T({expr})` is defined
as follows, based on the nature of ``T``:

* If ``T`` is a dimensioned subtype then :samp:`DV(T({expr}))` is ``DV(T)``
  provided that either *expr* is dimensionless or
  :samp:`DV(T)` = :samp:`DV({expr})`. The conversion is illegal
  if *expr* is dimensioned and :samp:`DV({expr})` /= ``DV(T)``.
  Note that vector equality does not require that the corresponding
  Unit_Names be the same.

  As a consequence of the above rule, it is possible to convert between
  different dimension systems that follow the same international system
  of units, with the seven physical components given in the standard order
  (length, mass, time, etc.). Thus a length in meters can be converted to
  a length in inches (with a suitable conversion factor) but cannot be
  converted, for example, to a mass in pounds.

* If ``T`` is the base type for *expr* (and the dimensionless root type of
  the dimension system), then :samp:`DV(T({expr}))` is ``DV(expr)``.
  Thus, if *expr* is of a dimensioned subtype of ``T``, the conversion may
  be regarded as a "view conversion" that preserves dimensionality.

  This rule makes it possible to write generic code that can be instantiated
  with compatible dimensioned subtypes.  The generic unit will contain
  conversions that will consequently be present in instantiations, but
  conversions to the base type will preserve dimensionality and make it
  possible to write generic code that is correct with respect to
  dimensionality.

* Otherwise (i.e., ``T`` is neither a dimensioned subtype nor a dimensionable
  base type), :samp:`DV(T({expr}))` is the empty vector. Thus a dimensioned
  value can be explicitly converted to a non-dimensioned subtype, which
  of course then escapes dimensionality analysis.

The dimension vector for a type qualification :samp:`T'({expr})` is the same
as for the type conversion :samp:`T({expr})`.

An assignment statement

   .. code-block:: ada

         Source := Target;

requires ``DV(Source)`` = ``DV(Target)``, and analogously for parameter
passing (the dimension vector for the actual parameter must be equal to the
dimension vector for the formal parameter).


.. _Stack_Related_Facilities:

Stack Related Facilities
========================

This section describes some useful tools associated with stack
checking and analysis. In
particular, it deals with dynamic and static stack usage measurements.

.. _Stack_Overflow_Checking:

Stack Overflow Checking
-----------------------

.. index:: Stack Overflow Checking

.. index:: -fstack-check (gcc)

For most operating systems, ``gcc`` does not perform stack overflow
checking by default. This means that if the main environment task or
some other task exceeds the available stack space, then unpredictable
behavior will occur. Most native systems offer some level of protection by
adding a guard page at the end of each task stack. This mechanism is usually
not enough for dealing properly with stack overflow situations because
a large local variable could "jump" above the guard page.
Furthermore, when the
guard page is hit, there may not be any space left on the stack for executing
the exception propagation code. Enabling stack checking avoids
such situations.

To activate stack checking, compile all units with the ``gcc`` option
:switch:`-fstack-check`. For example:

  ::

     $ gcc -c -fstack-check package1.adb

Units compiled with this option will generate extra instructions to check
that any use of the stack (for procedure calls or for declaring local
variables in declare blocks) does not exceed the available stack space.
If the space is exceeded, then a ``Storage_Error`` exception is raised.

For declared tasks, the default stack size is defined by the GNAT runtime,
whose size may be modified at bind time through the ``-d`` bind switch
(:ref:`Switches_for_gnatbind`). Task specific stack sizes may be set using the
``Storage_Size`` pragma.

For the environment task, the stack size is determined by the operating system.
Consequently, to modify the size of the environment task please refer to your
operating system documentation.


.. _Static_Stack_Usage_Analysis:

Static Stack Usage Analysis
---------------------------

.. index:: Static Stack Usage Analysis

.. index:: -fstack-usage

A unit compiled with ``-fstack-usage`` will generate an extra file
that specifies
the maximum amount of stack used, on a per-function basis.
The file has the same
basename as the target object file with a :file:`.su` extension.
Each line of this file is made up of three fields:

* The name of the function.
* A number of bytes.
* One or more qualifiers: ``static``, ``dynamic``, ``bounded``.

The second field corresponds to the size of the known part of the function
frame.

The qualifier ``static`` means that the function frame size
is purely static.
It usually means that all local variables have a static size.
In this case, the second field is a reliable measure of the function stack
utilization.

The qualifier ``dynamic`` means that the function frame size is not static.
It happens mainly when some local variables have a dynamic size. When this
qualifier appears alone, the second field is not a reliable measure
of the function stack analysis. When it is qualified with  ``bounded``, it
means that the second field is a reliable maximum of the function stack
utilization.

A unit compiled with ``-Wstack-usage`` will issue a warning for each
subprogram whose stack usage might be larger than the specified amount of
bytes.  The wording is in keeping with the qualifier documented above.


.. _Dynamic_Stack_Usage_Analysis:

Dynamic Stack Usage Analysis
----------------------------

It is possible to measure the maximum amount of stack used by a task, by
adding a switch to ``gnatbind``, as:

  ::

      $ gnatbind -u0 file

With this option, at each task termination, its stack usage is  output on
:file:`stderr`.
It is not always convenient to output the stack usage when the program
is still running. Hence, it is possible to delay this output until program
termination. for a given number of tasks specified as the argument of the
``-u`` option. For instance:

  ::

     $ gnatbind -u100 file

will buffer the stack usage information of the first 100 tasks to terminate and
output this info at program termination. Results are displayed in four
columns:

  ::

     Index | Task Name | Stack Size | Stack Usage

where:

* *Index* is a number associated with each task.

* *Task Name* is the name of the task analyzed.

* *Stack Size* is the maximum size for the stack.

* *Stack Usage* is the measure done by the stack analyzer.
  In order to prevent overflow, the stack
  is not entirely analyzed, and it's not possible to know exactly how
  much has actually been used.

By default the environment task stack, the stack that contains the main unit,
is not processed. To enable processing of the environment task stack, the
environment variable GNAT_STACK_LIMIT needs to be set to the maximum size of
the environment task stack. This amount is given in kilobytes. For example:

  ::

     $ set GNAT_STACK_LIMIT 1600

would specify to the analyzer that the environment task stack has a limit
of 1.6 megabytes. Any stack usage beyond this will be ignored by the analysis.

The package ``GNAT.Task_Stack_Usage`` provides facilities to get
stack-usage reports at run time. See its body for the details.



.. _Memory_Management_Issues:

Memory Management Issues
========================

This section describes some useful memory pools provided in the GNAT library
and in particular the GNAT Debug Pool facility, which can be used to detect
incorrect uses of access values (including 'dangling references').

.. only:: PRO or GPL

  It also describes the ``gnatmem`` tool, which can be used to track down
  "memory leaks".

.. _Some_Useful_Memory_Pools:

Some Useful Memory Pools
------------------------

.. index:: Memory Pool
.. index:: storage, pool

The ``System.Pool_Global`` package offers the Unbounded_No_Reclaim_Pool
storage pool. Allocations use the standard system call ``malloc`` while
deallocations use the standard system call ``free``. No reclamation is
performed when the pool goes out of scope. For performance reasons, the
standard default Ada allocators/deallocators do not use any explicit storage
pools but if they did, they could use this storage pool without any change in
behavior. That is why this storage pool is used  when the user
manages to make the default implicit allocator explicit as in this example:

  .. code-block:: ada

       type T1 is access Something;
        -- no Storage pool is defined for T2

       type T2 is access Something_Else;
       for T2'Storage_Pool use T1'Storage_Pool;
       -- the above is equivalent to
       for T2'Storage_Pool use System.Pool_Global.Global_Pool_Object;

The ``System.Pool_Local`` package offers the ``Unbounded_Reclaim_Pool`` storage
pool. The allocation strategy is similar to ``Pool_Local``
except that the all
storage allocated with this pool is reclaimed when the pool object goes out of
scope. This pool provides a explicit mechanism similar to the implicit one
provided by several Ada 83 compilers for allocations performed through a local
access type and whose purpose was to reclaim memory when exiting the
scope of a given local access. As an example, the following program does not
leak memory even though it does not perform explicit deallocation:

  .. code-block:: ada

     with System.Pool_Local;
     procedure Pooloc1 is
        procedure Internal is
           type A is access Integer;
           X : System.Pool_Local.Unbounded_Reclaim_Pool;
           for A'Storage_Pool use X;
           v : A;
        begin
           for I in  1 .. 50 loop
              v := new Integer;
           end loop;
        end Internal;
     begin
        for I in  1 .. 100 loop
           Internal;
        end loop;
     end Pooloc1;

The ``System.Pool_Size`` package implements the ``Stack_Bounded_Pool`` used when
``Storage_Size`` is specified for an access type.
The whole storage for the pool is
allocated at once, usually on the stack at the point where the access type is
elaborated. It is automatically reclaimed when exiting the scope where the
access type is defined. This package is not intended to be used directly by the
user and it is implicitly used for each such declaration:

  .. code-block:: ada

     type T1 is access Something;
     for T1'Storage_Size use 10_000;


.. _The_GNAT_Debug_Pool_Facility:

The GNAT Debug Pool Facility
----------------------------

.. index:: Debug Pool
.. index:: storage, pool, memory corruption

The use of unchecked deallocation and unchecked conversion can easily
lead to incorrect memory references. The problems generated by such
references are usually difficult to tackle because the symptoms can be
very remote from the origin of the problem. In such cases, it is
very helpful to detect the problem as early as possible. This is the
purpose of the Storage Pool provided by ``GNAT.Debug_Pools``.

In order to use the GNAT specific debugging pool, the user must
associate a debug pool object with each of the access types that may be
related to suspected memory problems. See Ada Reference Manual 13.11.

  .. code-block:: ada

     type Ptr is access Some_Type;
     Pool : GNAT.Debug_Pools.Debug_Pool;
     for Ptr'Storage_Pool use Pool;

``GNAT.Debug_Pools`` is derived from a GNAT-specific kind of
pool: the ``Checked_Pool``. Such pools, like standard Ada storage pools,
allow the user to redefine allocation and deallocation strategies. They
also provide a checkpoint for each dereference, through the use of
the primitive operation ``Dereference`` which is implicitly called at
each dereference of an access value.

Once an access type has been associated with a debug pool, operations on
values of the type may raise four distinct exceptions,
which correspond to four potential kinds of memory corruption:

* ``GNAT.Debug_Pools.Accessing_Not_Allocated_Storage``
* ``GNAT.Debug_Pools.Accessing_Deallocated_Storage``
* ``GNAT.Debug_Pools.Freeing_Not_Allocated_Storage``
* ``GNAT.Debug_Pools.Freeing_Deallocated_Storage``

For types associated with a Debug_Pool, dynamic allocation is performed using
the standard GNAT allocation routine. References to all allocated chunks of
memory are kept in an internal dictionary. Several deallocation strategies are
provided, whereupon the user can choose to release the memory to the system,
keep it allocated for further invalid access checks, or fill it with an easily
recognizable pattern for debug sessions. The memory pattern is the old IBM
hexadecimal convention: ``16#DEADBEEF#``.

See the documentation in the file g-debpoo.ads for more information on the
various strategies.

Upon each dereference, a check is made that the access value denotes a
properly allocated memory location. Here is a complete example of use of
``Debug_Pools``, that includes typical instances of  memory corruption:

  .. code-block:: ada

      with Gnat.Io; use Gnat.Io;
      with Unchecked_Deallocation;
      with Unchecked_Conversion;
      with GNAT.Debug_Pools;
      with System.Storage_Elements;
      with Ada.Exceptions; use Ada.Exceptions;
      procedure Debug_Pool_Test is

         type T is access Integer;
         type U is access all T;

         P : GNAT.Debug_Pools.Debug_Pool;
         for T'Storage_Pool use P;

         procedure Free is new Unchecked_Deallocation (Integer, T);
         function UC is new Unchecked_Conversion (U, T);
         A, B : aliased T;

         procedure Info is new GNAT.Debug_Pools.Print_Info(Put_Line);

      begin
         Info (P);
         A := new Integer;
         B := new Integer;
         B := A;
         Info (P);
         Free (A);
         begin
            Put_Line (Integer'Image(B.all));
         exception
            when E : others => Put_Line ("raised: " & Exception_Name (E));
         end;
         begin
            Free (B);
         exception
            when E : others => Put_Line ("raised: " & Exception_Name (E));
         end;
         B := UC(A'Access);
         begin
            Put_Line (Integer'Image(B.all));
         exception
            when E : others => Put_Line ("raised: " & Exception_Name (E));
         end;
         begin
            Free (B);
         exception
            when E : others => Put_Line ("raised: " & Exception_Name (E));
         end;
         Info (P);
      end Debug_Pool_Test;

The debug pool mechanism provides the following precise diagnostics on the
execution of this erroneous program:

  ::

     Debug Pool info:
       Total allocated bytes :  0
       Total deallocated bytes :  0
       Current Water Mark:  0
       High Water Mark:  0

     Debug Pool info:
       Total allocated bytes :  8
       Total deallocated bytes :  0
       Current Water Mark:  8
       High Water Mark:  8

     raised: GNAT.DEBUG_POOLS.ACCESSING_DEALLOCATED_STORAGE
     raised: GNAT.DEBUG_POOLS.FREEING_DEALLOCATED_STORAGE
     raised: GNAT.DEBUG_POOLS.ACCESSING_NOT_ALLOCATED_STORAGE
     raised: GNAT.DEBUG_POOLS.FREEING_NOT_ALLOCATED_STORAGE
     Debug Pool info:
       Total allocated bytes :  8
       Total deallocated bytes :  4
       Current Water Mark:  4
       High Water Mark:  8

.. only:: PRO or GPL

  .. _The_gnatmem_Tool:

  The ``gnatmem`` Tool
  --------------------

  .. index:: ! gnatmem

  The ``gnatmem`` utility monitors dynamic allocation and
  deallocation activity in a program, and displays information about
  incorrect deallocations and possible sources of memory leaks.
  It is designed to work in association with a static runtime library
  only and in this context provides three types of information:

  * General information concerning memory management, such as the total
    number of allocations and deallocations, the amount of allocated
    memory and the high water mark, i.e., the largest amount of allocated
    memory in the course of program execution.

  * Backtraces for all incorrect deallocations, that is to say deallocations
    which do not correspond to a valid allocation.

  * Information on each allocation that is potentially the origin of a memory
    leak.

  .. _Running_gnatmem:

  Running ``gnatmem``
  ^^^^^^^^^^^^^^^^^^^

  ``gnatmem`` makes use of the output created by the special version of
  allocation and deallocation routines that record call information. This allows
  it to obtain accurate dynamic memory usage history at a minimal cost to the
  execution speed. Note however, that ``gnatmem`` is only supported on
  GNU/Linux and Windows.

  The ``gnatmem`` command has the form

    ::

       $ gnatmem [ switches ] user_program

  The program must have been linked with the instrumented version of the
  allocation and deallocation routines. This is done by linking with the
  :file:`libgmem.a` library. For correct symbolic backtrace information,
  the user program should be compiled with debugging options
  (see :ref:`Switches_for_gcc`). For example to build :file:`my_program`:

    ::

       $ gnatmake -g my_program -largs -lgmem

  As library :file:`libgmem.a` contains an alternate body for package
  ``System.Memory``, :file:`s-memory.adb` should not be compiled and linked
  when an executable is linked with library :file:`libgmem.a`. It is then not
  recommended to use ``gnatmake`` with switch :switch:`-a`.

  When :file:`my_program` is executed, the file :file:`gmem.out` is produced.
  This file contains information about all allocations and deallocations
  performed by the program. It is produced by the instrumented allocations and
  deallocations routines and will be used by ``gnatmem``.

  In order to produce symbolic backtrace information for allocations and
  deallocations performed by the GNAT run-time library, you need to use a
  version of that library that has been compiled with the :switch:`-g` switch
  (see :ref:`Rebuilding_the_GNAT_Run-Time_Library`).

  ``gnatmem`` must be supplied with the :file:`gmem.out` file and the executable to
  examine. If the location of :file:`gmem.out` file was not explicitly supplied by
  :switch:`-i` switch, gnatmem will assume that this file can be found in the
  current directory. For example, after you have executed :file:`my_program`,
  :file:`gmem.out` can be analyzed by ``gnatmem`` using the command:

    ::

       $ gnatmem my_program

  This will produce the output with the following format:

    ::

        $ gnatmem my_program

        Global information
        ------------------
           Total number of allocations        :  45
           Total number of deallocations      :   6
           Final Water Mark (non freed mem)   :  11.29 Kilobytes
           High Water Mark                    :  11.40 Kilobytes

        .
        .
        .
        Allocation Root # 2
        -------------------
         Number of non freed allocations    :  11
         Final Water Mark (non freed mem)   :   1.16 Kilobytes
         High Water Mark                    :   1.27 Kilobytes
         Backtrace                          :
           my_program.adb:23 my_program.alloc
        .
        .
        .

  The first block of output gives general information. In this case, the
  Ada construct ``new`` was executed 45 times, and only 6 calls to an
  Unchecked_Deallocation routine occurred.

  Subsequent paragraphs display  information on all allocation roots.
  An allocation root is a specific point in the execution of the program
  that generates some dynamic allocation, such as a ``new``
  construct. This root is represented by an execution backtrace (or subprogram
  call stack). By default the backtrace depth for allocations roots is 1, so
  that a root corresponds exactly to a source location. The backtrace can
  be made deeper, to make the root more specific.

  .. _Switches_for_gnatmem:

  Switches for ``gnatmem``
  ^^^^^^^^^^^^^^^^^^^^^^^^

  ``gnatmem`` recognizes the following switches:

  .. index:: -q (gnatmem)

  :samp:`-q`
    Quiet. Gives the minimum output needed to identify the origin of the
    memory leaks. Omits statistical information.


  .. index:: N switch (gnatmem)

  :samp:`{N}`
    ``N`` is an integer literal (usually between 1 and 10) which controls the
    depth of the backtraces defining allocation root. The default value for
    N is 1. The deeper the backtrace, the more precise the localization of
    the root. Note that the total number of roots can depend on this
    parameter. This parameter must be specified *before* the name of the
    executable to be analyzed, to avoid ambiguity.


  .. index:: -b (gnatmem)

  :samp:`-b {N}`
    This switch has the same effect as just a depth parameter ``N``.


  .. index:: -i (gnatmem)

  :samp:`-i {file}`
    Do the ``gnatmem`` processing starting from :file:`file`, rather than
    :file:`gmem.out` in the current directory.


  .. index:: -m (gnatmem)

  :samp:`-m {n}`
    This switch causes ``gnatmem`` to mask the allocation roots that have less
    than n leaks.  The default value is 1. Specifying the value of 0 will allow
    examination of even the roots that did not result in leaks.


  .. index:: -s (gnatmem)

  :samp:`-s {order}`
    This switch causes ``gnatmem`` to sort the allocation roots according to the
    specified order of sort criteria, each identified by a single letter. The
    currently supported criteria are ``n``, ``h``, and ``w`` standing respectively for
    number of unfreed allocations, high watermark, and final watermark
    corresponding to a specific root. The default order is ``nwh``.


  .. index:: -t (gnatmem)

  :samp:`-t`
    This switch causes memory allocated size to be always output in bytes.
    Default ``gnatmem`` behavior is to show memory sizes less then 1 kilobyte
    in bytes, from 1 kilobyte till 1 megabyte in kilobytes and the rest in
    megabytes.


  .. _Example_of_gnatmem_Usage:

  Example of ``gnatmem`` Usage
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  The following example shows the use of ``gnatmem``
  on a simple memory-leaking program.
  Suppose that we have the following Ada program:

    .. code-block:: ada

       with Unchecked_Deallocation;
       procedure Test_Gm is

          type T is array (1..1000) of Integer;
          type Ptr is access T;
          procedure Free is new Unchecked_Deallocation (T, Ptr);
          A : Ptr;

          procedure My_Alloc is
          begin
             A := new T;
          end My_Alloc;

          procedure My_DeAlloc is
             B : Ptr := A;
          begin
             Free (B);
          end My_DeAlloc;

       begin
          My_Alloc;
          for I in 1 .. 5 loop
             for J in I .. 5 loop
                My_Alloc;
             end loop;
             My_Dealloc;
          end loop;
       end;

  The program needs to be compiled with the debugging option and linked with
  the ``gmem`` library:

    ::

       $ gnatmake -g test_gm -largs -lgmem

  Then we execute the program as usual:

    ::

       $ test_gm

  Then ``gnatmem`` is invoked simply with

    ::

       $ gnatmem test_gm

  which produces the following output (result may vary on different platforms):

    ::

        Global information
        ------------------
           Total number of allocations        :  18
           Total number of deallocations      :   5
           Final Water Mark (non freed mem)   :  53.00 Kilobytes
           High Water Mark                    :  56.90 Kilobytes

        Allocation Root # 1
        -------------------
         Number of non freed allocations    :  11
         Final Water Mark (non freed mem)   :  42.97 Kilobytes
         High Water Mark                    :  46.88 Kilobytes
         Backtrace                          :
           test_gm.adb:11 test_gm.my_alloc

        Allocation Root # 2
        -------------------
         Number of non freed allocations    :   1
         Final Water Mark (non freed mem)   :  10.02 Kilobytes
         High Water Mark                    :  10.02 Kilobytes
         Backtrace                          :
           s-secsta.adb:81 system.secondary_stack.ss_init

        Allocation Root # 3
        -------------------
         Number of non freed allocations    :   1
         Final Water Mark (non freed mem)   :  12 Bytes
         High Water Mark                    :  12 Bytes
         Backtrace                          :
           s-secsta.adb:181 system.secondary_stack.ss_init


  Note that the GNAT runtime contains itself a certain number of
  allocations that have no  corresponding deallocation,
  as shown here for root #2 and root #3.
  This is a normal behavior when the number of non-freed allocations
  is one, it allocates dynamic data structures that the run time needs for
  the complete lifetime of the program. Note also that there is only one
  allocation root in the user program with a single line back trace:
  test_gm.adb:11 test_gm.my_alloc, whereas a careful analysis of the
  program shows that 'My_Alloc' is called at 2 different points in the
  source (line 21 and line 24). If those two allocation roots need to be
  distinguished, the backtrace depth parameter can be used:

    ::

       $ gnatmem 3 test_gm

  which will give the following output:


    ::

        Global information
        ------------------
           Total number of allocations        :  18
           Total number of deallocations      :   5
           Final Water Mark (non freed mem)   :  53.00 Kilobytes
           High Water Mark                    :  56.90 Kilobytes

        Allocation Root # 1
        -------------------
         Number of non freed allocations    :  10
         Final Water Mark (non freed mem)   :  39.06 Kilobytes
         High Water Mark                    :  42.97 Kilobytes
         Backtrace                          :
           test_gm.adb:11 test_gm.my_alloc
           test_gm.adb:24 test_gm
           b_test_gm.c:52 main

        Allocation Root # 2
        -------------------
         Number of non freed allocations    :   1
         Final Water Mark (non freed mem)   :  10.02 Kilobytes
         High Water Mark                    :  10.02 Kilobytes
         Backtrace                          :
           s-secsta.adb:81  system.secondary_stack.ss_init
           s-secsta.adb:283 <system__secondary_stack___elabb>
           b_test_gm.c:33   adainit

        Allocation Root # 3
        -------------------
         Number of non freed allocations    :   1
         Final Water Mark (non freed mem)   :   3.91 Kilobytes
         High Water Mark                    :   3.91 Kilobytes
         Backtrace                          :
           test_gm.adb:11 test_gm.my_alloc
           test_gm.adb:21 test_gm
           b_test_gm.c:52 main

        Allocation Root # 4
        -------------------
         Number of non freed allocations    :   1
         Final Water Mark (non freed mem)   :  12 Bytes
         High Water Mark                    :  12 Bytes
         Backtrace                          :
           s-secsta.adb:181 system.secondary_stack.ss_init
           s-secsta.adb:283 <system__secondary_stack___elabb>
           b_test_gm.c:33   adainit

  The allocation root #1 of the first example has been split in 2 roots #1
  and #3, thanks to the more precise associated backtrace.
