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

The GNAT compiler handles an incorrect Ada program in three ways:

* The illegality may be a violation of the static semantics of Ada. In
  that case, GNAT diagnoses the constructs in the program that are illegal.
  It's then a straightforward matter for you to modify those parts of
  the program.

* The illegality may be a violation of the dynamic semantics of Ada. In
  that case the program compiles and executes, but may generate incorrect
  results or may terminate abnormally with some exception.

* When presented with a program that contains convoluted errors, GNAT
  itself may terminate abnormally without providing full diagnostics on
  the incorrect user program.

.. index:: Debugger

.. index:: !  gdb

.. _The_GNAT_Debugger_GDB:

The GNAT Debugger GDB
---------------------

``GDB`` is a general purpose, platform-independent debugger that
you can use to debug mixed-language programs, including compiled with ``gcc``,
and in particular is capable of debugging Ada programs compiled with
GNAT. The latest versions of ``GDB`` are Ada-aware and can handle
complex Ada data structures.

See :title:`Debugging with GDB`,
for full details on the usage of ``GDB``, including a section on
its usage on programs. That manual should be consulted for full
details. The section that follows is a brief introduction to the
philosophy and use of ``GDB``.

When programs are compiled, the compiler optionally writes debugging
information into the generated object file, including information on
line numbers and on declared types and variables. This information is
separate from the generated code. It makes the object files considerably
larger, but it does not add to the size of the actual executable that
is loaded into memory and has no impact on run-time performance. The
generation of debug information is triggered by the use of the
:switch:`-g` switch in the ``gcc`` or ``gnatmake`` command
you used to perform the compilations. It is important to emphasize that
it's a goal of GCC, and hence GNAT, that the use of this switch does
not change the generated code.

The compiler writes the debugging information in standard system formats that
are used by many tools, including debuggers and profilers. The format
of the information is typically designed to describe C types and
semantics, but GNAT implements a translation scheme which allows full
details about Ada types and variables to be encoded into these
standard C formats. Details of this encoding scheme may be found in
the file :file:`exp_dbug.ads` in the GNAT source distribution. However, the
details of this encoding are, in most cases, of no interest to a user,
since ``GDB`` automatically performs the necessary decoding.

When a program is bound and linked, the debugging information is
collected from the object files and stored in the executable image of
the program. Again, this process significantly increases the size of
the generated executable file, but does not increase the size of
the executable program in memory. Furthermore, if this program is run in
the normal manner, it runs exactly as if the debug information were
not present and takes no more actual memory.

However, if the program is run under control of ``GDB``, the
debugger is activated.  The image of the program is loaded, at which
point it is ready to run.  If you give a run command, the program
runs exactly as it would have if ``GDB`` were not present. This
is a crucial part of the ``GDB`` design philosophy: ``GDB`` is
entirely non-intrusive until a breakpoint is encountered.  If no
breakpoint is ever hit, the program runs exactly as it would if no
debugger were present. When a breakpoint is hit, ``GDB`` accesses
the debugging information and can respond to user commands to inspect
variables and more generally to report on the state of execution.

.. _Running_GDB:

Running GDB
-----------

This section describes how to initiate the debugger.

You can launch the debugger from a ``GNAT Studio`` menu or
directly from the command line. The description below covers the latter use.
You can use all the commands shown in the ``GNAT Studio`` debug console window,
but there are usually more GUI-based ways to achieve the same effect.

The command to run ``GDB`` is

  ::

     $ gdb program

where ``program`` is the name of the executable file. This
activates the debugger and results in a prompt for debugger commands.
The simplest command is simply ``run``, which causes the program to run
exactly as if the debugger were not present. The following section
describes some of the additional commands that you can give to ``GDB``.


.. _Introduction_to_GDB_Commands:

Introduction to GDB Commands
----------------------------

``GDB`` contains a large repertoire of commands.
See :title:`Debugging with GDB` for extensive documentation on the use
of these commands, together with examples of their use. Furthermore,
the command *help* invoked from within GDB activates a simple help
facility which summarizes the available commands and their options.
In this section, we summarize a few of the most commonly
used commands to give an idea of what ``GDB`` is about. You should create
a simple program with debugging information and experiment with the use of
these ``GDB`` commands on that program as you read through the
following section.

* :samp:`set args {arguments}`
    *arguments* is a list of arguments to be passed to the program on
    a subsequent run command, just as though the arguments had been
    entered on a normal invocation of the program. You do not need the
    ``set args`` command if the program does not require arguments.


* :samp:`run`
    The ``run`` command causes execution of the program to start from
    the beginning. If the program is already running, that is to say if
    you are currently positioned at a breakpoint, then a prompt will ask
    for confirmation that you want to abandon the current execution and
    restart. You can also specify program arguments on this command and
    if you specify ``run`` with no arguments, the arguments used on
    the previous command will be used again.


* :samp:`breakpoint {location}`
    This command sets a breakpoint, that is to say a point at which
    execution will halt and ``GDB`` will await further
    commands. *location* is either a line number within a file, which
    you specify in the format ``file:linenumber``, or the name of a
    subprogram. If you request a breakpoint be set on a subprogram
    that is overloaded, either a prompt will ask you to specify on
    which of those subprograms you want to breakpoint or a breakpoint
    will be set on all of them. If the program is run and execution
    encounters the breakpoint, the program stops and ``GDB``
    signals that the breakpoint was encountered by printing the line
    of code before which the program is halted.


* :samp:`catch exception {name}`
    This command causes the program execution to stop whenever exception
    ``name`` is raised.  If you omit ``name``, execution is
    suspended when any exception is raised.


* :samp:`print {expression}`
    This prints the value of the given expression. Most
    Ada expression formats are properly handled by ``GDB``, so the expression
    can contain function calls, variables, operators, and attribute references.


* :samp:`continue`
    Continues execution following a breakpoint until the next breakpoint or the
    termination of the program.


* :samp:`step`
    Executes a single line after a breakpoint. If the next statement
    is a subprogram call, execution continues into (the first statement of)
    the called subprogram.


* :samp:`next`
    Executes a single line. If this line is a subprogram call, the
    program executes that call and returns.


* :samp:`list`

    Lists a few lines around the current source location. In practice,
    it is usually more convenient to have a separate edit window open
    with the relevant source file displayed. ``emacs`` has debugging
    modes that display both the relevant source and ``GDB`` commands
    and output.  Successive applications of this command print
    subsequent lines. You can give this command an argument which is a
    line number, in which case it displays a few lines around the
    specified line.


* :samp:`backtrace`
    Displays a backtrace of the call chain. This command is typically
    used after a breakpoint has occurred to examine the sequence of calls that
    leads to the current breakpoint. The display includes one line for each
    activation record (frame) corresponding to an active subprogram.


* :samp:`up`
    At a breakpoint, ``GDB`` can display the values of variables local
    to the current frame. You can use the command ``up`` to
    examine the contents of other active frames by moving the focus up
    the stack, that is to say from callee to caller, one frame at a time.


* :samp:`down`
    Moves the focus of ``GDB`` down from the frame currently being
    examined to the frame of its callee (the reverse of the previous command),


* :samp:`frame {n}`
    Inspect the frame with the given number. The value 0 denotes the frame
    of the current breakpoint, that is to say the top of the call stack.


* :samp:`kill`
    Kills the child process in which the program is running under GDB.
    You may find this useful for several purposes:

    * It allows you to recompile and relink your program, since on many systems
      you cannot regenerate an executable file while it is running in a process.

    * You can run your program outside the debugger on systems that do not
      permit executing a program outside GDB while breakpoints are set
      within GDB.

    * It allows you to debug a core dump rather than a running process.

The above is a very short introduction to the commands that
``GDB`` provides. Important additional capabilities, including conditional
breakpoints, the ability to execute command sequences on a breakpoint,
the ability to debug at the machine instruction level and many other
features are described in detail in :title:`Debugging with GDB`.
Note that most commands can be abbreviated
(for example, "c" for ``continue`` and "bt" for ``backtrace``) and only enough
characters need be typed to disambiguate the command (e.g., "br" for
``breakpoint``).


.. _Using_Ada_Expressions:

Using Ada Expressions
---------------------

.. index:: Ada expressions (in gdb)

``GDB`` supports a very large subset of Ada expression syntax, with some
extensions. The philosophy behind the design of this subset is

  * ``GDB`` should provide basic literals and access to operations for
    arithmetic, dereferencing, field selection, indexing, and subprogram calls,
    leaving more sophisticated computations to subprograms written into the
    program (which therefore may be called from ``GDB``).

  * Type safety and strict adherence to Ada language restrictions
    are not particularly relevant in a debugging context.

  * Brevity is important to the ``GDB`` user.

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
subprograms while debugging. You do this by simply entering
a subprogram call statement in the form:

  ::

     call subprogram-name (parameters)

You can omit the keyword ``call`` in the normal case where the
``subprogram-name`` does not coincide with any of the predefined
``GDB`` commands.

The effect is to invoke the given subprogram, passing it the
list of parameters that is supplied. The parameters you specify can be expressions and
can include variables from the program being debugged. The
subprogram must be defined
at the library level within your program and ``GDB`` will call the
subprogram within the environment of your program execution (which
means that the subprogram is free to access or even modify variables
within your program).

The most important use of this facility that you can include
debugging routines that are tailored to particular data structures
in your program. You can write such debugging routines to provide a suitably
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
value, which is not very useful. But the ``PN`` routine (defined in file
:file:`treepr.adb` in the GNAT sources) takes a tree node as input and displays
a useful high level representation of the tree node, which includes the
syntactic category of the node, its position in the source,
the descendant nodes and parent node, as well as lots of
semantic information. To study this example in more detail, you might want to
look at the body of the ``PN`` procedure in the above file.

Another useful application of this capability is to deal with situations where
complex data which are not handled suitably by GDB. For example, if you specify
Convention Fortran for a multi-dimensional array, GDB does not know that
the ordering of array elements has been switched and will not properly
address the array elements. In such a case, instead of trying to print the
elements directly from GDB, you can write a callable procedure that prints
the elements in the format you desire.


.. _Using_the_Next_Command_in_a_Function:

Using the *next* Command in a Function
--------------------------------------

When you use the ``next`` command in a function, the current source
location will advance to the next statement as usual. A special case
arises in the case of a ``return`` statement.

Part of the code for a return statement is the 'epilogue' of the function.
This is the code that returns to the caller. There is only one copy of
this epilogue code and it is typically associated with the last return
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


    In this listing, the asterisk before the first task indicates it's
    currently running task. The first column lists the task ID used
    to refer to tasks in the following commands.


.. index:: Breakpoints and tasks

* ``break`` *linespec* ``task`` *taskid*, ``break`` *linespec* ``task`` *taskid* ``if`` ...

    These commands are like the ``break ... thread ...``.
    *linespec* specifies source lines.

    Use the qualifier :samp:`task {taskid}` with a breakpoint command
    to specify that you only want ``GDB`` to stop the program when that
    particular Ada task reaches this breakpoint. *taskid* is one of the
    numeric task identifiers assigned by ``GDB``, shown in the first
    column of the ``info tasks`` display.

    If you don't specify :samp:`task {taskid}` when you set a
    breakpoint, the breakpoint applies to *all* tasks of your
    program.

    You can use the ``task`` qualifier on conditional breakpoints as
    well; in this case, place :samp:`task {taskid}` before the
    breakpoint condition (before the ``if``).

.. index:: Task switching (in gdb)

* :samp:`task {taskno}`

    This command allows switching to the task referred by *taskno*. In
    particular, it allows browsing the backtrace of the specified
    task. You should switch back to the original task before
    continuing execution; otherwise the scheduling of the program may be
    disturbed.

For more detailed information on tasking support,
see :title:`Debugging with GDB`.


.. index:: Debugging Generic Units
.. index:: Generics

.. _Debugging_Generic_Units:

Debugging Generic Units
-----------------------

GNAT always uses the code expansion mechanism for generic
instantiation. This means that each time an instantiation occurs, the
compiler makes a complete copy of the original code, with
appropriate substitutions of formals by actuals.

You can't refer to the original generic entities in ``GDB``, but you
can debug a particular instance of a generic by using the appropriate
expanded names. For example, if we have

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
instance in the normal manner and examine the values of local
variables, as you do for other units.


.. index:: Remote Debugging with gdbserver

.. _Remote_Debugging_with_gdbserver:

Remote Debugging with gdbserver
-------------------------------

On platforms that support ``gdbserver``, you can use this tool
to debug your application remotely.  This can be useful in situations
where the program needs to be run on a target host that is different
from the host used for development, particularly when the target has
a limited amount of resources (either CPU and/or memory).

To do so, start your program using ``gdbserver`` on the target machine.
``gdbserver`` automatically suspends the execution of your program
at its entry point, waiting for a debugger to connect to it.  You use the
following commands to start an application and tell ``gdbserver`` to
wait for a connection with the debugger on ``localhost`` port 4444.


  ::

     $ gdbserver localhost:4444 program
     Process program created; pid = 5685
     Listening on port 4444

Once ``gdbserver`` has started listening, you can tell the debugger to
establish a connection with this ``gdbserver``, and then start a
debugging session as if the program was being debugged on the
same host, directly under the control of ``GDB``.

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

You can also use ``gdbserver`` to attach to an already running
program, in which case the execution of that program is suspended
until you have established the connection between the debugger and ``gdbserver``.

For more information on how to use ``gdbserver``, see the *Using the
gdbserver Program* section in :title:`Debugging with GDB`.  GNAT
provides support for ``gdbserver`` on x86-linux, x86-windows and
x86_64-linux.


.. index:: Abnormal Termination or Failure to Terminate

.. _GNAT_Abnormal_Termination_or_Failure_to_Terminate:

GNAT Abnormal Termination or Failure to Terminate
-------------------------------------------------

When presented with programs that contain serious errors in syntax
or semantics,
GNAT may, on rare occasions, experience problems such
as aborting with a
segmentation fault or illegal memory access, raising an internal
exception, terminating abnormally, or failing to terminate at all.
In such cases, you can activate
various features of GNAT that can help you pinpoint the construct in your
program that is the likely source of the problem.

The following strategies for you to use in such cases are presented in
increasing order of difficulty, corresponding to your experience in
using GNAT and your familiarity with compiler internals.

* Run ``gcc`` with the :switch:`-gnatf`. This switch causes all errors
  on a given line to be reported. In its absence, GNAT only displays
  the first error on a line.

  The :switch:`-gnatdO` switch causes errors to be displayed as soon as they
  are encountered, rather than after compilation is terminated. If GNAT
  terminates prematurely or goes into an infinite loop, the last error
  message displayed may help to pinpoint the culprit.

* Run ``gcc`` with the :switch:`-v` (verbose) switch. In this
  mode, ``gcc`` produces ongoing information about the progress of the
  compilation and provides the name of each procedure as code is
  generated. This switch allows you to find which Ada procedure was being
  compiled when it encountered a problem.

.. index:: -gnatdc switch

* Run ``gcc`` with the :switch:`-gnatdc` switch. This is a GNAT specific
  switch that does for the front-end what :switch:`-v` does
  for the back end. The system prints the name of each unit,
  either a compilation unit or nested unit, as it is being analyzed.

* Finally, you can start
  ``gdb`` directly on the ``gnat1`` executable. ``gnat1`` is the
  front-end of GNAT and can be run independently (normally it is just
  called from ``gcc``). You can use ``gdb`` on ``gnat1`` as you
  would on a C program (but :ref:`The_GNAT_Debugger_GDB` for caveats). The
  ``where`` command is the first line of attack; the variable
  ``lineno`` (seen by ``print lineno``), used by the second phase of
  ``gnat1`` and by the ``gcc`` back end, indicates the source line at
  which the execution stopped, and ``input_file name`` indicates the name of
  the source file.


.. _Naming_Conventions_for_GNAT_Source_Files:

Naming Conventions for GNAT Source Files
----------------------------------------

In order to bettter understand the workings of the GNAT system, the following
brief description of its organization may be helpful:

* Files with prefix :file:`sc` contain the lexical scanner.

* All files prefixed with :file:`par` are components of the parser. The
  numbers correspond to chapters of the Ada Reference Manual. For example,
  parsing of select statements can be found in :file:`par-ch9.adb`.

* All files prefixed with :file:`sem` perform semantic analysis. The
  numbers correspond to chapters of the Ada standard. For example, all
  issues involving context clauses can be found in
  :file:`sem_ch10.adb`. In addition, some features of the language
  require sufficient special processing to justify their own semantic
  files, such as :file:`sem_aggr.adb` for aggregates and
  :file:`sem_disp.adb` for dynamic dispatching.

* All files prefixed with :file:`exp` perform normalization and
  expansion of the intermediate representation (abstract syntax tree, or AST).
  The expansion has the effect of lowering the semantic level of the AST to
  a level closer to what the back end can handle. For example, it converts
  tasking operations into calls to the appropriate runtime routines.
  These files use the same numbering scheme as the parser and semantics files.
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

* The files prefixed with :file:`gen_il` generate most of the functions
  defined in :file:`sinfo.ads` and :file:`einfo.ads`, which set and get
  various fields and flags of the AST.

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
does too, except GNAT internal debugging switches and modes are not
secret. A summary and full description of all the compiler and binder
debug flags are in the file :file:`debug.adb`. You must obtain the
sources of the compiler to see the full detailed effects of these flags.

The switches that print the source of the program (reconstructed from
the internal tree) are of general interest for user programs, as are the
options to print
the full internal tree and the entity table (the symbol table
information). The reconstructed source provides a readable version of the
program after the front-end has completed analysis and  expansion
and is useful when studying the performance of specific constructs.
For example, constraint checks are shown explicitly, complex aggregates
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
is enabled and no exception is raised during program execution.

.. index:: traceback, non-symbolic

.. _Non-Symbolic_Traceback:

Non-Symbolic Traceback
^^^^^^^^^^^^^^^^^^^^^^

Note: this feature is not supported on all platforms. See
:samp:`GNAT.Traceback` spec in :file:`g-traceb.ads`
for a complete list of supported platforms.

.. rubric:: Tracebacks From an Unhandled Exception

A runtime non-symbolic traceback is a list of addresses of call
instructions.  To enable this feature you must use the :switch:`-E`
``gnatbind`` switch. With this switch, a stack traceback is stored at
runtime as part of exception information.

You can translate this information using the ``addr2line`` tool, provided that
the program is compiled with debugging options (see :ref:`Switches_for_gcc`)
and linked at a fixed position with :switch:`-no-pie`.

Here's a simple example with ``gnatmake``:

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

     $ gnatmake stb -g -bargs -E -largs -no-pie
     $ stb

     Execution of stb terminated by unhandled exception
     raised CONSTRAINT_ERROR : stb.adb:5 explicit raise
     Load address: 0x400000
     Call stack traceback locations:
     0x401373 0x40138b 0x40139c 0x401335 0x4011c4 0x4011f1 0x77e892a4

As we can see, the traceback lists a sequence of addresses for the unhandled
exception ``CONSTRAINT_ERROR`` raised in procedure P1. It's easy to
see that this exception come from procedure P1. To translate these
addresses into the source lines where the calls appear, you need to
invoke the ``addr2line`` tool like this:

  ::

     $ addr2line -e stb 0x401373 0x40138b 0x40139c 0x401335 0x4011c4
        0x4011f1 0x77e892a4

     d:/stb/stb.adb:5
     d:/stb/stb.adb:10
     d:/stb/stb.adb:14
     d:/stb/b~stb.adb:197
     crtexe.c:?
     crtexe.c:?
     ??:0

The ``addr2line`` tool has several other useful options:

  =========================  ====================================================
  :samp:`-a --addresses`     to show the addresses alongside the line numbers
  :samp:`-f --functions`     to get the function name corresponding to a location
  :samp:`-p --pretty-print`  to print all the information on a single line
  :samp:`--demangle=gnat`    to use the GNAT decoding mode for the function names
  =========================  ====================================================

  ::

     $ addr2line -e stb -a -f -p --demangle=gnat 0x401373 0x40138b
        0x40139c 0x401335 0x4011c4 0x4011f1 0x77e892a4

     0x00401373: stb.p1 at d:/stb/stb.adb:5
     0x0040138B: stb.p2 at d:/stb/stb.adb:10
     0x0040139C: stb at d:/stb/stb.adb:14
     0x00401335: main at d:/stb/b~stb.adb:197
     0x004011c4: ?? at crtexe.c:?
     0x004011f1: ?? at crtexe.c:?
     0x77e892a4: ?? ??:0


From this traceback, we can see that the exception was raised in :file:`stb.adb`
at line 5, which was reached from a procedure call in :file:`stb.adb` at line
10, and so on. :file:`b~std.adb` is the binder file, which contains the
call to the main program; :ref:`Running_gnatbind`. The remaining entries are
assorted runtime routines. The output will vary from platform to platform.

You can also use ``GDB`` with these traceback addresses to debug
the program. For example, we can break at a given code location, as reported
in the stack traceback::

     $ gdb -nw stb

     (gdb) break *0x401373
     Breakpoint 1 at 0x401373: file stb.adb, line 5.

It is important to note that the stack traceback addresses do not change when
debug information is included. This is particularly useful because it makes it
possible to release software without debug information (to minimize object
size), get a field report that includes a stack traceback whenever an internal
bug occurs, and then be able to retrieve the sequence of calls with the same
program compiled with debug information.

However the ``addr2line`` tool does not work with Position-Independent Code
(PIC), the historical example being Linux dynamic libraries and Windows DLLs,
which nowadays encompasse Position-Independent Executables (PIE) on recent
Linux and Windows versions.

In order to translate addresses the source lines with Position-Independent
Executables on recent Linux and Windows versions, in other words without
using the switch :switch:`-no-pie` during linking, you need to use the
``gnatsymbolize`` tool with :switch:`--load` instead of the ``addr2line``
tool. The main difference is that you need to copy the Load Address output
in the traceback ahead of the sequence of addresses. The default mode
of ``gnatsymbolize`` is equivalent to that of ``addr2line`` with the above
switches, so none of them are needed::

     $ gnatmake stb -g -bargs -E
     $ stb

     Execution of stb terminated by unhandled exception
     raised CONSTRAINT_ERROR : stb.adb:5 explicit raise
     Load address: 0x400000
     Call stack traceback locations:
     0x401373 0x40138b 0x40139c 0x401335 0x4011c4 0x4011f1 0x77e892a4

     $ gnatsymbolize --load stb 0x400000 0x401373 0x40138b 0x40139c 0x401335 \
        0x4011c4 0x4011f1 0x77e892a4

     0x00401373 Stb.P1 at stb.adb:5
     0x0040138B Stb.P2 at stb.adb:10
     0x0040139C Stb at stb.adb:14
     0x00401335 Main at b~stb.adb:197
     0x004011c4 __tmainCRTStartup at ???
     0x004011f1 mainCRTStartup at ???
     0x77e892a4 ??? at ???


.. rubric:: Tracebacks From Exception Occurrences

Non-symbolic tracebacks are obtained by using the :switch:`-E` binder switch.
The stack traceback is attached to the exception information string and you can
retrieve it in an exception handler within the Ada program by means of the
Ada facilities defined in ``Ada.Exceptions``. Here's a simple example:

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

  ::

     $ gnatmake stb -g -bargs -E -largs -no-pie
     $ stb

     raised CONSTRAINT_ERROR : stb.adb:12 range check failed
     Load address: 0x400000
     Call stack traceback locations:
     0x4015e4 0x401633 0x401644 0x401461 0x4011c4 0x4011f1 0x77e892a4


.. rubric:: Tracebacks From Anywhere in a Program

You can also retrieve a stack traceback from anywhere in a program.
For this, you need to use the ``GNAT.Traceback`` API. This package includes a
procedure called ``Call_Chain`` that computes a complete stack traceback as
well as useful display procedures described below. You don't have to use
the :switch:`-E` ``gnatbind`` switch in this case because the stack traceback
mechanism is invoked explicitly.

In the following example, we compute a traceback at a specific location in the
program and display it using ``GNAT.Debug_Utilities.Image`` to convert
addresses to strings:


  .. code-block:: ada

      with Ada.Text_IO;
      with GNAT.Traceback;
      with GNAT.Debug_Utilities;
      with System;

      procedure STB is

         use Ada;
         use Ada.Text_IO;
         use GNAT;
         use GNAT.Traceback;
         use System;

         LA : constant Address := Executable_Load_Address;

         procedure P1 is
            TB  : Tracebacks_Array (1 .. 10);
            --  We are asking for a maximum of 10 stack frames.
            Len : Natural;
            --  Len will receive the actual number of stack frames returned.
         begin
            Call_Chain (TB, Len);

            Put ("In STB.P1 : ");

            for K in 1 .. Len loop
               Put (Debug_Utilities.Image_C (TB (K)));
               Put (' ');
            end loop;

            New_Line;
         end P1;

         procedure P2 is
         begin
            P1;
         end P2;

      begin
         if LA /= Null_Address then
            Put_Line ("Load address: " & Debug_Utilities.Image_C (LA));
         end if;

         P2;
      end STB;

  ::

     $ gnatmake stb -g
     $ stb

     Load address: 0x400000
     In STB.P1 : 0x40F1E4 0x4014F2 0x40170B 0x40171C 0x401461 0x4011C4 \
       0x4011F1 0x77E892A4


You can get even more information by invoking the ``addr2line`` tool or
the ``gnatsymbolize`` tool as described earlier (note that the hexadecimal
addresses need to be specified in C format, with a leading '0x').

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
with debug information. If you do not compile it with debug information,
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

      $ gnatmake -g stb -bargs -E
      $ stb

      0040149F in stb.p1 at stb.adb:8
      004014B7 in stb.p2 at stb.adb:13
      004014CF in stb.p3 at stb.adb:18
      004015DD in ada.stb at stb.adb:22
      00401461 in main at b~stb.adb:168
      004011C4 in __mingw_CRTStartup at crt1.c:200
      004011F1 in mainCRTStartup at crt1.c:222
      77E892A4 in ?? at ??:0

.. rubric:: Tracebacks From Anywhere in a Program

You can get a symbolic stack traceback
from anywhere in a program, just as you can for non-symbolic tracebacks.
The first step is to obtain a non-symbolic
traceback. Then call ``Symbolic_Traceback`` to compute the symbolic
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

You may also enable symbolic tracebacks by using
the :switch:`-Es` switch to gnatbind (as in ``gprbuild -g ... -bargs -Es``).
This causes the Exception_Information to contain a symbolic
traceback, which will also be printed if an unhandled exception
terminates the program.


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
``GDB`` up to the ``Map.Clear`` statement, trying to print ``Map`` will
yield information that is only relevant to the developers of the standard
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

Fortunately, ``GDB ``has a feature called `pretty-printers
<http://docs.adacore.com/gdb-docs/html/gdb.html#Pretty_002dPrinter-Introduction>`_,
which allows customizing how ``GDB`` displays data structures. The
``GDB`` shipped with GNAT embeds such pretty-printers for the most
common containers in the standard library.  To enable them, either run
the following command manually under ``GDB`` or add it to your
:file:`.gdbinit` file:

  ::

      python import gnatdbg; gnatdbg.setup()

Once you've done this, ``GDB``'s ``print`` command will automatically use
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

To display a value without involving pretty-printers, you can invoke
``print`` with its ``/r`` option:

  ::

      (gdb) print/r map
      $1 = (
        tree => (...

You can also obtain finer control of pretty-printers: see `GDB's online
documentation
<http://docs.adacore.com/gdb-docs/html/gdb.html#Pretty_002dPrinter-Commands>`_
for more information.


.. index:: Profiling


.. _Profiling:

Profiling
=========

This section describes how to use the ``gprof`` profiler tool on Ada programs.

.. index:: !  gprof
.. index:: Profiling

.. _Profiling_an_Ada_Program_with_gprof:

Profiling an Ada Program with gprof
-----------------------------------

This section is not meant to be an exhaustive documentation of ``gprof``.
You can find full documentation for it in the :title:`GNU Profiler User's Guide`
documentation that is part of this GNAT distribution.

Profiling a program helps determine the parts of a program that are executed
most often and are therefore the most time-consuming.

``gprof`` is the standard GNU profiling tool; it has been enhanced to
better handle Ada programs and multitasking.
It's currently supported on the following platforms

* Linux x86/x86_64
* Windows x86/x86_64 (without PIE support)

In order to profile a program using ``gprof``, you need to perform the
following steps:

#. Instrument the code, which requires a full recompilation of the project with the
   proper switches.

#. Execute the program under the analysis conditions, i.e. with the desired
   input.

#. Analyze the results using the ``gprof`` tool.

The following sections detail the different steps and indicate how
to interpret the results.


.. _Compilation_for_profiling:

Compilation for profiling
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: -pg (gcc), for profiling
.. index:: -pg (gnatlink), for profiling

In order to profile a program, you must first to tell the compiler
to generate the necessary profiling information. You do this using the compiler switch :switch:`-pg`, which you must add to other compilation switches. You need
to specify this
switch during compilation and link stages, and you can
specified it only once when using ``gnatmake``:

  ::

     $ gnatmake -f -pg -P my_project

Note that only the objects that were compiled with the :switch:`-pg` switch will
be profiled; if you need to profile your whole project, use the :switch:`-f`
``gnatmake`` switch to force full recompilation.

Note that on Windows, ``gprof`` does not support PIE. You should add
the :switch:`-no-pie` switch to the linker flags to disable PIE.


.. _Program_execution:


Program execution
^^^^^^^^^^^^^^^^^

Once the program has been compiled for profiling, you can run it as usual.

The only constraint imposed by profiling is that the program must terminate
normally. An interrupted program (via a Ctrl-C, kill, etc.) will not be
properly analyzed.

Once the program completes execution, a data file called :file:`gmon.out` is
generated in the directory where the program was launched from. If this file
already exists, it will be overwritten by running the program.


.. _Running_gprof:

Running gprof
^^^^^^^^^^^^^

You can call the ``gprof`` tool as follows:

  ::

     $ gprof my_prog gmon.out

or simply:

  ::

    $ gprof my_prog

The complete form of the ``gprof`` command line is the following:

  ::

     $ gprof [switches] [executable [data-file]]

``gprof`` supports numerous switches, whose order does not matter. You
can find the full list of switches in the :title:`GNU Profiler
User's Guide`.

The following are the most relevant of those switches:

.. index:: --demangle (gprof)

:switch:`--demangle[={style}]`, :switch:`--no-demangle`
  These switches control whether symbol names should be demangled when
  printing output.  The default is to demangle C++ symbols.  You can use
  :switch:`--no-demangle` to turn off demangling. Different
  compilers have different mangling styles.  The optional demangling style
  argument can be used to choose an appropriate demangling style for your
  compiler, in particular Ada symbols generated by GNAT can be demangled using
  :switch:`--demangle=gnat`.


.. index:: -e (gprof)

:switch:`-e {function_name}`
  The :switch:`-e {function}` option tells ``gprof`` not to print
  information about the function ``function_name`` and its
  children in the call graph.  The function will still be listed
  as a child of any functions that call it, but its index number will be
  shown as ``[not printed]``.  You may specify more than one :switch:`-e` switch,
  but you may only include one ``function_name``  with each :switch:`-e`
  switch.


.. index:: -E (gprof)

:switch:`-E {function_name}`
  The :switch:`-E {function}` switch works like the :switch:`-e` switch, but
  execution time spent in the function (and children who were not called from
  anywhere else) will not be used to compute the percentages-of-time for
  the call graph.  You may specify more than one :switch:`-E` switch, but
  you may only include one  ``function_name`` with each :switch:`-E` switch.


.. index:: -f (gprof)

:switch:`-f {function_name}`
  The :switch:`-f {function}` switch causes ``gprof`` to limit the
  call graph to the function ``function_name`` and its children and
  their children.  You may specify more than one :switch:`-f` switch,
  but you may only include one ``function_name`` with each :switch:`-f` switch.


.. index:: -F (gprof)

:switch:`-F {function_name}`
  The :switch:`-F {function}` switch works like the :switch:`-f` switch, but
  only time spent in the function and its children and their
  children will be used to determine total-time and
  percentages-of-time for the call graph.  You may specify more than one
  :switch:`-F` switch, but you may include only one ``function_name``  with each
  :switch:`-F` switch.  The :switch:`-F` switch overrides the :switch:`-E`
  switch.


.. _Interpretation_of_profiling_results:

Interpretation of profiling results
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The results of the profiling analysis are represented by two arrays:
the 'flat profile' and the 'call graph'. You can find full
documentation of those outputs in the :title:`GNU Profiler User's Guide`.

The flat profile shows the time spent in each function of the program and how
many time it has been called. This allows you to easily locate the most
time-consuming functions.

The call graph shows, for each subprogram, the subprograms that call it,
and the subprograms that it calls. It also provides an estimate of the time
spent in each of those callers and called subprograms.



.. _Improving_Performance:

Improving Performance
=====================

.. index:: Improving performance

This section presents several topics related to program performance.
It first describes some of the tradeoffs that you need to consider
and some of the techniques for making your program run faster.

It then documents the unused subprogram/data elimination feature,
which can reduce the size of program executables.

.. _Performance_Considerations:

Performance Considerations
--------------------------

The GNAT system provides a number of options that allow a trade-off
between:

* performance of the generated code

* speed of compilation

* minimization of dependences and recompilation

* the degree of run-time checking.

The default (if you don't select any switches) aims at improving the speed
of compilation and minimizing dependences, at the expense of performance
of the generated code and consists of:

* no optimization

* no inlining of subprogram calls

* all run-time checks enabled except overflow and elaboration checks

These options are suitable for most program development purposes. This
section describes how you can modify these choices and also provides
some guidelines on debugging optimized code.


.. _Controlling_Run-Time_Checks:

Controlling Run-Time Checks
^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, GNAT generates all run-time checks, except stack overflow
checks and checks for access before elaboration on subprogram
calls. The latter are not required in default mode because all
necessary checking is done at compile time.

.. index:: -gnatp (gcc)
.. index:: -gnato (gcc)

The GNAT switch, :switch:`-gnatp` allows you to modify this default; see
:ref:`Run-Time_Checks`.

Our experience is that the default is suitable for most development
purposes.

Elaboration checks are off by default and also not needed by default
since GNAT uses a static elaboration analysis approach that avoids the
need for run-time checking. This manual contains a full chapter
discussing the issue of elaboration checks and you should read this
chapter if the default is not satisfactory for your use,

For validity checks, the minimal checks required by the Ada Reference
Manual (for case statements and assignments to array elements) are enabled
by default. You can suppress these by using the :switch:`-gnatVn` switch.
Note that in Ada 83, there were no validity checks, so if the Ada 83 mode
is acceptable (or when comparing GNAT performance with an Ada 83 compiler),
it may be reasonable to routinely use :switch:`-gnatVn`. Validity checks
are also suppressed entirely if you use :switch:`-gnatp`.

.. index:: Overflow checks
.. index:: Checks, overflow

.. index:: Suppress
.. index:: Unsuppress
.. index:: pragma Suppress
.. index:: pragma Unsuppress

Note that the setting of the switches controls the default setting of
the checks. You may modify them using either ``pragma Suppress`` (to
remove checks) or ``pragma Unsuppress`` (to add back suppressed
checks) in your program source.


.. _Use_of_Restrictions:

Use of Restrictions
^^^^^^^^^^^^^^^^^^^

You can use pragma Restrictions to control which features are
permitted in your program. In most cases, the use of this pragma
itself does not affect the generated code (but, of course, if you
avoid relatively expensive features like finalization, you'll have
more efficient programs and that's enforceable by the use of pragma
Restrictions (No_Finalization).

One notable exception to this rule is that the possibility of task abort
results in some distributed overhead, particularly if finalization or
exception handlers are used. This is because certain sections of code
must be marked as non-abortable.

If you use neither the ``abort`` statement nor asynchronous transfer
of control (``select ... then abort``), this distributed overhead can
be removed, which may have a general positive effect in improving
overall performance, especially in code involving frequent use of
tasking constructs and controlled types, which will show much improved
performance.  The relevant restrictions pragmas are

  .. code-block:: ada

      pragma Restrictions (No_Abort_Statements);
      pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

We recommend that you use these restriction pragmas if possible. If you do
this, it also means you can write code without worrying about the
possibility of an immediate abort at any point.


.. _Optimization_Levels:

Optimization Levels
^^^^^^^^^^^^^^^^^^^

.. index:: -O (gcc)

Without any optimization switch, the compiler's goal is to reduce the
cost of compilation and to make debugging produce the expected
results.  This means that statements are independent: if you stop the
program with a breakpoint between statements, you can then assign a
new value to any variable or change the program counter to any other
statement in the subprogram and get exactly the results you would
expect from the source code. However, the generated programs are
considerably larger and slower than when optimization is enabled.

Turning on optimization makes the compiler attempt to improve the
performance and/or code size at the expense of compilation time and
possibly the ability to debug the program.

If you use multiple :switch:`-O` switches, with or without level
numbers, the last such switch is the one that's used.

You can use the
:switch:`-O` switch (the permitted forms are :switch:`-O0`, :switch:`-O1`
:switch:`-O2`, :switch:`-O3`, and :switch:`-Os`)
to ``gcc`` to control the optimization level:


* :switch:`-O0`
    No optimization (the default);
    generates unoptimized code but has
    the fastest compilation time. Debugging is easiest with this switch.

    Note that many other compilers do substantial optimization even if
    'no optimization' is specified. With GCC, it is very unusual to
    use :switch:`-O0` for production if execution time is of any
    concern, since :switch:`-O0` means (almost) no optimization. You
    should keep this difference between GCC and other compilers in
    mind when doing performance comparisons.

* :switch:`-O1`
    Moderate optimization; optimizes reasonably well but does not
    degrade compilation time significantly. You may not be able to see
    some variables in the debugger and changing the value of some
    variables in the debugger may not have the effect you desire.

* :switch:`-O2`
    Full optimization;
    generates highly optimized code and has
    the slowest compilation time. You may see significant impacts on
    your ability to display and modify variables in the debugger.

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
the :switch:`-O` settings and a number of :switch:`-f` switches that
individually enable or disable specific optimizations.

Unlike some other compilation systems, GCC has
been tested extensively at all optimization levels. There are some bugs
which appear only with optimization turned on, but there have also been
bugs which show up only in *unoptimized* code. Selecting a lower
level of optimization does not improve the reliability of the code
generator, which in practice is highly reliable at all optimization
levels.

A note regarding the use of :switch:`-O3`: The use of this optimization level
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
    loop to the beginning of the loop.

  - *Instruction scheduling:* moving instructions so as to
    overlap loads and stores (typically) with other code or in
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

  - A variable may be dead and its register re-used.  This is
    probably the most common cause.

  - As mentioned above, the assignment of a value to a variable may
    have been moved.

  - A variable may be eliminated entirely by value propagation or
    other means.  In this case, GCC may incorrectly generate debugging
    information for the variable

  In general, when an unexpected value appears for a local variable or parameter
  you should first ascertain if that value was actually computed by
  your program as opposed to being incorrectly reported by the debugger.
  Record fields or
  array elements in an object designated by an access value
  are generally less of a problem once you have verified that the access
  value is sensible.
  Typically, this means checking variables in the preceding code and in the
  calling subprogram to verify that the value observed is explainable from other
  values (you must apply the procedure recursively to those
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

* The called subprogram is suitable for inlining: it must be small enough
  and not contain something that the back end cannot support in inlined
  subprograms.

  .. index:: pragma Inline
  .. index:: Inline

* Any one of the following applies: ``pragma Inline`` is applied to the
  subprogram; the subprogram is local to the unit and called once from
  within it; the subprogram is small and optimization level :switch:`-O2` is
  specified; optimization level :switch:`-O3` is specified; or the subprogram
  is an expression function.

Calls to subprograms in |withed| units are normally not inlined.
To achieve inlining in those case (that is, replacement of the call by the code
in the body of the subprogram), the following conditions must all be true:

* The optimization level is at least :switch:`-O1`.

* The called subprogram is suitable for inlining: It must be small enough
  and not contain something that the back end cannot support in inlined
  subprograms.

* There is a ``pragma Inline`` for the subprogram.

* The :switch:`-gnatn` switch is used on the command line.

Even if all these conditions are met, it may not be possible for
the compiler to inline the call due to the length of the body,
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
occurs whether or not the call is in fact inlined by the back end.

The use of front end inlining with :switch:`-gnatN` generates similar
additional dependencies.

.. index:: -fno-inline (gcc)

Note: The :switch:`-fno-inline` switch overrides all other conditions
and ensures that no inlining occurs, unless requested with pragma
Inline_Always for most back ends. The extra dependences resulting from
:switch:`-gnatn` will still be active, even if this switch is used to
suppress the resulting inlining actions.

.. index:: -fno-inline-functions (gcc)

For the GCC back end, you can use the
:switch:`-fno-inline-functions` switch to prevent automatic inlining
of subprograms if you use :switch:`-O3`.

.. index:: -fno-inline-small-functions (gcc)

For the GCC back end, you can use the
:switch:`-fno-inline-small-functions` switch to prevent automatic
inlining of small subprograms if you use :switch:`-O2`.

.. index:: -fno-inline-functions-called-once (gcc)

For the GC back end, you can use the
:switch:`-fno-inline-functions-called-once` switch to prevent inlining
of subprograms local to the unit and called once from within it if you
use :switch:`-O1`.

A note regarding the use of :switch:`-O3`: :switch:`-gnatn` is made up of two
sub-switches :switch:`-gnatn1` and :switch:`-gnatn2` that you can directly
specify. :switch:`-gnatn` is translated into one of them
based on the optimization level. With :switch:`-O2` or below, :switch:`-gnatn`
is equivalent to :switch:`-gnatn1` which activates pragma ``Inline`` with
moderate inlining across modules. With :switch:`-O3`, :switch:`-gnatn` is
equivalent to :switch:`-gnatn2` which activates pragma ``Inline`` with
full inlining across modules. If you have used pragma ``Inline`` in
appropriate cases, it's usually much better to use :switch:`-O2`
and :switch:`-gnatn` and avoid the use of :switch:`-O3` which has the additional
effect of inlining subprograms you did not think should be inlined. We have
found that the use of :switch:`-O3` may slow down the compilation and increase
the code size by performing excessive inlining, leading to increased
instruction cache pressure from the increased code size and thus minor
performance degradations. So the bottom line here is that you should not
automatically assume that :switch:`-O3` is better than :switch:`-O2` and
indeed you should use :switch:`-O3` only if tests show that it actually
improves performance for your program.

.. _Floating_Point_Operations:

Floating Point Operations
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Floating-Point Operations

On almost all targets, GNAT maps ``Float`` and ``Long_Float`` to the 32-bit and
64-bit standard IEEE floating-point representations and operations will
use standard IEEE arithmetic as provided by the processor. On most, but
not all, architectures, the attribute ``Machine_Overflows`` is ``False`` for these
types, meaning that the semantics of overflow is implementation-defined.
In the case of GNAT, these semantics correspond to the normal IEEE
treatment of infinities and NaN (not a number) values. For example,
1.0 / 0.0 yields plus infinitiy and 0.0 / 0.0 yields a NaN. By
avoiding explicit overflow checks, the performance is greatly improved
on many targets. However, if required, you can enable floating-point overflow
by using the pragma ``Check_Float_Overflow``.

Another consideration that applies specifically to x86 32-bit
architectures is which form of floating-point arithmetic is used.
By default, the operations use the old style x86 floating-point,
which implements an 80-bit extended precision form (on these
architectures the type ``Long_Long_Float`` corresponds to that form).
In addition, generation of efficient code in this mode means that
the extended precision form is used for intermediate results.
This may be helpful in improving the final precision of a complex
expression, but it means that the results obtained on the x86
may be different from those on other architectures and, for some
algorithms, the extra intermediate precision can be detrimental.

In addition to this old-style floating-point, all modern x86 chips
implement an alternative floating-point operation model referred
to as SSE2. In this model, there is no extended form and
execution performance is significantly enhanced. To force GNAT to use
this more modern form, use both of the switches:

   -msse2 -mfpmath=sse

A unit compiled with these switches will automatically use the more
efficient SSE2 instruction set for ``Float`` and ``Long_Float`` operations.
Note that the ABI has the same form for both floating-point models,
so you can mix units compiled with and without these switches.





.. _Vectorization_of_loops:

Vectorization of loops
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Optimization Switches

The GCC and LLVM back ends have an auto-vectorizer that's enabled by
default at some optimization levels.  For the GCC back end, it's
enabled by default at :switch:`-O3` and you can request it at other
levels with :switch:`-ftree-vectorize`. For the LLVM back end, it's
enabled by default at lower levels, but you can explicitly enable or
disable it with the :switch:`-fno-vectorize`, :switch:`-fvectorize`,
:switch:`-fno-slp-vectorize`, and :switch:`-fslp-vectorize` switches.

To get auto-vectorization, you also need to make sure that the target
architecture features a supported SIMD instruction set.  For example,
for the x86 architecture, you should at least specify :switch:`-msse2`
to get significant vectorization (but you don't need to specify it for
x86-64 as it is part of the base 64-bit architecture).  Similarly, for
the PowerPC architecture, you should specify :switch:`-maltivec`.

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
addition and some multiplication operators are generally supported, as
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
types.  This is because the less information the compiler has about the
bounds of the array, the more fallback code it needs to generate in order to
fix things up at run time.

You can specify that a given loop should be subject to vectorization
preferably to other optimizations by means of pragma ``Loop_Optimize``:

  .. code-block:: ada

      pragma Loop_Optimize (Vector);

placed immediately within the loop will convey the appropriate hint to the
compiler for this loop. This is currently only supported for the GCC
back end.

You can also help the compiler generate better vectorized code
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
This is also currently only supported by the GCC back end.


.. _Other_Optimization_Switches:

Other Optimization Switches
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Optimization Switches

You can also use any specialized optimization switches supported by
the back end being used.  These switches have not been extensively
tested with GNAT but can generally be expected to work. Examples of
switches in this category for the GCC back end are
:switch:`-funroll-loops` and the various target-specific :switch:`-m`
options (in particular, it has been observed that :switch:`-march=xxx`
can significantly improve performance on appropriate machines). For
full details of these switches, see the *Submodel Options* section in
the *Hardware Models and Configurations* chapter of :title:`Using the
GNU Compiler Collection (GCC)`.


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

     procedure M is
        type Int1 is new Integer;
        I1 : Int1;

        type Int2 is new Integer;
        type A2 is access Int2;
        V2 : A2;
        ...

     begin
        ...
        for J in Data'Range loop
           if Data (J) = I1 then
              V2.all := V2.all + 1;
           end if;
        end loop;
        ...
     end;

Here, since ``V2`` can only access objects of type ``Int2``
and ``I1`` is not one of them, there is no possibility that the assignment
to ``V2.all`` affects the value of ``I1``. This means that the compiler
optimizer can infer that the value ``I1`` is constant for all iterations
of the loop and load it from memory only once, before entering the loop,
instead of in every iteration (this is called load hoisting).

This kind of optimizations, based on strict type-based aliasing, is
triggered by specifying an optimization level of :switch:`-O2` or
higher (or :switch:`-Os`) for the GCC back end and :switch:`-O1` or
higher for the LLVM back end and allows the compiler to generate more
efficient code.

However, although this optimization is always correct in terms of the
formal semantics of the Ada Reference Manual, you can run into
difficulties arise if you use features like ``Unchecked_Conversion``
to break the typing system. Consider the following complete program
example:

  .. code-block:: ada

      package P1 is
         type Int1 is new Integer;
         type A1 is access Int1;

         type Int2 is new Integer;
         type A2 is access Int2;
      end P1;

      with P1; use P1;
      package P2 is
         function To_A2 (Input : A1) return A2;
      end p2;

      with Ada.Unchecked_Conversion;
      package body P2 is
         function To_A2 (Input : A1) return A2 is
            function Conv is
              new Ada.Unchecked_Conversion (A1, A2);
         begin
            return Conv (Input);
         end To_A2;
      end P2;

      with P1; use P1;
      with P2; use P2;
      with Text_IO; use Text_IO;
      procedure M is
         V1 : A1 := new Int1;
         V2 : A2 := To_A2 (V1);
      begin
         V1.all := 1;
         V2.all := 0;
         Put_Line (Int1'Image (V1.all));
      end;

This program prints out ``0`` in :switch:`-O0` mode,
but it prints out ``1`` in :switch:`-O2` mode. That's because in strict
aliasing mode, the compiler may and does assume that the assignment to
``V2.all`` could not affect the value of ``V1.all``, since different
types are involved.

This behavior is not a case of non-conformance with the standard, since
the Ada RM specifies that an unchecked conversion where the resulting
bit pattern is not a correct value of the target type can result in an
abnormal value and attempting to reference an abnormal value makes the
execution of a program erroneous.  That's the case here since the result
does not point to an object of type ``Int2``.  This means that the effect
is entirely unpredictable.

However, although that explanation may satisfy a language lawyer, in
practice, you probably expect an unchecked conversion
involving pointers to create true aliases and the behavior of printing
``1`` is questionable. In this case, the strict type-based aliasing
optimizations are clearly unwelcome.

Indeed, the compiler recognizes this possibility and the instantiation of
Unchecked_Conversion generates a warning:

  ::

     p2.adb:5:07: warning: possible aliasing problem with type "A2"
     p2.adb:5:07: warning: use -fno-strict-aliasing switch for references
     p2.adb:5:07: warning:  or use "pragma No_Strict_Aliasing (A2);"

Unfortunately the problem is only recognized when compiling the body of
package ``P2``, but the actual problematic code is generated while
compiling the body of ``M`` and this latter compilation does not see
the suspicious instance of ``Unchecked_Conversion``.

As implied by the warning message, there are approaches you can use to
avoid the unwanted strict aliasing optimizations in a case like this.

One possibility is to simply avoid the use of higher levels of optimization,
but that is quite drastic, since it throws away a number of useful
optimizations that don't involve strict aliasing assumptions.

A less drastic approach is for you to compile the program using the
:switch:`-fno-strict-aliasing` switch. Actually, it is only the
unit containing the dereferencing of the suspicious pointer
that you need to compile with that switch. So, in this case, if you compile
unit ``M`` with this switch, you get the expected
value of ``0`` printed. Analyzing which units might need
the switch can be painful, so you may find it a more reasonable approach
is to compile the entire program with options :switch:`-O2`
and :switch:`-fno-strict-aliasing`. If you obtain satisfactory performance
with this combination of options, then the
advantage is that you have avoided the entire issue of possible problematic
optimizations due to strict aliasing.

To avoid the use of compiler switches, you may use the configuration
pragma ``No_Strict_Aliasing`` with no parameters
to specify that for all access types, the strict
aliasing optimizations should be suppressed.

However, these approaches are still overkill, in that they cause
all manipulations of all access values to be deoptimized. A more
refined approach is to concentrate attention on the specific
access type identified as problematic.

The first possibility is to move the instantiation of unchecked
conversion to the unit in which the type is declared. In this
example, you would move the instantiation of ``Unchecked_Conversion``
from the body of package ``P2`` to the spec of package ``P1``.
Now, the warning disappears because any use of the access type
knows there is a suspicious unchecked conversion and the strict
aliasing optimizations are automatically suppressed for it.

If it's not practical to move the unchecked conversion to the same unit
in which the destination access type is declared (perhaps because the
source type is not visible in that unit), the second possibiliy is for you to
use pragma ``No_Strict_Aliasing`` for the type. You must place this pragma
in the same declarative part as the declaration of the access type:

  .. code-block:: ada

     type A2 is access Int2;
     pragma No_Strict_Aliasing (A2);

Here again, the compiler now knows that strict aliasing optimizations
should be suppressed for any dereference made through type ``A2`` and
the expected behavior is obtained.

The third possibility is to declare that one of the designated types
involved, namely ``Int1`` or ``Int2``, is allowed to alias any other
type in the universe, by using pragma ``Universal_Aliasing``:

  .. code-block:: ada

     type Int2 is new Integer;
     pragma Universal_Aliasing (Int2);

The effect is equivalent to applying pragma ``No_Strict_Aliasing`` to
every access type designating ``Int2``, in particular ``A2``, and, more
generally, to every reference made to an object of declared type ``Int2``,
so it's very powerful and effectively takes ``Int2`` out of the alias
analysis performed by the compiler in all circumstances.

You can also use this pragma used to deal with aliasing issues that arise
from the use of ``Unchecked_Conversion`` in the source code but
without the presence of access types. The typical example is code
that streams data by means of arrays of storage units (bytes):

 .. code-block:: ada

    type Byte is mod 2**System.Storage_Unit;
    for Byte'Size use System.Storage_Unit;

    type Chunk_Of_Bytes is array (1 .. 64) of Byte;

    procedure Send (S : Chunk_Of_Bytes);

    type Rec is record
       ...
    end record;

    procedure Dump (R : Rec) is
       function To_Stream is
          new Ada.Unchecked_Conversion (Rec, Chunk_Of_Bytes);
    begin
       Send (To_Stream (R));
    end;

This generates the following warning for the call to ``Send``:

  ::

     dump.adb:8:25: warning: unchecked conversion implemented by copy
     dump.adb:8:25: warning: use pragma Universal_Aliasing on either type
     dump.adb:8:25: warning: to enable RM 13.9(12) implementation permission

This occurs because the formal parameter ``S`` of ``Send`` is passed by
reference by the compiler and it's not possible to pass a reference to
``R`` directly in the call without violating strict type-based aliasing.
That's why the compiler generates a temporary of type ``Chunk_Of_Bytes``
just before the call and passes a reference to this temporary instead.

As implied by the warning message, you can avoid the temporary
(and the warning) by means of pragma ``Universal_Aliasing``:

 .. code-block:: ada

    type Chunk_Of_Bytes is array (1 .. 64) of Byte;
    pragma Universal_Aliasing (Chunk_Of_Bytes);

You can also apply this pragma to the component type instead:

 .. code-block:: ada

    type Byte is mod 2**System.Storage_Unit;
    for Byte'Size use System.Storage_Unit;
    pragma Universal_Aliasing (Byte);

and every array type whose component is ``Byte`` will inherit the pragma.

To summarize, the alias analysis performed in strict aliasing mode by
the compiler can have significant benefits. We've seen cases of large
scale application code where the execution time is increased by up to
5% when these optimizations are turned off. However, if you have code
that make significant use of unchecked conversion, you might want to
just stick with :switch:`-O1` (with the GCC back end) and avoid the
entire issue. If you get adequate performance at this level of
optimization, that's probably the safest approach. If tests show that
you really need higher levels of optimization, then you can experiment
with :switch:`-O2` and :switch:`-O2 -fno-strict-aliasing` to see how
much effect this has on size and speed of the code. If you really need
to use :switch:`-O2` with strict aliasing in effect, then you should
review any uses of unchecked conversion, particularly if you are
getting the warnings described above.


.. _Aliased_Variables_and_Optimization:

Aliased Variables and Optimization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Aliasing

There are scenarios in which your programs may
use low level techniques to modify variables
that otherwise might be considered to be unassigned. For example,
you can pass a variable to a procedure by reference by taking
the address of the parameter and using that address to modify the
variable's value, even though the address is passed as an ``in`` parameter.
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

where Get_String is a C function that uses the address in ``Temp`` to
modify the variable ``Name``. This code is dubious, and arguably
erroneous, and the compiler is entitled to assume that
``Name`` is never modified, and generate code accordingly.

However, in practice, this could cause some existing code that
seems to work with no optimization to start failing at higher
levels of optimization.

What the compiler does for such cases, is to assume that marking a
variable as aliased indicates that some "funny business" may be going
on. The optimizer recognizes the ``aliased`` keyword and inhibits any
optimizations that assume the variable cannot be assigned to.  This
means that the above example will in fact "work" reliably, that is, it
will produce the expected results. However, you should nevertheless
avoid code such as this if possible because it's not portable and may not
functin as you expect with all compilers.


.. _Atomic_Variables_and_Optimization:

Atomic Variables and Optimization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Atomic

You need to take two things into consideration with regard to performance when
you use atomic variables.

First, the RM only guarantees that access to atomic variables
be atomic, but has nothing to say about how this is achieved,
though there is a strong implication that this should not be
achieved by explicit locking code. Indeed, GNAT never
generates any locking code for atomic variable access; it will
simply reject any attempt to make a variable or type atomic
if the atomic access cannot be achieved without such locking code.

That being said, it's important to understand that you cannot
assume the the program will always access the entire variable. Consider
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
variable with a single load instruction. It is perfectly legitimate, if
the hardware allows it, to do a byte read of just the ``B`` field. This read
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

Now the reference to ``RV`` must read the whole variable.
Actually, one can imagine some compiler which figures
out that the whole copy is not required (because only
the ``B`` field is actually accessed), but GNAT
certainly won't do that, and we don't know of any
compiler that would not handle this right, and the
above code will in practice work portably across
all architectures (that permit the Atomic declaration).

The second issue with atomic variables has to do with
the possible requirement of generating synchronization
code. For more details on this, consult the sections on
the pragmas Enable/Disable_Atomic_Synchronization in the
:title:``GNAT Reference Manual``. If performance is critical, and
such synchronization code is not required, you may find it
useful to disable it.


.. _Passive_Task_Optimization:

Passive Task Optimization
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Passive Task

A passive task is one which is sufficiently simple that,
in theory, a compiler could recognize it and implement it
efficiently without creating a new thread. The original design
of Ada 83 had in mind this kind of passive task optimization, but
only a few Ada 83 compilers attempted it. The reason was that
it was difficult to determine the exact conditions under which
the optimization was possible. The result is a very fragile
optimization where a very minor change in the program can
suddenly silently make a task non-optimizable.

With the revisiting of this issue in Ada 95, there was general
agreement that this approach was fundamentally flawed and the
notion of protected types was introduced. When using protected
types, the restrictions are well defined, you KNOW that the
operations will be optimized, and furthermore this optimized
performance is fully portable.

Although it would theoretically be possible for GNAT to attempt to
do this optimization, it really doesn't make sense in the
context of Ada 95 and none of the Ada 95 compilers implement
this optimization as far as we know. GNAT never
attempts to perform this optimization.

In any new Ada 95 code that you write, you should always
use protected types in place of tasks that might be able to
be optimized in this manner.
Of course, this does not help if you have legacy Ada 83 code
that depends on this optimization, but it is unusual to encounter
a case where the performance gains from this optimization
are significant.

Your program should work correctly without this optimization. If
you have performance problems, the most practical
approach is to figure out exactly where these performance problems
arise and update those particular tasks to be protected types. Note
that typically clients of the tasks who call entries will not have
to be modified, only the task definitions themselves.


.. _Text_IO_Suggestions:

``Text_IO`` Suggestions
-----------------------

.. index:: Text_IO and performance

The ``Ada.Text_IO`` package has fairly high overhead due in part to
the requirement of maintaining page and line counts. If performance
is critical, one recommendation is to use ``Stream_IO`` instead of
``Text_IO`` for large-volume output, since it has less overhead.

If you must use ``Text_IO``, note that output to the standard output and
standard error files is unbuffered by default (this provides
better behavior when output statements are used for debugging or if
the progress of a program is observed by tracking the output, e.g. by
using the Unix *tail -f* command to watch redirected output).

If you're generating large volumes of output with ``Text_IO`` and
performance is an important factor, use a designated file instead
of the standard output file or change the standard output file to
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

By default, an executable contains all code and data of its objects
(directly linked or coming from statically linked libraries), even data or code
never used by this executable. This feature eliminates such unused code from your
executable, thus making it smaller (in disk and in memory).

You can use this functionality on all Linux platforms except for the IA-64
architecture and on all cross platforms using the ELF binary file format.
In both cases, GNU binutils version 2.16 or later are required to enable it.

.. _Compilation_options:

Compilation options
^^^^^^^^^^^^^^^^^^^

The operation of eliminating the unused code and data from the final executable
is directly performed by the linker.

.. index:: -ffunction-sections (gcc)
.. index:: -fdata-sections (gcc)

In order to do this, it has to work with objects compiled with the
following switches passed to the GCC back end:
:switch:`-ffunction-sections` :switch:`-fdata-sections`.

These options are usable with C and Ada files.
They cause the compiler to place each
function or data in a separate section in the resulting object file.

Once you've created the objects and static libraries with these switches, the
linker can perform the dead code elimination. You can do this by specifying
the :switch:`-Wl,--gc-sections` switch to your ``gcc`` command or in the
:switch:`-largs` section of your invocation of ``gnatmake``. This causes
the linker to perform a
garbage collection and remove code and data that are never referenced.

If the linker performs a partial link (:switch:`-r` linker switch), then you
need to provide the entry point using the :switch:`-e` / :switch:`--entry`
linker switch.

Note that objects compiled without the :switch:`-ffunction-sections` and
:switch:`-fdata-sections` options can still be linked with the executable.
However, no dead code elimination can be performed on those objects (they will
be linked as is).

The GNAT static library is compiled with :switch:`-ffunction-sections`
and :switch:`-fdata-sections` on some platforms. This allows you to
eliminate the unused code and data of the GNAT library from your
executable.


.. _Example_of_unused_subprogram/data_elimination:

Example of unused subprogram/data elimination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here's a simple example:

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
excerpt and hence may be safely removed from the final executable.

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

You can see that the procedure ``Unused`` and the object
``Unused_Data`` are removed by the linker when you've used the
appropriate switches.


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

If ``A`` has the value ``Integer'Last``, the addition will cause
overflow since the result is out of range of the type ``Integer``.
In this case, execution will raise ``Constraint_Error`` if checks are
enabled.

A trickier situation arises in cases like the following:

  .. code-block:: ada

     A, C : Integer;
     ...
     A := (A + 1) + C;

where ``A`` is ``Integer'Last`` and ``C`` is ``-1``.
Here, the final result of the expression on the right hand side is
``Integer'Last`` which is in range, but the question arises whether the
intermediate addition of ``(A + 1)`` raises an overflow error.

The (perhaps surprising) answer is that the Ada language
definition does not answer this question. Instead, it leaves
it up to the implementation to do one of two things if overflow
checks are enabled.

* raise an exception (``Constraint_Error``), or

* yield the correct mathematical result which is then used in
  subsequent operations.

If the compiler chooses the first approach, the execution of this
example will indeed raise ``Constraint_Error`` if overflow checking is
enabled or result in erroneous execution if overflow checks are suppressed.

But if the compiler
chooses the second approach, it can perform both additions yielding
the correct mathematical result, which is in range, so no exception
is raised and the right result is obtained, regardless of whether
overflow checks are suppressed.

Note that in the first example, an
exception will be raised in either case, since if the compiler
gives the correct mathematical result for the addition, it will
be out of range of the target type of the assignment and thus
fails the range check.

This lack of specified behavior in the handling of overflow for
intermediate results is a source of non-portability and can thus
be problematic when you port programs. Most typically, this arises
in a situation where the original compiler did not raise an exception
and you move the application to a compiler where the check is
performed on the intermediate result and an unexpected exception is
raised.

Furthermore, when using Ada 2012's preconditions and other
assertion forms, another issue arises. Consider:

  .. code-block:: ada

       procedure P (A, B : Integer) with
         Pre => A + B <= Integer'Last;

We often want to regard arithmetic in a context such as this from a
purely mathematical point of view. So, for example, if the two actual
parameters for a call to ``P`` are both ``Integer'Last`` then the
precondition should be evaluated as ``False``. If we're executing in a
mode with run-time checks enabled for preconditions, then we would
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
is ``True``, but at run time we may (but are not guaranteed to) get an
exception raised because of the intermediate overflow (and we really
would prefer this precondition to be considered ``True`` at run time).


.. _Management_of_Overflows_in_GNAT:

Management of Overflows in GNAT
-------------------------------

To deal with the portability issue and with the problem of
mathematical versus run-time interpretation of the expressions in
assertions, GNAT provides comprehensive control over the handling
of intermediate overflows. It can operate in three modes, and
in addition, permits separate selection of operating modes for
the expressions within assertions (here the term 'assertions'
is used in the technical sense, which includes preconditions and so forth)
and for expressions appearing outside assertions.

The three modes are:

* *Use base type for intermediate operations* (``STRICT``)

  In this mode, all intermediate results for predefined arithmetic
  operators are computed using the base type, and the result must
  be in range of the base type. If this is not the
  case, then either an exception is raised (if overflow checks are
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
  enough. Consider the following example:

    .. code-block:: ada

           procedure R (A, B, C, D : Integer) with
             Pre => (A**2 * B**2) / (C**2 * D**2) <= 10;

  where ``A`` = ``B`` = ``C`` = ``D`` = ``Integer'Last``.
  Now the intermediate results are
  out of the range of ``Long_Long_Integer`` even though the final result
  is in range and the precondition is ``True`` from a mathematical point
  of view. In such a case, operating in this mode, an overflow occurs
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
  of evaluating the precondition would be ``True``.

  This mode has the advantage of avoiding any intermediate
  overflows, but at the expense of significant run-time overhead,
  including the use of a library (included automatically in this
  mode) for multiple-precision arithmetic.

  This mode provides cleaner semantics for assertions, since now
  the run-time behavior emulates true arithmetic behavior for the
  predefined arithmetic operators, meaning that there is never a
  conflict between the mathematical view of the assertion and its
  run-time behavior.

  Note that in this mode, the behavior is unaffected by whether or
  not overflow checks are suppressed, since overflow does not occur.
  Gigantic intermediate expressions can still raise
  ``Storage_Error`` as a result of attempting to compute the
  results of such expressions (e.g. ``Integer'Last ** Integer'Last``)
  but overflow is impossible.


Note that these modes apply only to the evaluation of predefined
arithmetic, membership, and comparison operators for signed integer
arithmetic.

For fixed-point arithmetic, you suppress checks. But if checks
are enabled,
fixed-point values are always checked for overflow against the
base type for intermediate expressions (i.e., such checks always
operate in the equivalent of ``STRICT`` mode).

For floating-point, on nearly all architectures, ``Machine_Overflows``
is ``False``, and IEEE infinities are generated, so overflow exceptions
are never raised. If you want to avoid infinities and check that
final results of expressions are in range, you can declare a
constrained floating-point type and range checks are carried
out in the normal manner (with infinite values always failing all
range checks).


.. _Specifying_the_Desired_Mode:

Specifying the Desired Mode
---------------------------

.. index:: pragma Overflow_Mode

You can specify
the desired mode of for handling intermediate overflow using
either the ``Overflow_Mode`` pragma or an equivalent compiler switch.
The pragma has the form:

  .. code-block:: ada

      pragma Overflow_Mode ([General =>] MODE [, [Assertions =>] MODE]);

where ``MODE`` is one of

* ``STRICT``:  intermediate overflows checked (using base type)
* ``MINIMIZED``: minimize intermediate overflows
* ``ELIMINATED``: eliminate intermediate overflows

The case is ignored, so ``MINIMIZED``, ``Minimized`` and
``minimized`` all have the same effect.

If you only specify the ``General`` parameter, the given ``MODE`` applies
to expressions both within and outside assertions. If you specify both arguments,
the value of ``General`` applies to expressions outside assertions,
and ``Assertions`` applies to expressions within assertions. For example:

  .. code-block:: ada

     pragma Overflow_Mode
       (General => Minimized, Assertions => Eliminated);

specifies that expressions outside assertions be evaluated
in 'minimize intermediate overflows' mode and expressions within
assertions be evaluated in 'eliminate intermediate overflows' mode.
This is often a reasonable choice, avoiding excessive overhead
outside assertions, but assuring a high degree of portability
when importing code from another compiler while incurring
the extra overhead for assertion expressions to ensure that
the behavior at run time matches the expected mathematical
behavior.

The ``Overflow_Mode`` pragma has the same scoping and placement
rules as pragma ``Suppress``, so you can use it either as a
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

Additionally, you can use the compiler switch :switch:`-gnato?` or
:switch:`-gnato??` to control the checking mode default (which you can
subsequently override using the above pragmas).

Here ``?`` is one of the digits ``1`` through ``3``:

  ====== ======================================================
  ``1``  use base type for intermediate operations (``STRICT``)
  ``2``  minimize intermediate overflows (``MINIMIZED``)
  ``3``  eliminate intermediate overflows (``ELIMINATED``)
  ====== ======================================================

As with the pragma, if only one digit appears, it applies to all
cases; if two digits are given, the first applies to expressions outside
assertions and the second within assertions. Thus the equivalent
of the example pragma above would be
:switch:`-gnato23`.

If you don't provide any digits following the :switch:`-gnato`, it's
equivalent to :switch:`-gnato11`, causing all intermediate operations
to be computed using the base type (``STRICT`` mode).


.. _Default_Settings:

Default Settings
----------------

The default mode for overflow checks is

  ::

      General => Strict

which causes all computations both inside and outside assertions to use the
base type, and is equivalent to :switch:`-gnato` (with no digits following).

The pragma ``Suppress (Overflow_Check)`` disables overflow
checking but has no effect on the method used for computing
intermediate results.
The pragma ``Unsuppress (Overflow_Check)`` enables overflow
checking but has no effect on the method used for computing
intermediate results.


.. _Implementation_Notes:

Implementation Notes
--------------------

In practice, on typical 64-bit machines, the ``MINIMIZED`` mode is
reasonably efficient and you can generally use it. It also helps
to ensure compatibility with code imported from other
compilers to GNAT.

Setting all intermediate overflows checking (``STRICT`` mode)
makes sense if you want to
make sure your code is compatible with any other
Ada implementations. You may find this useful in ensuring portability
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
allocation, but it does use the secondary stack, so an
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
platforms for which ``Long_Long_Integer`` is at least 64-bits (nearly all GNAT
platforms).



.. _Performing_Dimensionality_Analysis_in_GNAT:

Performing Dimensionality Analysis in GNAT
==========================================

.. index:: Dimensionality analysis

The GNAT compiler supports dimensionality checking. You can
specify physical units for objects and the compiler verifies that uses
of these objects are compatible with their dimension, in a fashion that is
familiar to engineering practice. The dimensions of algebraic expressions
(including powers with static exponents) are computed from their constituents.

.. index:: Dimension_System aspect
.. index:: Dimension aspect

This feature depends on Ada 2012 aspect specifications and is available for
versions 7.0.1 and later of GNAT.
The GNAT-specific aspect ``Dimension_System``
allows you to define a system of units; the aspect ``Dimension``
allows you to declare dimensioned quantities within a given system.
(These aspects are described in the *Implementation Defined Aspects*
chapter of the :title:`GNAT Reference Manual`).

The major advantage of this model is that it does not require the declaration of
multiple operators for all possible combinations of types: you is only need
to use the proper subtypes in object declarations.

.. index:: System.Dim.Mks package (GNAT library)
.. index:: MKS_Type type

The simplest way to impose dimensionality checking on a computation is to make
use of one of the instantiations of the package ``System.Dim.Generic_Mks``, which
is part of the GNAT library. This generic package defines a floating-point
type ``MKS_Type``, for which a sequence of dimension names are specified,
together with their conventional abbreviations.  You should read the following
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
the aspect that specifies its dimensions within the MKS system as well as the
string to be used for output of a value of that unit:

  .. code-block:: ada

     subtype Acceleration is Mks_Type
       with Dimension => ("m/sec^2",
                          Meter => 1,
                          Second => -2,
                          others => 0);

Here's a complete example:

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

The dimensions of an expression are properly displayed even if there is
no explicit subtype for it. If we add to the program:

  .. code-block:: ada

        Put ("Final velocity: ");
        Put (G * T, Aft =>2, Exp =>0);
        Put_Line ("");

the output includes:

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

* :samp:`DV({expr1 op expr2})`, where *op* is "+" or "-", is :samp:`DV({expr1})`
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
for "+" and "-" except when comparing to a literal; thus

  .. code-block:: ada

        acc > len

is equivalent to

  .. code-block:: ada

       acc-len > 0.0

and is thus illegal, but

  .. code-block:: ada

        acc > 10.0

is accepted with a warning. Analogously, a conditional expression requires the
same dimension vector for each branch (with no exception for literals).

The dimension vector of a type conversion :samp:`T({expr})` is defined
as follows, based on the nature of ``T``:

* If ``T`` is a dimensioned subtype, then :samp:`DV(T({expr}))` is ``DV(T)``
  provided that either *expr* is dimensionless or
  :samp:`DV(T)` = :samp:`DV({expr})`. The conversion is illegal
  if *expr* is dimensioned and :samp:`DV({expr})` /= ``DV(T)``.
  Note that vector equality does not require that the corresponding
  Unit_Names be the same.

  As a consequence of the above rule, you can convert between
  different dimension systems that follow the same international
  system of units, with the seven physical components given in the
  standard order (length, mass, time, etc.). Thus, you can convert a
  length in meters to a length in inches (with a suitable conversion
  factor) but not, for example, to a mass in pounds.

* If ``T`` is the base type for *expr* (and the dimensionless root type of
  the dimension system), then :samp:`DV(T({expr}))` is ``DV(expr)``.
  Thus, if *expr* is of a dimensioned subtype of ``T``, the conversion may
  be regarded as a "view conversion" that preserves dimensionality.

  This rule means you can write generic code that can be instantiated
  with compatible dimensioned subtypes.  You include in the generic unit
  conversions that will consequently be present in instantiations, but
  conversions to the base type will preserve dimensionality and make it
  possible to write generic code that is correct with respect to
  dimensionality.

* Otherwise (i.e., ``T`` is neither a dimensioned subtype nor a dimensionable
  base type), :samp:`DV(T({expr}))` is the empty vector. Thus, a dimensioned
  value can be explicitly converted to a non-dimensioned subtype, which
  of course then escapes dimensionality analysis.

The dimension vector for a type qualification :samp:`T'({expr})` is the same
as for the type conversion :samp:`T({expr})`.

An assignment statement

   .. code-block:: ada

         Source := Target;

requires ``DV(Source)`` = ``DV(Target)`` and analogously for parameter
passing (the dimension vector for the actual parameter must be equal to the
dimension vector for the formal parameter).

When using dimensioned types with elementary functions, you need not
instantiate the ``Ada.Numerics.Generic_Elementary_Functions`` package using
the ``Mks_Type`` nor for any of the derived subtypes such as ``Distance``.
For functions such as ``Sqrt``, the dimensional analysis will fail when using
the subtypes because both the parameter and return are of the same type.

An example instantiation

  .. code-block:: ada

        package Mks_Numerics is new
           Ada.Numerics.Generic_Elementary_Functions (System.Dim.Mks.Mks_Type);

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
some other task exceeds the available stack space, unpredictable
behavior will occur. Most native systems offer some level of protection by
adding a guard page at the end of each task stack. This mechanism is usually
not enough for dealing properly with stack overflow situations because
a large local variable could "jump" above the guard page.
Furthermore, when the
guard page is hit, there may not be any space left on the stack for executing
the exception propagation code. Enabling stack checking avoids
such situations.

To activate stack checking, compile all units with the ``gcc`` switch
:switch:`-fstack-check`. For example:

  ::

     $ gcc -c -fstack-check package1.adb

Units compiled with this option will generate extra instructions to check
that any use of the stack (for procedure calls or for declaring local
variables in declare blocks) does not exceed the available stack space.
If the space is exceeded, a ``Storage_Error`` exception is raised.

For declared tasks, the default stack size is defined by the GNAT runtime,
whose size may be modified at bind time through the ``-d`` bind switch
(:ref:`Switches_for_gnatbind`). You can set task specific stack sizes using the
``Storage_Size`` pragma.

For the environment task, the stack size is determined by the operating system.
Consequently, to modify the size of the environment task please refer to your
operating system documentation.

When using the LLVM back end, this switch doesn't perform full stack overflow
checking, but just checks for very large local dynamic allocations.

.. _Static_Stack_Usage_Analysis:

Static Stack Usage Analysis
---------------------------

.. index:: Static Stack Usage Analysis

.. index:: -fstack-usage

A unit compiled with the :switch:`-fstack-usage` switch generate an extra file
that specifies
the maximum amount of stack used on a per-function basis.
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

Compilation of a unit with the :switch:`-Wstack-usage` switch will
issue a warning for each subprogram whose stack usage might be larger
than the specified amount of bytes.  The wording of that warning is
consistent with that in the file documented above.

This is not supported by the LLVM back end.


.. _Dynamic_Stack_Usage_Analysis:

Dynamic Stack Usage Analysis
----------------------------

You can measure the maximum amount of stack used by a task by
adding a switch to ``gnatbind``, as:

  ::

      $ gnatbind -u0 file

With this option, at each task termination, its stack usage is output on
:file:`stderr`.
Note that this switch is not compatible with tools like
Valgrind and DrMemory; they will report errors.

It is not always convenient to output the stack usage when the program
is still running. Hence, you can delay this output until the
termination of the number of tasks specified as the argument of the
:switch:`-u` switch. For example:

  ::

     $ gnatbind -u100 file

buffers the stack usage information of the first 100 tasks to terminate and
outputs it when the program terminates. Results are displayed in four
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

By default, ``gnatbind`` does not process the environment task stack,
the stack that contains the main unit. To enable processing of the
environment task stack, set the environment variable GNAT_STACK_LIMIT
to the maximum size of the environment task stack. This amount is
given in kilobytes. For example:

  ::

     $ set GNAT_STACK_LIMIT 1600

would specify to the analyzer that the environment task stack has a limit
of 1.6 megabytes. Any stack usage beyond this will be ignored by the analysis.

This is not suppored by the LLVM back end.

The package ``GNAT.Task_Stack_Usage`` provides facilities to get
stack-usage reports at run time. See its body for the details.



.. _Memory_Management_Issues:

Memory Management Issues
========================

This section describes some useful memory pools provided in the GNAT library,
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

The ``System.Pool_Global`` package provides the ``Unbounded_No_Reclaim_Pool``
storage pool. Allocations use the standard system call ``malloc`` while
deallocations use the standard system call ``free``. No reclamation is
performed when the pool goes out of scope. For performance reasons, the
standard default Ada allocators/deallocators do not use any explicit storage
pools but if they did, they could use this storage pool without any change in
behavior. That is why this storage pool is used  when the user
makes the default implicit allocator explicit as in this example:

  .. code-block:: ada

       type T1 is access Something;
        -- no Storage pool is defined for T2

       type T2 is access Something_Else;
       for T2'Storage_Pool use T1'Storage_Pool;
       -- the above is equivalent to
       for T2'Storage_Pool use System.Pool_Global.Global_Pool_Object;

The ``System.Pool_Local`` package provides the ``Unbounded_Reclaim_Pool`` storage
pool. Its allocation strategy is similar to ``Pool_Local``
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
           for I in 1 .. 50 loop
              v := new Integer;
           end loop;
        end Internal;
     begin
        for I in 1 .. 100 loop
           Internal;
        end loop;
     end Pooloc1;

The ``System.Pool_Size`` package implements the ``Stack_Bounded_Pool`` used when
``Storage_Size`` is specified for an access type.
The whole storage for the pool is
allocated at once, usually on the stack at the point where the access type is
elaborated. It is automatically reclaimed when exiting the scope where the
access type is defined. This package is not intended to be used directly by the
user; it is implicitly used for each declaration with a specified
``Storage_Size``:

  .. code-block:: ada

     type T1 is access Something;
     for T1'Storage_Size use 10_000;


.. _The_GNAT_Debug_Pool_Facility:

The GNAT Debug Pool Facility
----------------------------

.. index:: Debug Pool
.. index:: storage, pool, memory corruption

Using unchecked deallocation and unchecked conversion can easily
lead to incorrect memory references. The problems generated by such
references are usually difficult to find because the symptoms can be
very remote from the origin of the problem. In such cases, it is
very helpful to detect the problem as early as possible. This is the
purpose of the Storage Pool provided by ``GNAT.Debug_Pools``.

In order to use the GNAT specific debugging pool, you must
associate a debug pool object with each of the access types that may be
related to suspected memory problems. See Ada Reference Manual 13.11.

  .. code-block:: ada

     type Ptr is access Some_Type;
     Pool : GNAT.Debug_Pools.Debug_Pool;
     for Ptr'Storage_Pool use Pool;

``GNAT.Debug_Pools`` is derived from a GNAT-specific kind of
pool: the ``Checked_Pool``. Such pools, like standard Ada storage pools,
allow you to redefine allocation and deallocation strategies. They
also provide a checkpoint for each dereference through the use of
the primitive operation ``Dereference`` which is implicitly called at
each dereference of an access value.

Once you have associated an access type with a debug pool, operations on
values of the type may raise four distinct exceptions,
which correspond to four potential kinds of memory corruption:

* ``GNAT.Debug_Pools.Accessing_Not_Allocated_Storage``
* ``GNAT.Debug_Pools.Accessing_Deallocated_Storage``
* ``GNAT.Debug_Pools.Freeing_Not_Allocated_Storage``
* ``GNAT.Debug_Pools.Freeing_Deallocated_Storage``

For types associated with a Debug_Pool, dynamic allocation is performed using
the standard GNAT allocation routine. References to all allocated chunks of
memory are kept in an internal dictionary. Several deallocation strategies are
provided, allowing you to choose to release the memory to the system,
keep it allocated for further invalid access checks, or fill it with an easily
recognizable pattern for debug sessions. The memory pattern is the old IBM
hexadecimal convention: ``16#DEADBEEF#``.

See the documentation in the file :file:`g-debpoo.ads` for more
information on the various strategies.

Upon each dereference, a check is made that the access value denotes a
properly allocated memory location. Here's a complete example of use of
``Debug_Pools``, which includes typical instances of  memory corruption:

  .. code-block:: ada

      with GNAT.IO; use GNAT.IO;
      with Ada.Unchecked_Deallocation;
      with Ada.Unchecked_Conversion;
      with GNAT.Debug_Pools;
      with System.Storage_Elements;
      with Ada.Exceptions; use Ada.Exceptions;
      procedure Debug_Pool_Test is

         type T is access Integer;
         type U is access all T;

         P : GNAT.Debug_Pools.Debug_Pool;
         for T'Storage_Pool use P;

         procedure Free is new Ada.Unchecked_Deallocation (Integer, T);
         function UC is new Ada.Unchecked_Conversion (U, T);
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
  It is designed to work for fixed-position executables that use
  a static runtime library and, in this context, provides three
  types of information:

  * General information concerning memory management, such as the total
    number of allocations and deallocations, the amount of allocated
    memory and the high water mark, i.e., the largest amount of allocated
    memory in the course of program execution.

  * Backtraces for all incorrect deallocations, which are deallocations
    that do not correspond to a valid allocation.

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

       $ gnatmem [ switches ] [ DEPTH ] user_program

  You must link your program with the instrumented version of the
  allocation and deallocation routines. You do this by linking with the
  :file:`libgmem.a` library. For correct symbolic backtrace information,
  you should also compile your program with debugging options
  (see :ref:`Switches_for_gcc`) and be linked at a fixed position (with
  :switch:`-no-pie`). For example to build :file:`my_program` with
  ``gnatmake``:

    ::

       $ gnatmake my_program -g -largs -lgmem -no-pie

  Because library :file:`libgmem.a` contains an alternate body for package
  ``System.Memory``, you should not compile and link :file:`s-memory.adb`
  when you link an executable with library :file:`libgmem.a`. In that case,
  we don't recommended specifying switch :switch:`-a` to ``gnatmake``.

  When :file:`my_program` is executed, the file :file:`gmem.out` is produced.
  This file contains information about all allocations and deallocations
  performed by the program. It is produced by the instrumented allocations and
  deallocations routines and will be used by ``gnatmem``.

  To produce symbolic backtrace information for allocations and
  deallocations performed by the GNAT run-time library, you need to use a
  version of that library that has been compiled with the :switch:`-g` switch
  (see :ref:`Rebuilding_the_GNAT_Run-Time_Library`).

  You must supply ``gnatmem``  with the :file:`gmem.out` file and the executable to
  examine. If the location of :file:`gmem.out` file was not explicitly supplied by
  :switch:`-i` switch, ``gnatmem`` assumes that this file can be found in the
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
  Ada construct ``new`` was executed 45 times and only 6 calls to an
  ``Unchecked_Deallocation`` routine occurred.

  Subsequent paragraphs display information on all allocation roots.
  An *allocation root* is a specific point in the execution of the program
  that generates some dynamic allocation, such as a ``new``
  construct. This root is represented by an execution backtrace (or subprogram
  call stack). By default, the backtrace depth for allocations roots is 1, so
  that a root corresponds exactly to a source location. The backtrace can
  be made deeper, to make the root more specific.

  .. _Switches_for_gnatmem:

  Switches for ``gnatmem``
  ^^^^^^^^^^^^^^^^^^^^^^^^

  ``gnatmem`` recognizes the following switches:

  .. index:: -q (gnatmem)

  :switch:`-q`
    Quiet. Gives the minimum output needed to identify the origin of the
    memory leaks. Omits statistical information.


  .. index:: DEPTH switch (gnatmem)

  :switch:`{DEPTH}`
    ``DEPTH`` is an integer literal (usually between 1 and 10) which controls
    the depth of the backtraces defining allocation root. The default value for
    DEPTH is 1. The deeper the backtrace, the more precise the localization of
    the root. Note that the total number of roots can depend on this
    parameter; in other words there may be more roots when the requested
    backtrace depth is higher. You must specify this parameter *before* the
    name of the executable to be analyzed, to avoid ambiguity.


  .. index:: -b (gnatmem)

  :switch:`-b {N}`
    This switch has the same effect as just a depth parameter ``N``.


  .. index:: -i (gnatmem)

  :switch:`-i {file}`
    Do the ``gnatmem`` processing starting from :file:`file`, rather than
    :file:`gmem.out` in the current directory.


  .. index:: -m (gnatmem)

  :switch:`-m {n}`
    This switch causes ``gnatmem`` to mask the allocation roots that have less
    than ``n`` leaks.  The default value is 1. Specifying the value of 0 will allow
    examination of even the roots that did not result in leaks.


  .. index:: -s (gnatmem)

  :switch:`-s {order}`
    This switch causes ``gnatmem`` to sort the allocation roots
    according to the specified sort criteria, each identified by a
    single letter. The currently supported criteria are ``n``, ``h``,
    and ``w`` representing, respectively, the number of unfreed
    allocations, the high watermark, and the final watermark corresponding to
    a specific root. The default order is ``nwh``.


  .. index:: -t (gnatmem)

  :switch:`-t`
    This switch causes memory allocated size to be always output in bytes.
    The default ``gnatmem`` behavior is to show memory sizes less then 1 kilobyte
    in bytes, from 1 kilobyte till 1 megabyte in kilobytes and the rest in
    megabytes.


  .. _Example_of_gnatmem_Usage:

  Example of ``gnatmem`` Usage
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  The following example shows the use of ``gnatmem``
  on a simple memory-leaking program.
  Suppose that we have the following Ada program:

    .. code-block:: ada

       with Ada.Unchecked_Deallocation;
       procedure Test_Gm is

          type T is array (1..1000) of Integer;
          type Ptr is access T;
          procedure Free is new Ada.Unchecked_Deallocation (T, Ptr);
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

  We execute the program as usual:

    ::

       $ test_gm

  ``gnatmem`` is invoked simply with

    ::

       $ gnatmem test_gm

  which produces the following output (the details may vary on
  different platforms):

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


  Note that the GNAT runtime itself contains a certain number of
  allocations that have no corresponding deallocations,
  as shown here for root #2 and root #3.
  This is a normal behavior when the number of non-freed allocations
  is one: it allocates dynamic data structures that the run time needs for
  the complete lifetime of the program. Note also that there is only one
  allocation root in the user program, with a single line back trace:
  ``test_gm.adb:11 test_gm.my_alloc``, whereas a careful analysis of the
  program shows that ``My_Alloc`` is called at 2 different points in the
  source (line 21 and line 24). If those two allocation roots need to be
  distinguished, you can use the backtrace depth parameter:

    ::

       $ gnatmem 3 test_gm

  which produces the following output:


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
