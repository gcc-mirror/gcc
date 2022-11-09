..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: initialization routines, termination routines, constructors, output of, destructors, output of

.. _initialization:

How Initialization Functions Are Handled
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The compiled code for certain languages includes :dfn:`constructors`
(also called :dfn:`initialization routines`)---functions to initialize
data in the program when the program is started.  These functions need
to be called before the program is 'started'---that is to say, before
``main`` is called.

Compiling some languages generates :dfn:`destructors` (also called
:dfn:`termination routines`) that should be called when the program
terminates.

To make the initialization and termination functions work, the compiler
must output something in the assembler code to cause those functions to
be called at the appropriate time.  When you port the compiler to a new
system, you need to specify how to do this.

There are two major ways that GCC currently supports the execution of
initialization and termination functions.  Each way has two variants.
Much of the structure is common to all four variations.

.. index:: __CTOR_LIST__, __DTOR_LIST__

The linker must build two lists of these functions---a list of
initialization functions, called ``__CTOR_LIST__``, and a list of
termination functions, called ``__DTOR_LIST__``.

Each list always begins with an ignored function pointer (which may hold
0, -1, or a count of the function pointers after it, depending on
the environment).  This is followed by a series of zero or more function
pointers to constructors (or destructors), followed by a function
pointer containing zero.

Depending on the operating system and its executable file format, either
:samp:`crtstuff.c` or :samp:`libgcc2.c` traverses these lists at startup
time and exit time.  Constructors are called in reverse order of the
list; destructors in forward order.

The best way to handle static constructors works only for object file
formats which provide arbitrarily-named sections.  A section is set
aside for a list of constructors, and another for a list of destructors.
Traditionally these are called :samp:`.ctors` and :samp:`.dtors`.  Each
object file that defines an initialization function also puts a word in
the constructor section to point to that function.  The linker
accumulates all these words into one contiguous :samp:`.ctors` section.
Termination functions are handled similarly.

This method will be chosen as the default by :samp:`target-def.h` if
``TARGET_ASM_NAMED_SECTION`` is defined.  A target that does not
support arbitrary sections, but does support special designated
constructor and destructor sections may define ``CTORS_SECTION_ASM_OP``
and ``DTORS_SECTION_ASM_OP`` to achieve the same effect.

When arbitrary sections are available, there are two variants, depending
upon how the code in :samp:`crtstuff.c` is called.  On systems that
support a :dfn:`.init` section which is executed at program startup,
parts of :samp:`crtstuff.c` are compiled into that section.  The
program is linked by the :command:`gcc` driver like this:

.. code-block:: c++

  ld -o output_file crti.o crtbegin.o ... -lgcc crtend.o crtn.o

The prologue of a function (``__init``) appears in the ``.init``
section of :samp:`crti.o`; the epilogue appears in :samp:`crtn.o`.  Likewise
for the function ``__fini`` in the :dfn:`.fini` section.  Normally these
files are provided by the operating system or by the GNU C library, but
are provided by GCC for a few targets.

The objects :samp:`crtbegin.o` and :samp:`crtend.o` are (for most targets)
compiled from :samp:`crtstuff.c`.  They contain, among other things, code
fragments within the ``.init`` and ``.fini`` sections that branch
to routines in the ``.text`` section.  The linker will pull all parts
of a section together, which results in a complete ``__init`` function
that invokes the routines we need at startup.

To use this variant, you must define the ``INIT_SECTION_ASM_OP``
macro properly.

If no init section is available, when GCC compiles any function called
``main`` (or more accurately, any function designated as a program
entry point by the language front end calling ``expand_main_function``),
it inserts a procedure call to ``__main`` as the first executable code
after the function prologue.  The ``__main`` function is defined
in :samp:`libgcc2.c` and runs the global constructors.

In file formats that don't support arbitrary sections, there are again
two variants.  In the simplest variant, the GNU linker (GNU ``ld``)
and an 'a.out' format must be used.  In this case,
``TARGET_ASM_CONSTRUCTOR`` is defined to produce a ``.stabs``
entry of type :samp:`N_SETT`, referencing the name ``__CTOR_LIST__``,
and with the address of the void function containing the initialization
code as its value.  The GNU linker recognizes this as a request to add
the value to a :dfn:`set`; the values are accumulated, and are eventually
placed in the executable as a vector in the format described above, with
a leading (ignored) count and a trailing zero element.
``TARGET_ASM_DESTRUCTOR`` is handled similarly.  Since no init
section is available, the absence of ``INIT_SECTION_ASM_OP`` causes
the compilation of ``main`` to call ``__main`` as above, starting
the initialization process.

The last variant uses neither arbitrary sections nor the GNU linker.
This is preferable when you want to do dynamic linking and when using
file formats which the GNU linker does not support, such as 'ECOFF'.  In
this case, ``TARGET_HAVE_CTORS_DTORS`` is false, initialization and
termination functions are recognized simply by their names.  This requires
an extra program in the linkage step, called :command:`collect2`.  This program
pretends to be the linker, for use with GCC; it does its job by running
the ordinary linker, but also arranges to include the vectors of
initialization and termination functions.  These functions are called
via ``__main`` as described above.  In order to use this method,
``use_collect2`` must be defined in the target in :samp:`config.gcc`.
