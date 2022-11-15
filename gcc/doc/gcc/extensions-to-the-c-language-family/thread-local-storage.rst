..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Thread-Local Storage, TLS, __thread

.. _thread-local:

Thread-Local Storage
********************

Thread-local storage (TLS) is a mechanism by which variables
are allocated such that there is one instance of the variable per extant
thread.  The runtime model GCC uses to implement this originates
in the IA-64 processor-specific ABI, but has since been migrated
to other processors as well.  It requires significant support from
the linker (:command:`ld`), dynamic linker (:command:`ld.so`), and
system libraries (:samp:`libc.so` and :samp:`libpthread.so`), so it
is not available everywhere.

At the user level, the extension is visible with a new storage
class keyword: ``__thread``.  For example:

.. code-block:: c++

  __thread int i;
  extern __thread struct state s;
  static __thread char *p;

The ``__thread`` specifier may be used alone, with the ``extern``
or ``static`` specifiers, but with no other storage class specifier.
When used with ``extern`` or ``static``, ``__thread`` must appear
immediately after the other storage class specifier.

The ``__thread`` specifier may be applied to any global, file-scoped
static, function-scoped static, or static data member of a class.  It may
not be applied to block-scoped automatic or non-static data member.

When the address-of operator is applied to a thread-local variable, it is
evaluated at run time and returns the address of the current thread's
instance of that variable.  An address so obtained may be used by any
thread.  When a thread terminates, any pointers to thread-local variables
in that thread become invalid.

No static initialization may refer to the address of a thread-local variable.

In C++, if an initializer is present for a thread-local variable, it must
be a :samp:`{constant-expression}`, as defined in 5.19.2 of the ANSI/ISO C++
standard.

See `ELF Handling For Thread-Local Storage <https://www.akkadia.org/drepper/tls.pdf>`_ for a detailed explanation of
the four thread-local storage addressing models, and how the runtime
is expected to function.

.. toctree::
  :maxdepth: 2


.. _c99-thread-local-edits:

ISO/IEC 9899:1999 Edits for Thread-Local Storage
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following are a set of changes to ISO/IEC 9899:1999 (aka C99)
that document the exact semantics of the language extension.

* 5.1.2  Execution environments

  Add new text after paragraph 1

  Within either execution environment, a :dfn:`thread` is a flow of
  control within a program.  It is implementation defined whether
  or not there may be more than one thread associated with a program.
  It is implementation defined how threads beyond the first are
  created, the name and type of the function called at thread
  startup, and how threads may be terminated.  However, objects
  with thread storage duration shall be initialized before thread
  startup.

* 6.2.4  Storage durations of objects

  Add new text before paragraph 3

  An object whose identifier is declared with the storage-class
  specifier ``__thread`` has :dfn:`thread storage duration`.
  Its lifetime is the entire execution of the thread, and its
  stored value is initialized only once, prior to thread startup.

* 6.4.1  Keywords

  Add ``__thread``.

* 6.7.1  Storage-class specifiers

  Add ``__thread`` to the list of storage class specifiers in
  paragraph 1.

  Change paragraph 2 to

  With the exception of ``__thread``, at most one storage-class
  specifier may be given [...].  The ``__thread`` specifier may
  be used alone, or immediately following ``extern`` or
  ``static``.

  Add new text after paragraph 6

  The declaration of an identifier for a variable that has
  block scope that specifies ``__thread`` shall also
  specify either ``extern`` or ``static``.

  The ``__thread`` specifier shall be used only with
  variables.

.. _c++98-thread-local-edits:

ISO/IEC 14882:1998 Edits for Thread-Local Storage
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following are a set of changes to ISO/IEC 14882:1998 (aka C++98)
that document the exact semantics of the language extension.

* [intro.execution]

  New text after paragraph 4

  A :dfn:`thread` is a flow of control within the abstract machine.
  It is implementation defined whether or not there may be more than
  one thread.

  New text after paragraph 7

  It is unspecified whether additional action must be taken to
  ensure when and whether side effects are visible to other threads.

* [lex.key]

  Add ``__thread``.

* [basic.start.main]

  Add after paragraph 5

  The thread that begins execution at the ``main`` function is called
  the :dfn:`main thread`.  It is implementation defined how functions
  beginning threads other than the main thread are designated or typed.
  A function so designated, as well as the ``main`` function, is called
  a :dfn:`thread startup function`.  It is implementation defined what
  happens if a thread startup function returns.  It is implementation
  defined what happens to other threads when any thread calls ``exit``.

* [basic.start.init]

  Add after paragraph 4

  The storage for an object of thread storage duration shall be
  statically initialized before the first statement of the thread startup
  function.  An object of thread storage duration shall not require
  dynamic initialization.

* [basic.start.term]

  Add after paragraph 3

  The type of an object with thread storage duration shall not have a
  non-trivial destructor, nor shall it be an array type whose elements
  (directly or indirectly) have non-trivial destructors.

* [basic.stc]

  Add 'thread storage duration' to the list in paragraph 1.

  Change paragraph 2

  Thread, static, and automatic storage durations are associated with
  objects introduced by declarations [...].

  Add ``__thread`` to the list of specifiers in paragraph 3.

* [basic.stc.thread]

  New section before [basic.stc.static]

  The keyword ``__thread`` applied to a non-local object gives the
  object thread storage duration.

  A local variable or class data member declared both ``static``
  and ``__thread`` gives the variable or member thread storage
  duration.

* [basic.stc.static]

  Change paragraph 1

  All objects that have neither thread storage duration, dynamic
  storage duration nor are local [...].

* [dcl.stc]

  Add ``__thread`` to the list in paragraph 1.

  Change paragraph 1

  With the exception of ``__thread``, at most one
  :samp:`{storage-class-specifier}` shall appear in a given
  :samp:`{decl-specifier-seq}`.  The ``__thread`` specifier may
  be used alone, or immediately following the ``extern`` or
  ``static`` specifiers.  [...]

  Add after paragraph 5

  The ``__thread`` specifier can be applied only to the names of objects
  and to anonymous unions.

* [class.mem]

  Add after paragraph 6

  Non- ``static`` members shall not be ``__thread``.
