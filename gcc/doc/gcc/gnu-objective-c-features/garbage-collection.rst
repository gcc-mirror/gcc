..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _garbage-collection:

Garbage Collection
******************

This section is specific for the GNU Objective-C runtime.  If you are
using a different runtime, you can skip it.

Support for garbage collection with the GNU runtime has been added by
using a powerful conservative garbage collector, known as the
Boehm-Demers-Weiser conservative garbage collector.

To enable the support for it you have to configure the compiler using
an additional argument, :option:`--enable-objc-gc`.  This will
build the boehm-gc library, and build an additional runtime library
which has several enhancements to support the garbage collector.  The
new library has a new name, :samp:`libobjc_gc.a` to not conflict with
the non-garbage-collected library.

When the garbage collector is used, the objects are allocated using the
so-called typed memory allocation mechanism available in the
Boehm-Demers-Weiser collector.  This mode requires precise information on
where pointers are located inside objects.  This information is computed
once per class, immediately after the class has been initialized.

There is a new runtime function ``class_ivar_set_gcinvisible()``
which can be used to declare a so-called :dfn:`weak pointer`
reference.  Such a pointer is basically hidden for the garbage collector;
this can be useful in certain situations, especially when you want to
keep track of the allocated objects, yet allow them to be
collected.  This kind of pointers can only be members of objects, you
cannot declare a global pointer as a weak reference.  Every type which is
a pointer type can be declared a weak pointer, including ``id``,
``Class`` and ``SEL``.

Here is an example of how to use this feature.  Suppose you want to
implement a class whose instances hold a weak pointer reference; the
following class does this:

.. code-block:: objective-c

  @interface WeakPointer : Object
  {
      const void* weakPointer;
  }

  - initWithPointer:(const void*)p;
  - (const void*)weakPointer;
  @end

  @implementation WeakPointer

  + (void)initialize
  {
    if (self == objc_lookUpClass ("WeakPointer"))
      class_ivar_set_gcinvisible (self, "weakPointer", YES);
  }

  - initWithPointer:(const void*)p
  {
    weakPointer = p;
    return self;
  }

  - (const void*)weakPointer
  {
    return weakPointer;
  }

  @end

Weak pointers are supported through a new type character specifier
represented by the :samp:`!` character.  The
``class_ivar_set_gcinvisible()`` function adds or removes this
specifier to the string type description of the instance variable named
as argument.