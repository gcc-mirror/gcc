..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _constant-string-objects:

Constant String Objects
***********************

GNU Objective-C provides constant string objects that are generated
directly by the compiler.  You declare a constant string object by
prefixing a C constant string with the character :samp:`@`:

.. code-block:: objective-c

    id myString = @"this is a constant string object";

The constant string objects are by default instances of the
``NXConstantString`` class which is provided by the GNU Objective-C
runtime.  To get the definition of this class you must include the
:samp:`objc/NXConstStr.h` header file.

User defined libraries may want to implement their own constant string
class.  To be able to support them, the GNU Objective-C compiler provides
a new command line options :option:`-fconstant-string-class=class-name`.
The provided class should adhere to a strict structure, the same
as ``NXConstantString`` 's structure:

.. code-block:: objective-c

  @interface MyConstantStringClass
  {
    Class isa;
    char *c_string;
    unsigned int len;
  }
  @end

``NXConstantString`` inherits from ``Object`` ; user class
libraries may choose to inherit the customized constant string class
from a different class than ``Object``.  There is no requirement in
the methods the constant string class has to implement, but the final
ivar layout of the class must be the compatible with the given
structure.

When the compiler creates the statically allocated constant string
object, the ``c_string`` field will be filled by the compiler with
the string; the ``length`` field will be filled by the compiler with
the string length; the ``isa`` pointer will be filled with
``NULL`` by the compiler, and it will later be fixed up automatically
at runtime by the GNU Objective-C runtime library to point to the class
which was set by the :option:`-fconstant-string-class` option when the
object file is loaded (if you wonder how it works behind the scenes, the
name of the class to use, and the list of static objects to fixup, are
stored by the compiler in the object file in a place where the GNU
runtime library will find them at runtime).

As a result, when a file is compiled with the
:option:`-fconstant-string-class` option, all the constant string objects
will be instances of the class specified as argument to this option.  It
is possible to have multiple compilation units referring to different
constant string classes, neither the compiler nor the linker impose any
restrictions in doing this.