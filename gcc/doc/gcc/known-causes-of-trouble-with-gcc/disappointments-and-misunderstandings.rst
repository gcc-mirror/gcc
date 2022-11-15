..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _disappointments:

Disappointments and Misunderstandings
*************************************

These problems are perhaps regrettable, but we don't know any practical
way around them.

* Certain local variables aren't recognized by debuggers when you compile
  with optimization.

  This occurs because sometimes GCC optimizes the variable out of
  existence.  There is no way to tell the debugger how to compute the
  value such a variable 'would have had', and it is not clear that would
  be desirable anyway.  So GCC simply does not mention the eliminated
  variable when it writes debugging information.

  You have to expect a certain amount of disagreement between the
  executable and your source code, when you use optimization.

  .. index:: conflicting types, scope of declaration

* Users often think it is a bug when GCC reports an error for code
  like this:

  .. code-block:: c++

    int foo (struct mumble *);

    struct mumble { ... };

    int foo (struct mumble *x)
    { ... }

  This code really is erroneous, because the scope of ``struct
  mumble`` in the prototype is limited to the argument list containing it.
  It does not refer to the ``struct mumble`` defined with file scope
  immediately below---they are two unrelated types with similar names in
  different scopes.

  But in the definition of ``foo``, the file-scope type is used
  because that is available to be inherited.  Thus, the definition and
  the prototype do not match, and you get an error.

  This behavior may seem silly, but it's what the ISO standard specifies.
  It is easy enough for you to make your code work by moving the
  definition of ``struct mumble`` above the prototype.  It's not worth
  being incompatible with ISO C just to avoid an error for the example
  shown above.

* Accesses to bit-fields even in volatile objects works by accessing larger
  objects, such as a byte or a word.  You cannot rely on what size of
  object is accessed in order to read or write the bit-field; it may even
  vary for a given bit-field according to the precise usage.

  If you care about controlling the amount of memory that is accessed, use
  volatile but do not use bit-fields.

* GCC comes with shell scripts to fix certain known problems in system
  header files.  They install corrected copies of various header files in
  a special directory where only GCC will normally look for them.  The
  scripts adapt to various systems by searching all the system header
  files for the problem cases that we know about.

  If new system header files are installed, nothing automatically arranges
  to update the corrected header files.  They can be updated using the
  :command:`mkheaders` script installed in
  :samp:`{libexecdir}/gcc/{target}/{version}/install-tools/`.

.. index:: floating point precision

* On 68000 and x86 systems, for instance, you can get paradoxical results
  if you test the precise values of floating point numbers.  For example,
  you can find that a floating point value which is not a NaN is not equal
  to itself.  This results from the fact that the floating point registers
  hold a few more bits of precision than fit in a ``double`` in memory.
  Compiled code moves values between memory and floating point registers
  at its convenience, and moving them into memory truncates them.

  .. index:: ffloat-store

  You can partially avoid this problem by using the :option:`-ffloat-store`
  option (see :ref:`optimize-options`).

* On AIX and other platforms without weak symbol support, templates
  need to be instantiated explicitly and symbols for static members
  of templates will not be generated.

* On AIX, GCC scans object files and library archives for static
  constructors and destructors when linking an application before the
  linker prunes unreferenced symbols.  This is necessary to prevent the
  AIX linker from mistakenly assuming that static constructor or
  destructor are unused and removing them before the scanning can occur.
  All static constructors and destructors found will be referenced even
  though the modules in which they occur may not be used by the program.
  This may lead to both increased executable size and unexpected symbol
  references.
