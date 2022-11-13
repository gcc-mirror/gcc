..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: interface and implementation headers, C++, C++ interface and implementation headers, pragmas, interface and implementation

.. _c++-interface:

C++ Interface and Implementation Pragmas
****************************************

``#pragma interface`` and ``#pragma implementation`` provide the
user with a way of explicitly directing the compiler to emit entities
with vague linkage (and debugging information) in a particular
translation unit.

.. note::

  These ``#pragma`` s have been superceded as of GCC 2.7.2
  by COMDAT support and the 'key method' heuristic
  mentioned in :ref:`vague-linkage`.  Using them can actually cause your
  program to grow due to unnecessary out-of-line copies of inline
  functions.

``#pragma interface`` :samp:`#pragma interface "{subdir}/{objects}.h"`

  .. index:: #pragma interface

  Use this directive in *header files* that define object classes, to save
  space in most of the object files that use those classes.  Normally,
  local copies of certain information (backup copies of inline member
  functions, debugging information, and the internal tables that implement
  virtual functions) must be kept in each object file that includes class
  definitions.  You can use this pragma to avoid such duplication.  When a
  header file containing :samp:`#pragma interface` is included in a
  compilation, this auxiliary information is not generated (unless
  the main input source file itself uses :samp:`#pragma implementation`).
  Instead, the object files contain references to be resolved at link
  time.

  The second form of this directive is useful for the case where you have
  multiple headers with the same name in different directories.  If you
  use this form, you must specify the same string to :samp:`#pragma
  implementation`.

``#pragma implementation`` :samp:`#pragma implementation "{objects}.h"`

  .. index:: #pragma implementation

  Use this pragma in a *main input file*, when you want full output from
  included header files to be generated (and made globally visible).  The
  included header file, in turn, should use :samp:`#pragma interface`.
  Backup copies of inline member functions, debugging information, and the
  internal tables used to implement virtual functions are all generated in
  implementation files.

  .. index:: implied #pragma implementation, #pragma implementation, implied, naming convention, implementation headers

  If you use :samp:`#pragma implementation` with no argument, it applies to
  an include file with the same basenameA file's :dfn:`basename`
  is the name stripped of all leading path information and of trailing
  suffixes, such as :samp:`.h` or :samp:`.C` or :samp:`.cc`.
  as your source
  file.  For example, in :samp:`allclass.cc`, giving just
  :samp:`#pragma implementation`
  by itself is equivalent to :samp:`#pragma implementation "allclass.h"`.

  Use the string argument if you want a single implementation file to
  include code from multiple header files.  (You must also use
  :samp:`#include` to include the header file; :samp:`#pragma
  implementation` only specifies how to use the file---it doesn't actually
  include it.)

  There is no way to split up the contents of a single header file into
  multiple implementation files.

.. index:: inlining and C++ pragmas, C++ pragmas, effect on inlining, pragmas in C++, effect on inlining

:samp:`#pragma implementation` and :samp:`#pragma interface` also have an
effect on function inlining.

If you define a class in a header file marked with :samp:`#pragma
interface`, the effect on an inline function defined in that class is
similar to an explicit ``extern`` declaration---the compiler emits
no code at all to define an independent version of the function.  Its
definition is used only for inlining with its callers.

.. index:: fno-implement-inlines

Conversely, when you include the same header file in a main source file
that declares it as :samp:`#pragma implementation`, the compiler emits
code for the function itself; this defines a version of the function
that can be found via pointers (or by callers compiled without
inlining).  If all calls to the function can be inlined, you can avoid
emitting the function by compiling with :option:`-fno-implement-inlines`.
If any calls are not inlined, you will get linker errors.