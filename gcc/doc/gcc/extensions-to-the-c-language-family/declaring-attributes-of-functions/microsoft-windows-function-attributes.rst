..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _microsoft-windows-function-attributes:

Microsoft Windows Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following attributes are available on Microsoft Windows and Symbian OS
targets.

.. index:: dllexport function attribute, __declspec(dllexport)

.. microsoft-windows-fn-attr:: dllexport

  On Microsoft Windows targets and Symbian OS targets the
  :microsoft-windows-fn-attr:`dllexport` attribute causes the compiler to provide a global
  pointer to a pointer in a DLL, so that it can be referenced with the
  :microsoft-windows-fn-attr:`dllimport` attribute.  On Microsoft Windows targets, the pointer
  name is formed by combining ``_imp__`` and the function or variable
  name.

  You can use ``__declspec(dllexport)`` as a synonym for
  ``__attribute__ ((dllexport))`` for compatibility with other
  compilers.

  On systems that support the :microsoft-windows-fn-attr:`visibility` attribute, this
  attribute also implies 'default' visibility.  It is an error to
  explicitly specify any other visibility.

  GCC's default behavior is to emit all inline functions with the
  :microsoft-windows-fn-attr:`dllexport` attribute.  Since this can cause object file-size bloat,
  you can use :option:`-fno-keep-inline-dllexport`, which tells GCC to
  ignore the attribute for inlined functions unless the
  :option:`-fkeep-inline-functions` flag is used instead.

  The attribute is ignored for undefined symbols.

  When applied to C++ classes, the attribute marks defined non-inlined
  member functions and static data members as exports.  Static consts
  initialized in-class are not marked unless they are also defined
  out-of-class.

  For Microsoft Windows targets there are alternative methods for
  including the symbol in the DLL's export table such as using a
  :samp:`.def` file with an ``EXPORTS`` section or, with GNU ld, using
  the :option:`--export-all` linker flag.

.. index:: dllimport function attribute, __declspec(dllimport)

.. microsoft-windows-fn-attr:: dllimport

  On Microsoft Windows and Symbian OS targets, the :microsoft-windows-fn-attr:`dllimport`
  attribute causes the compiler to reference a function or variable via
  a global pointer to a pointer that is set up by the DLL exporting the
  symbol.  The attribute implies ``extern``.  On Microsoft Windows
  targets, the pointer name is formed by combining ``_imp__`` and the
  function or variable name.

  You can use ``__declspec(dllimport)`` as a synonym for
  ``__attribute__ ((dllimport))`` for compatibility with other
  compilers.

  On systems that support the :microsoft-windows-fn-attr:`visibility` attribute, this
  attribute also implies 'default' visibility.  It is an error to
  explicitly specify any other visibility.

  Currently, the attribute is ignored for inlined functions.  If the
  attribute is applied to a symbol *definition*, an error is reported.
  If a symbol previously declared :microsoft-windows-fn-attr:`dllimport` is later defined, the
  attribute is ignored in subsequent references, and a warning is emitted.
  The attribute is also overridden by a subsequent declaration as
  :microsoft-windows-fn-attr:`dllexport`.

  When applied to C++ classes, the attribute marks non-inlined
  member functions and static data members as imports.  However, the
  attribute is ignored for virtual methods to allow creation of vtables
  using thunks.

  On the SH Symbian OS target the :microsoft-windows-fn-attr:`dllimport` attribute also has
  another affect---it can cause the vtable and run-time type information
  for a class to be exported.  This happens when the class has a
  dllimported constructor or a non-inline, non-pure virtual function
  and, for either of those two conditions, the class also has an inline
  constructor or destructor and has a key function that is defined in
  the current translation unit.

  For Microsoft Windows targets the use of the :microsoft-windows-fn-attr:`dllimport`
  attribute on functions is not necessary, but provides a small
  performance benefit by eliminating a thunk in the DLL.  The use of the
  :microsoft-windows-fn-attr:`dllimport` attribute on imported variables can be avoided by passing the
  :option:`--enable-auto-import` switch to the GNU linker.  As with
  functions, using the attribute for a variable eliminates a thunk in
  the DLL.

  One drawback to using this attribute is that a pointer to a
  *variable* marked as :microsoft-windows-fn-attr:`dllimport` cannot be used as a constant
  address. However, a pointer to a *function* with the
  :microsoft-windows-fn-attr:`dllimport` attribute can be used as a constant initializer; in
  this case, the address of a stub function in the import lib is
  referenced.  On Microsoft Windows targets, the attribute can be disabled
  for functions by setting the :option:`-mnop-fun-dllimport` flag.
