..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: attribute of types, type attributes

.. _type-attributes:

Specifying Attributes of Types
******************************

The keyword ``__attribute__`` allows you to specify various special
properties of types.  Some type attributes apply only to structure and
union types, and in C++, also class types, while others can apply to
any type defined via a ``typedef`` declaration.  Unless otherwise
specified, the same restrictions and effects apply to attributes regardless
of whether a type is a trivial structure or a C++ class with user-defined
constructors, destructors, or a copy assignment.

Other attributes are defined for functions (see :ref:`function-attributes`),
labels (see :ref:`label-attributes`), enumerators (see :ref:`enumerator-attributes`), statements (see :ref:`statement-attributes`), and for variables
(see :ref:`variable-attributes`).

The ``__attribute__`` keyword is followed by an attribute specification
enclosed in double parentheses.

You may specify type attributes in an enum, struct or union type
declaration or definition by placing them immediately after the
``struct``, ``union`` or ``enum`` keyword.  You can also place
them just past the closing curly brace of the definition, but this is less
preferred because logically the type should be fully defined at
the closing brace.

You can also include type attributes in a ``typedef`` declaration.
See :ref:`attribute-syntax`, for details of the exact syntax for using
attributes.

.. _common-type-attributes:

Common Type Attributes
^^^^^^^^^^^^^^^^^^^^^^

The following type attributes are supported on most targets.

.. index:: aligned type attribute

.. type-attr:: aligned, aligned (alignment)

  The :type-attr:`aligned` attribute specifies a minimum alignment (in bytes) for
  variables of the specified type.  When specified, :samp:`{alignment}` must be
  a power of 2.  Specifying no :samp:`{alignment}` argument implies the maximum
  alignment for the target, which is often, but by no means always, 8 or 16
  bytes.  For example, the declarations:

  .. code-block:: c++

    struct __attribute__ ((aligned (8))) S { short f[3]; };
    typedef int more_aligned_int __attribute__ ((aligned (8)));

  force the compiler to ensure (as far as it can) that each variable whose
  type is ``struct S`` or ``more_aligned_int`` is allocated and
  aligned *at least* on a 8-byte boundary.  On a SPARC, having all
  variables of type ``struct S`` aligned to 8-byte boundaries allows
  the compiler to use the ``ldd`` and ``std`` (doubleword load and
  store) instructions when copying one variable of type ``struct S`` to
  another, thus improving run-time efficiency.

  Note that the alignment of any given ``struct`` or ``union`` type
  is required by the ISO C standard to be at least a perfect multiple of
  the lowest common multiple of the alignments of all of the members of
  the ``struct`` or ``union`` in question.  This means that you *can*
  effectively adjust the alignment of a ``struct`` or ``union``
  type by attaching an :type-attr:`aligned` attribute to any one of the members
  of such a type, but the notation illustrated in the example above is a
  more obvious, intuitive, and readable way to request the compiler to
  adjust the alignment of an entire ``struct`` or ``union`` type.

  As in the preceding example, you can explicitly specify the alignment
  (in bytes) that you wish the compiler to use for a given ``struct``
  or ``union`` type.  Alternatively, you can leave out the alignment factor
  and just ask the compiler to align a type to the maximum
  useful alignment for the target machine you are compiling for.  For
  example, you could write:

  .. code-block:: c++

    struct __attribute__ ((aligned)) S { short f[3]; };

  Whenever you leave out the alignment factor in an :type-attr:`aligned`
  attribute specification, the compiler automatically sets the alignment
  for the type to the largest alignment that is ever used for any data
  type on the target machine you are compiling for.  Doing this can often
  make copy operations more efficient, because the compiler can use
  whatever instructions copy the biggest chunks of memory when performing
  copies to or from the variables that have types that you have aligned
  this way.

  In the example above, if the size of each ``short`` is 2 bytes, then
  the size of the entire ``struct S`` type is 6 bytes.  The smallest
  power of two that is greater than or equal to that is 8, so the
  compiler sets the alignment for the entire ``struct S`` type to 8
  bytes.

  Note that although you can ask the compiler to select a time-efficient
  alignment for a given type and then declare only individual stand-alone
  objects of that type, the compiler's ability to select a time-efficient
  alignment is primarily useful only when you plan to create arrays of
  variables having the relevant (efficiently aligned) type.  If you
  declare or use arrays of variables of an efficiently-aligned type, then
  it is likely that your program also does pointer arithmetic (or
  subscripting, which amounts to the same thing) on pointers to the
  relevant type, and the code that the compiler generates for these
  pointer arithmetic operations is often more efficient for
  efficiently-aligned types than for other types.

  Note that the effectiveness of :type-attr:`aligned` attributes may be limited
  by inherent limitations in your linker.  On many systems, the linker is
  only able to arrange for variables to be aligned up to a certain maximum
  alignment.  (For some linkers, the maximum supported alignment may
  be very very small.)  If your linker is only able to align variables
  up to a maximum of 8-byte alignment, then specifying ``aligned (16)``
  in an ``__attribute__`` still only provides you with 8-byte
  alignment.  See your linker documentation for further information.

  When used on a struct, or struct member, the :type-attr:`aligned` attribute can
  only increase the alignment; in order to decrease it, the :type-attr:`packed`
  attribute must be specified as well.  When used as part of a typedef, the
  :type-attr:`aligned` attribute can both increase and decrease alignment, and
  specifying the :type-attr:`packed` attribute generates a warning.

  .. index:: warn_if_not_aligned type attribute

.. type-attr:: warn_if_not_aligned (alignment)

  This attribute specifies a threshold for the structure field, measured
  in bytes.  If the structure field is aligned below the threshold, a
  warning will be issued.  For example, the declaration:

  .. code-block:: c++

    typedef unsigned long long __u64
       __attribute__((aligned (4), warn_if_not_aligned (8)));

    struct foo
    {
      int i1;
      int i2;
      __u64 x;
    };

  causes the compiler to issue an warning on ``struct foo``, like
  :samp:`warning: alignment 4 of 'struct foo' is less than 8`.
  It is used to define ``struct foo`` in such a way that
  ``struct foo`` has the same layout and the structure field ``x``
  has the same alignment when ``__u64`` is aligned at either 4 or
  8 bytes.  Align ``struct foo`` to 8 bytes:

  .. code-block:: c++

    struct __attribute__ ((aligned (8))) foo
    {
      int i1;
      int i2;
      __u64 x;
    };

  silences the warning.  The compiler also issues a warning, like
  :samp:`warning: 'x' offset 12 in 'struct foo' isn't aligned to 8`,
  when the structure field has the misaligned offset:

  .. code-block:: c++

    struct __attribute__ ((aligned (8))) foo
    {
      int i1;
      int i2;
      int i3;
      __u64 x;
    };

  This warning can be disabled by :option:`-Wno-if-not-aligned`.

.. index:: alloc_size type attribute

.. type-attr:: alloc_size (position), alloc_size (position-1, position-2)

  The ``alloc_size`` type attribute may be applied to the definition
  of a type of a function that returns a pointer and takes at least one
  argument of an integer type.  It indicates that the returned pointer
  points to an object whose size is given by the function argument at
  :samp:`{position-1}`, or by the product of the arguments at :samp:`{position-1}`
  and :samp:`{position-2}`.  Meaningful sizes are positive values less than
  ``PTRDIFF_MAX``.  Other sizes are disagnosed when detected.  GCC uses
  this information to improve the results of ``__builtin_object_size``.

  For instance, the following declarations

  .. code-block:: c++

    typedef __attribute__ ((alloc_size (1, 2))) void*
      calloc_type (size_t, size_t);
    typedef __attribute__ ((alloc_size (1))) void*
      malloc_type (size_t);

  specify that ``calloc_type`` is a type of a function that, like
  the standard C function ``calloc``, returns an object whose size
  is given by the product of arguments 1 and 2, and that
  ``malloc_type``, like the standard C function ``malloc``,
  returns an object whose size is given by argument 1 to the function.

.. index:: copy type attribute

.. type-attr:: copy, copy (expression)

  The :type-attr:`copy` attribute applies the set of attributes with which
  the type of the :samp:`{expression}` has been declared to the declaration
  of the type to which the attribute is applied.  The attribute is
  designed for libraries that define aliases that are expected to
  specify the same set of attributes as the aliased symbols.
  The :type-attr:`copy` attribute can be used with types, variables, or
  functions.  However, the kind of symbol to which the attribute is
  applied (either varible or function) must match the kind of symbol
  to which the argument refers.
  The :type-attr:`copy` attribute copies only syntactic and semantic attributes
  but not attributes that affect a symbol's linkage or visibility such as
  ``alias``, :type-attr:`visibility`, or :type-attr:`weak`.  The :type-attr:`deprecated`
  attribute is also not copied.  See :ref:`common-function-attributes`.
  See :ref:`common-variable-attributes`.

  For example, suppose ``struct A`` below is defined in some third
  party library header to have the alignment requirement ``N`` and
  to force a warning whenever a variable of the type is not so aligned
  due to attribute :type-attr:`packed`.  Specifying the :type-attr:`copy` attribute
  on the definition on the unrelated ``struct B`` has the effect of
  copying all relevant attributes from the type referenced by the pointer
  expression to ``struct B``.

  .. code-block:: c++

    struct __attribute__ ((aligned (N), warn_if_not_aligned (N)))
    A { /* ... */ };
    struct __attribute__ ((copy ( (struct A *)0)) B { /* ... */ };

.. index:: deprecated type attribute

.. type-attr:: deprecated, deprecated (msg)

  The :type-attr:`deprecated` attribute results in a warning if the type
  is used anywhere in the source file.  This is useful when identifying
  types that are expected to be removed in a future version of a program.
  If possible, the warning also includes the location of the declaration
  of the deprecated type, to enable users to easily find further
  information about why the type is deprecated, or what they should do
  instead.  Note that the warnings only occur for uses and then only
  if the type is being applied to an identifier that itself is not being
  declared as deprecated.

  .. code-block:: c++

    typedef int T1 __attribute__ ((deprecated));
    T1 x;
    typedef T1 T2;
    T2 y;
    typedef T1 T3 __attribute__ ((deprecated));
    T3 z __attribute__ ((deprecated));

  results in a warning on line 2 and 3 but not lines 4, 5, or 6.  No
  warning is issued for line 4 because T2 is not explicitly
  deprecated.  Line 5 has no warning because T3 is explicitly
  deprecated.  Similarly for line 6.  The optional :samp:`{msg}`
  argument, which must be a string, is printed in the warning if
  present.  Control characters in the string will be replaced with
  escape sequences, and if the :option:`-fmessage-length` option is set
  to 0 (its default value) then any newline characters will be ignored.

  The :type-attr:`deprecated` attribute can also be used for functions and
  variables (see :ref:`function-attributes`, see :ref:`variable-attributes`.)

  The message attached to the attribute is affected by the setting of
  the :option:`-fmessage-length` option.

.. index:: unavailable type attribute

.. type-attr:: unavailable, unavailable (msg)

  The :type-attr:`unavailable` attribute behaves in the same manner as the
  :type-attr:`deprecated` one, but emits an error rather than a warning.  It is
  used to indicate that a (perhaps previously :type-attr:`deprecated`) type is
  no longer usable.

  The :type-attr:`unavailable` attribute can also be used for functions and
  variables (see :ref:`function-attributes`, see :ref:`variable-attributes`.)

.. index:: designated_init type attribute

.. type-attr:: designated_init

  This attribute may only be applied to structure types.  It indicates
  that any initialization of an object of this type must use designated
  initializers rather than positional initializers.  The intent of this
  attribute is to allow the programmer to indicate that a structure's
  layout may change, and that therefore relying on positional
  initialization will result in future breakage.

  GCC emits warnings based on this attribute by default; use
  :option:`-Wno-designated-init` to suppress them.

.. index:: may_alias type attribute

.. type-attr:: may_alias

  Accesses through pointers to types with this attribute are not subject
  to type-based alias analysis, but are instead assumed to be able to alias
  any other type of objects.
  In the context of section 6.5 paragraph 7 of the C99 standard,
  an lvalue expression
  dereferencing such a pointer is treated like having a character type.
  See :option:`-fstrict-aliasing` for more information on aliasing issues.
  This extension exists to support some vector APIs, in which pointers to
  one vector type are permitted to alias pointers to a different vector type.

  Note that an object of a type with this attribute does not have any
  special semantics.

  Example of use:

  .. code-block:: c++

    typedef short __attribute__ ((__may_alias__)) short_a;

    int
    main (void)
    {
      int a = 0x12345678;
      short_a *b = (short_a *) &a;

      b[1] = 0;

      if (a == 0x12345678)
        abort();

      exit(0);
    }

  If you replaced ``short_a`` with ``short`` in the variable
  declaration, the above program would abort when compiled with
  :option:`-fstrict-aliasing`, which is on by default at :option:`-O2` or
  above.

.. index:: mode type attribute

.. type-attr:: mode (mode)

  This attribute specifies the data type for the declaration---whichever
  type corresponds to the mode :samp:`{mode}`.  This in effect lets you
  request an integer or floating-point type according to its width.

  See :ref:`gccint:machine-modes`,
  for a list of the possible keywords for :samp:`{mode}`.
  You may also specify a mode of ``byte`` or ``__byte__`` to
  indicate the mode corresponding to a one-byte integer, ``word`` or
  ``__word__`` for the mode of a one-word integer, and ``pointer``
  or ``__pointer__`` for the mode used to represent pointers.

.. index:: packed type attribute

.. option:: packed

  This attribute, attached to a ``struct``, ``union``, or C++ ``class``
  type definition, specifies that each of its members (other than zero-width
  bit-fields) is placed to minimize the memory required.  This is equivalent
  to specifying the :type-attr:`packed` attribute on each of the members.

  When attached to an ``enum`` definition, the :type-attr:`packed` attribute
  indicates that the smallest integral type should be used.
  Specifying the :option:`-fshort-enums` flag on the command line
  is equivalent to specifying the :type-attr:`packed`
  attribute on all ``enum`` definitions.

  In the following example ``struct my_packed_struct`` 's members are
  packed closely together, but the internal layout of its ``s`` member
  is not packed---to do that, ``struct my_unpacked_struct`` needs to
  be packed too.

  .. code-block:: c++

    struct my_unpacked_struct
     {
        char c;
        int i;
     };

    struct __attribute__ ((__packed__)) my_packed_struct
      {
         char c;
         int  i;
         struct my_unpacked_struct s;
      };

  You may only specify the :type-attr:`packed` attribute on the definition
  of an ``enum``, ``struct``, ``union``, or ``class``,
  not on a ``typedef`` that does not also define the enumerated type,
  structure, union, or class.

.. index:: scalar_storage_order type attribute

.. type-attr:: scalar_storage_order ("endianness")

  When attached to a ``union`` or a ``struct``, this attribute sets
  the storage order, aka endianness, of the scalar fields of the type, as
  well as the array fields whose component is scalar.  The supported
  endiannesses are ``big-endian`` and ``little-endian``.  The attribute
  has no effects on fields which are themselves a ``union``, a ``struct``
  or an array whose component is a ``union`` or a ``struct``, and it is
  possible for these fields to have a different scalar storage order than the
  enclosing type.

  Note that neither pointer nor vector fields are considered scalar fields in
  this context, so the attribute has no effects on these fields.

  This attribute is supported only for targets that use a uniform default
  scalar storage order (fortunately, most of them), i.e. targets that store
  the scalars either all in big-endian or all in little-endian.

  Additional restrictions are enforced for types with the reverse scalar
  storage order with regard to the scalar storage order of the target:

  * Taking the address of a scalar field of a ``union`` or a
    ``struct`` with reverse scalar storage order is not permitted and yields
    an error.

  * Taking the address of an array field, whose component is scalar, of
    a ``union`` or a ``struct`` with reverse scalar storage order is
    permitted but yields a warning, unless :option:`-Wno-scalar-storage-order`
    is specified.

  * Taking the address of a ``union`` or a ``struct`` with reverse
    scalar storage order is permitted.

  These restrictions exist because the storage order attribute is lost when
  the address of a scalar or the address of an array with scalar component is
  taken, so storing indirectly through this address generally does not work.
  The second case is nevertheless allowed to be able to perform a block copy
  from or to the array.

  Moreover, the use of type punning or aliasing to toggle the storage order
  is not supported; that is to say, if a given scalar object can be accessed
  through distinct types that assign a different storage order to it, then the
  behavior is undefined.

.. index:: transparent_union type attribute

.. type-attr:: transparent_union

  This attribute, attached to a ``union`` type definition, indicates
  that any function parameter having that union type causes calls to that
  function to be treated in a special way.

  First, the argument corresponding to a transparent union type can be of
  any type in the union; no cast is required.  Also, if the union contains
  a pointer type, the corresponding argument can be a null pointer
  constant or a void pointer expression; and if the union contains a void
  pointer type, the corresponding argument can be any pointer expression.
  If the union member type is a pointer, qualifiers like ``const`` on
  the referenced type must be respected, just as with normal pointer
  conversions.

  Second, the argument is passed to the function using the calling
  conventions of the first member of the transparent union, not the calling
  conventions of the union itself.  All members of the union must have the
  same machine representation; this is necessary for this argument passing
  to work properly.

  Transparent unions are designed for library functions that have multiple
  interfaces for compatibility reasons.  For example, suppose the
  ``wait`` function must accept either a value of type ``int *`` to
  comply with POSIX, or a value of type ``union wait *`` to comply with
  the 4.1BSD interface.  If ``wait`` 's parameter were ``void *``,
  ``wait`` would accept both kinds of arguments, but it would also
  accept any other pointer type and this would make argument type checking
  less useful.  Instead, ``<sys/wait.h>`` might define the interface
  as follows:

  .. code-block:: c++

    typedef union __attribute__ ((__transparent_union__))
      {
        int *__ip;
        union wait *__up;
      } wait_status_ptr_t;

    pid_t wait (wait_status_ptr_t);

  This interface allows either ``int *`` or ``union wait *``
  arguments to be passed, using the ``int *`` calling convention.
  The program can call ``wait`` with arguments of either type:

  .. code-block:: c++

    int w1 () { int w; return wait (&w); }
    int w2 () { union wait w; return wait (&w); }

  With this interface, ``wait`` 's implementation might look like this:

  .. code-block:: c++

    pid_t wait (wait_status_ptr_t p)
    {
      return waitpid (-1, p.__ip, 0);
    }

.. index:: unused type attribute

.. type-attr:: unused

  When attached to a type (including a ``union`` or a ``struct``),
  this attribute means that variables of that type are meant to appear
  possibly unused.  GCC does not produce a warning for any variables of
  that type, even if the variable appears to do nothing.  This is often
  the case with lock or thread classes, which are usually defined and then
  not referenced, but contain constructors and destructors that have
  nontrivial bookkeeping functions.

.. index:: vector_size type attribute

.. type-attr:: vector_size (bytes)

  This attribute specifies the vector size for the type, measured in bytes.
  The type to which it applies is known as the :dfn:`base type`.  The :samp:`{bytes}`
  argument must be a positive power-of-two multiple of the base type size.  For
  example, the following declarations:

  .. code-block:: c++

    typedef __attribute__ ((vector_size (32))) int int_vec32_t ;
    typedef __attribute__ ((vector_size (32))) int* int_vec32_ptr_t;
    typedef __attribute__ ((vector_size (32))) int int_vec32_arr3_t[3];

  define ``int_vec32_t`` to be a 32-byte vector type composed of ``int``
  sized units.  With ``int`` having a size of 4 bytes, the type defines
  a vector of eight units, four bytes each.  The mode of variables of type
  ``int_vec32_t`` is ``V8SI``.  ``int_vec32_ptr_t`` is then defined
  to be a pointer to such a vector type, and ``int_vec32_arr3_t`` to be
  an array of three such vectors.  See :ref:`vector-extensions`, for details of
  manipulating objects of vector types.

  This attribute is only applicable to integral and floating scalar types.
  In function declarations the attribute applies to the function return
  type.

  For example, the following:

  .. code-block:: c++

    __attribute__ ((vector_size (16))) float get_flt_vec16 (void);

  declares ``get_flt_vec16`` to be a function returning a 16-byte vector
  with the base type ``float``.

.. index:: visibility type attribute

.. type-attr:: visibility

  In C++, attribute visibility (see :ref:`function-attributes`) can also be
  applied to class, struct, union and enum types.  Unlike other type
  attributes, the attribute must appear between the initial keyword and
  the name of the type; it cannot appear after the body of the type.

  Note that the type visibility is applied to vague linkage entities
  associated with the class (vtable, typeinfo node, etc.).  In
  particular, if a class is thrown as an exception in one shared object
  and caught in another, the class must have default visibility.
  Otherwise the two shared objects are unable to use the same
  typeinfo node and exception handling will break.

.. type-attr:: objc_root_class

  .. note::

    Objective-C and Objective-C++ only

  .. index:: objc_root_class type attribute

  This attribute marks a class as being a root class, and thus allows
  the compiler to elide any warnings about a missing superclass and to
  make additional checks for mandatory methods as needed.

To specify multiple attributes, separate them by commas within the
double parentheses: for example, :samp:`__attribute__ ((aligned (16),
packed))`.

.. index:: uncached type attribute, ARC

.. _arc-type-attributes:

ARC Type Attributes
^^^^^^^^^^^^^^^^^^^

Declaring objects with ``uncached`` allows you to exclude
data-cache participation in load and store operations on those objects
without involving the additional semantic implications of
``volatile``.  The ``.di`` instruction suffix is used for all
loads and stores of data declared ``uncached``.

.. index:: notshared type attribute, ARM

.. _arm-type-attributes:

ARM Type Attributes
^^^^^^^^^^^^^^^^^^^

On those ARM targets that support :type-attr:`dllimport` (such as Symbian
OS), you can use the ``notshared`` attribute to indicate that the
virtual table and other similar data for a class should not be
exported from a DLL.  For example:

.. code-block:: c++

  class __declspec(notshared) C {
  public:
    __declspec(dllimport) C();
    virtual void f();
  }

  __declspec(dllexport)
  C::C() {}

In this code, ``C::C`` is exported from the current DLL, but the
virtual table for ``C`` is not exported.  (You can use
``__attribute__`` instead of ``__declspec`` if you prefer, but
most Symbian OS code uses ``__declspec``.)

.. index:: preserve_access_index type attribute, BPF

.. _bpf-type-attributes:

BPF Type Attributes
^^^^^^^^^^^^^^^^^^^

BPF Compile Once - Run Everywhere (CO-RE) support. When attached to a
``struct`` or ``union`` type definition, indicates that CO-RE
relocation information should be generated for any access to a variable
of that type. The behavior is equivalent to the programmer manually
wrapping every such access with ``__builtin_preserve_access_index``.

.. index:: based type attribute, MeP, tiny type attribute, MeP, near type attribute, MeP, far type attribute, MeP

.. _mep-type-attributes:

MeP Type Attributes
^^^^^^^^^^^^^^^^^^^

Many of the MeP variable attributes may be applied to types as well.
Specifically, the :type-attr:`based`, :type-attr:`tiny`, :type-attr:`near`, and
:type-attr:`far` attributes may be applied to either.  The :type-attr:`io` and
:type-attr:`cb` attributes may not be applied to types.

.. _powerpc-type-attributes:

PowerPC Type Attributes
^^^^^^^^^^^^^^^^^^^^^^^

Three attributes currently are defined for PowerPC configurations:
``altivec``, :type-attr:`ms_struct` and ``gcc_struct``.

.. index:: ms_struct type attribute, PowerPC, gcc_struct type attribute, PowerPC

For full documentation of the :type-attr:`ms_struct` and ``gcc_struct``
attributes please see the documentation in :ref:`x86-type-attributes`.

.. index:: altivec type attribute, PowerPC

The ``altivec`` attribute allows one to declare AltiVec vector data
types supported by the AltiVec Programming Interface Manual.  The
attribute requires an argument to specify one of three vector types:
``vector__``, ``pixel__`` (always followed by unsigned short),
and ``bool__`` (always followed by unsigned).

.. code-block:: c++

  __attribute__((altivec(vector__)))
  __attribute__((altivec(pixel__))) unsigned short
  __attribute__((altivec(bool__))) unsigned

These attributes mainly are intended to support the ``__vector``,
``__pixel``, and ``__bool`` AltiVec keywords.

.. _x86-type-attributes:

x86 Type Attributes
^^^^^^^^^^^^^^^^^^^

Two attributes are currently defined for x86 configurations:
:x86-type-attr:`ms_struct` and ``gcc_struct``.

.. index:: ms_struct type attribute, x86, gcc_struct type attribute, x86

.. x86-type-attr:: ms_struct, gcc_struct

  If :type-attr:`packed` is used on a structure, or if bit-fields are used
  it may be that the Microsoft ABI packs them differently
  than GCC normally packs them.  Particularly when moving packed
  data between functions compiled with GCC and the native Microsoft compiler
  (either via function call or as data in a file), it may be necessary to access
  either format.

  The :type-attr:`ms_struct` and ``gcc_struct`` attributes correspond
  to the :option:`-mms-bitfields` and :option:`-mno-ms-bitfields`
  command-line options, respectively;
  see :ref:`x86-options`, for details of how structure layout is affected.
  See :ref:`x86-variable-attributes`, for information about the corresponding
  attributes on variables.
