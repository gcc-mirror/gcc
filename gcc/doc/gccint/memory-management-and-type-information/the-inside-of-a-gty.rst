..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gty-options:

The Inside of a GTY(())
***********************

Sometimes the C code is not enough to fully describe the type
structure.  Extra information can be provided with ``GTY`` options
and additional markers.  Some options take a parameter, which may be
either a string or a type name, depending on the parameter.  If an
option takes no parameter, it is acceptable either to omit the
parameter entirely, or to provide an empty string as a parameter.  For
example, ``GTY ((skip))`` and ``GTY ((skip ("")))`` are
equivalent.

When the parameter is a string, often it is a fragment of C code.  Four
special escapes may be used in these strings, to refer to pieces of
the data structure being marked:

.. index:: % in GTY option

``%h``
  The current structure.

``%1``
  The structure that immediately contains the current structure.

``%0``
  The outermost structure that contains the current structure.

``%a``
  A partial expression of the form ``[i1][i2]...`` that indexes
  the array item currently being marked.

  For instance, suppose that you have a structure of the form

.. code-block:: c++

  struct A {
    ...
  };
  struct B {
    struct A foo[12];
  };

and ``b`` is a variable of type ``struct B``.  When marking
:samp:`b.foo[11]`, ``%h`` would expand to :samp:`b.foo[11]`,
``%0`` and ``%1`` would both expand to :samp:`b`, and ``%a``
would expand to :samp:`[11]`.

As in ordinary C, adjacent strings will be concatenated; this is
helpful when you have a complicated expression.

.. code-block:: c++

  GTY ((chain_next ("TREE_CODE (&%h.generic) == INTEGER_TYPE"
                    " ? TYPE_NEXT_VARIANT (&%h.generic)"
                    " : TREE_CHAIN (&%h.generic)")))

The available options are:

.. index:: length

:samp:`length ("{expression}")`
  There are two places the type machinery will need to be explicitly told
  the length of an array of non-atomic objects.  The first case is when a
  structure ends in a variable-length array, like this:

  .. code-block:: c++

    struct GTY(()) rtvec_def {
      int num_elem;         /* number of elements */
      rtx GTY ((length ("%h.num_elem"))) elem[1];
    };

  In this case, the ``length`` option is used to override the specified
  array length (which should usually be ``1``).  The parameter of the
  option is a fragment of C code that calculates the length.

  The second case is when a structure or a global variable contains a
  pointer to an array, like this:

  .. code-block:: c++

    struct gimple_omp_for_iter * GTY((length ("%h.collapse"))) iter;

  In this case, ``iter`` has been allocated by writing something like

  .. code-block:: c++

      x->iter = ggc_alloc_cleared_vec_gimple_omp_for_iter (collapse);

  and the ``collapse`` provides the length of the field.

  This second use of ``length`` also works on global variables, like:

  .. code-block:: c++

    static GTY((length("reg_known_value_size"))) rtx *reg_known_value;

  Note that the ``length`` option is only meant for use with arrays of
  non-atomic objects, that is, objects that contain pointers pointing to
  other GTY-managed objects.  For other GC-allocated arrays and strings
  you should use ``atomic`` or ``string_length``.

  .. index:: string_length

:samp:`string_length ("{expression}")`
  In order to simplify production of PCH, a structure member that is a plain
  array of bytes (an optionally ``const`` and/or ``unsigned`` ``char
  *``) is treated specially by the infrastructure. Even if such an array has not
  been allocated in GC-controlled memory, it will still be written properly into
  a PCH.  The machinery responsible for this needs to know the length of the
  data; by default, the length is determined by calling ``strlen`` on the
  pointer.  The ``string_length`` option specifies an alternate way to
  determine the length, such as by inspecting another struct member:

  .. code-block:: c++

    struct GTY(()) non_terminated_string {
      size_t sz;
      const char * GTY((string_length ("%h.sz"))) data;
    };

  .. index:: skip

``skip``
  If ``skip`` is applied to a field, the type machinery will ignore it.
  This is somewhat dangerous; the only safe use is in a union when one
  field really isn't ever used.

  .. index:: callback

``callback``
  ``callback`` should be applied to fields with pointer to function type
  and causes the field to be ignored similarly to ``skip``, except when
  writing PCH and the field is non-NULL it will remember the field's address
  for relocation purposes if the process writing PCH has different load base
  from a process reading PCH.

  .. index:: for_user

``for_user``
  Use this to mark types that need to be marked by user gc routines, but are not
  refered to in a template argument.  So if you have some user gc type T1 and a
  non user gc type T2 you can give T2 the for_user option so that the marking
  functions for T1 can call non mangled functions to mark T2.

  .. index:: desc, tag, default

:samp:`desc ("{expression}")` :samp:`tag ("{constant}")` ``default``
  The type machinery needs to be told which field of a ``union`` is
  currently active.  This is done by giving each field a constant
  ``tag`` value, and then specifying a discriminator using ``desc``.
  The value of the expression given by ``desc`` is compared against
  each ``tag`` value, each of which should be different.  If no
  ``tag`` is matched, the field marked with ``default`` is used if
  there is one, otherwise no field in the union will be marked.

  In the ``desc`` option, the 'current structure' is the union that
  it discriminates.  Use ``%1`` to mean the structure containing it.
  There are no escapes available to the ``tag`` option, since it is a
  constant.

  For example,

  .. code-block:: c++

    struct GTY(()) tree_binding
    {
      struct tree_common common;
      union tree_binding_u {
        tree GTY ((tag ("0"))) scope;
        struct cp_binding_level * GTY ((tag ("1"))) level;
      } GTY ((desc ("BINDING_HAS_LEVEL_P ((tree)&%0)"))) xscope;
      tree value;
    };

  In this example, the value of BINDING_HAS_LEVEL_P when applied to a
  ``struct tree_binding *`` is presumed to be 0 or 1.  If 1, the type
  mechanism will treat the field ``level`` as being present and if 0,
  will treat the field ``scope`` as being present.

  The ``desc`` and ``tag`` options can also be used for inheritance
  to denote which subclass an instance is.  See :ref:`inheritance-and-gty`
  for more information.

  .. index:: cache

``cache``
  When the ``cache`` option is applied to a global variable gt_cleare_cache is
  called on that variable between the mark and sweep phases of garbage
  collection.  The gt_clear_cache function is free to mark blocks as used, or to
  clear pointers in the variable.

  .. index:: deletable

``deletable``
  ``deletable``, when applied to a global variable, indicates that when
  garbage collection runs, there's no need to mark anything pointed to
  by this variable, it can just be set to ``NULL`` instead.  This is used
  to keep a list of free structures around for re-use.

  .. index:: maybe_undef

``maybe_undef``
  When applied to a field, ``maybe_undef`` indicates that it's OK if
  the structure that this fields points to is never defined, so long as
  this field is always ``NULL``.  This is used to avoid requiring
  backends to define certain optional structures.  It doesn't work with
  language frontends.

  .. index:: nested_ptr

:samp:`nested_ptr ({type}, "{to expression}", "{from expression}")`
  The type machinery expects all pointers to point to the start of an
  object.  Sometimes for abstraction purposes it's convenient to have
  a pointer which points inside an object.  So long as it's possible to
  convert the original object to and from the pointer, such pointers
  can still be used.  :samp:`{type}` is the type of the original object,
  the :samp:`{to expression}` returns the pointer given the original object,
  and the :samp:`{from expression}` returns the original object given
  the pointer.  The pointer will be available using the ``%h``
  escape.

  .. index:: chain_next, chain_prev, chain_circular

:samp:`chain_next ("{expression}")` :samp:`chain_prev ("{expression}")` :samp:`chain_circular ("{expression}")`
  It's helpful for the type machinery to know if objects are often
  chained together in long lists; this lets it generate code that uses
  less stack space by iterating along the list instead of recursing down
  it.  ``chain_next`` is an expression for the next item in the list,
  ``chain_prev`` is an expression for the previous item.  For singly
  linked lists, use only ``chain_next`` ; for doubly linked lists, use
  both.  The machinery requires that taking the next item of the
  previous item gives the original item.  ``chain_circular`` is similar
  to ``chain_next``, but can be used for circular single linked lists.

  .. index:: reorder

:samp:`reorder ("{function name}")`
  Some data structures depend on the relative ordering of pointers.  If
  the precompiled header machinery needs to change that ordering, it
  will call the function referenced by the ``reorder`` option, before
  changing the pointers in the object that's pointed to by the field the
  option applies to.  The function must take four arguments, with the
  signature :samp:`void \*, void \*, gt_pointer_operator, void \*`.
  The first parameter is a pointer to the structure that contains the
  object being updated, or the object itself if there is no containing
  structure.  The second parameter is a cookie that should be ignored.
  The third parameter is a routine that, given a pointer, will update it
  to its correct new value.  The fourth parameter is a cookie that must
  be passed to the second parameter.

  PCH cannot handle data structures that depend on the absolute values
  of pointers.  ``reorder`` functions can be expensive.  When
  possible, it is better to depend on properties of the data, like an ID
  number or the hash of a string instead.

  .. index:: atomic

``atomic``
  The ``atomic`` option can only be used with pointers.  It informs
  the GC machinery that the memory that the pointer points to does not
  contain any pointers, and hence it should be treated by the GC and PCH
  machinery as an 'atomic' block of memory that does not need to be
  examined when scanning memory for pointers.  In particular, the
  machinery will not scan that memory for pointers to mark them as
  reachable (when marking pointers for GC) or to relocate them (when
  writing a PCH file).

  The ``atomic`` option differs from the ``skip`` option.
  ``atomic`` keeps the memory under Garbage Collection, but makes the
  GC ignore the contents of the memory.  ``skip`` is more drastic in
  that it causes the pointer and the memory to be completely ignored by
  the Garbage Collector.  So, memory marked as ``atomic`` is
  automatically freed when no longer reachable, while memory marked as
  ``skip`` is not.

  The ``atomic`` option must be used with great care, because all
  sorts of problem can occur if used incorrectly, that is, if the memory
  the pointer points to does actually contain a pointer.

  Here is an example of how to use it:

  .. code-block:: c++

    struct GTY(()) my_struct {
      int number_of_elements;
      unsigned int * GTY ((atomic)) elements;
    };

  In this case, ``elements`` is a pointer under GC, and the memory it
  points to needs to be allocated using the Garbage Collector, and will
  be freed automatically by the Garbage Collector when it is no longer
  referenced.  But the memory that the pointer points to is an array of
  ``unsigned int`` elements, and the GC must not try to scan it to
  find pointers to mark or relocate, which is why it is marked with the
  ``atomic`` option.

  Note that, currently, global variables cannot be marked with
  ``atomic`` ; only fields of a struct can.  This is a known
  limitation.  It would be useful to be able to mark global pointers
  with ``atomic`` to make the PCH machinery aware of them so that
  they are saved and restored correctly to PCH files.

  .. index:: special

:samp:`special ("{name}")`
  The ``special`` option is used to mark types that have to be dealt
  with by special case machinery.  The parameter is the name of the
  special case.  See :samp:`gengtype.cc` for further details.  Avoid
  adding new special cases unless there is no other alternative.

  .. index:: user

``user``
  The ``user`` option indicates that the code to mark structure
  fields is completely handled by user-provided routines.  See section
  :ref:`user-gc` for details on what functions need to be provided.