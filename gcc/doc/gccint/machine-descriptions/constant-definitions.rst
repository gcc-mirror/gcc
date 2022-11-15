..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: constant definitions, define_constants

.. _constant-definitions:

Constant Definitions
********************

Using literal constants inside instruction patterns reduces legibility and
can be a maintenance problem.

To overcome this problem, you may use the ``define_constants``
expression.  It contains a vector of name-value pairs.  From that
point on, wherever any of the names appears in the MD file, it is as
if the corresponding value had been written instead.  You may use
``define_constants`` multiple times; each appearance adds more
constants to the table.  It is an error to redefine a constant with
a different value.

To come back to the a29k load multiple example, instead of

.. code-block::

  (define_insn ""
    [(match_parallel 0 "load_multiple_operation"
       [(set (match_operand:SI 1 "gpc_reg_operand" "=r")
             (match_operand:SI 2 "memory_operand" "m"))
        (use (reg:SI 179))
        (clobber (reg:SI 179))])]
    ""
    "loadm 0,0,%1,%2")

You could write:

.. code-block::

  (define_constants [
      (R_BP 177)
      (R_FC 178)
      (R_CR 179)
      (R_Q  180)
  ])

  (define_insn ""
    [(match_parallel 0 "load_multiple_operation"
       [(set (match_operand:SI 1 "gpc_reg_operand" "=r")
             (match_operand:SI 2 "memory_operand" "m"))
        (use (reg:SI R_CR))
        (clobber (reg:SI R_CR))])]
    ""
    "loadm 0,0,%1,%2")

The constants that are defined with a define_constant are also output
in the insn-codes.h header file as #defines.

.. index:: enumerations, define_c_enum

You can also use the machine description file to define enumerations.
Like the constants defined by ``define_constant``, these enumerations
are visible to both the machine description file and the main C code.

The syntax is as follows:

.. code-block::

  (define_c_enum "name" [
    value0
    value1
    (value32 32)
    value33
    ...
    valuen
  ])

This definition causes the equivalent of the following C code to appear
in :samp:`insn-constants.h`:

.. code-block:: c++

  enum name {
    value0 = 0,
    value1 = 1,
    value32 = 32,
    value33 = 33,
    ...
    valuen = n
  };
  #define NUM_cname_VALUES (n + 1)

where :samp:`{cname}` is the capitalized form of :samp:`{name}`.
It also makes each :samp:`{valuei}` available in the machine description
file, just as if it had been declared with:

.. code-block::

  (define_constants [(valuei i)])

Each :samp:`{valuei}` is usually an upper-case identifier and usually
begins with :samp:`{cname}`.

You can split the enumeration definition into as many statements as
you like.  The above example is directly equivalent to:

.. code-block::

  (define_c_enum "name" [value0])
  (define_c_enum "name" [value1])
  ...
  (define_c_enum "name" [valuen])

Splitting the enumeration helps to improve the modularity of each
individual ``.md`` file.  For example, if a port defines its
synchronization instructions in a separate :samp:`sync.md` file,
it is convenient to define all synchronization-specific enumeration
values in :samp:`sync.md` rather than in the main :samp:`.md` file.

Some enumeration names have special significance to GCC:

``unspecv``

  .. index:: unspec_volatile

  If an enumeration called ``unspecv`` is defined, GCC will use it
  when printing out ``unspec_volatile`` expressions.  For example:

  .. code-block::

    (define_c_enum "unspecv" [
      UNSPECV_BLOCKAGE
    ])

  causes GCC to print :samp:`(unspec_volatile ... 0)` as:

  .. code-block:: c++

    (unspec_volatile ... UNSPECV_BLOCKAGE)

``unspec``

  .. index:: unspec

  If an enumeration called ``unspec`` is defined, GCC will use
  it when printing out ``unspec`` expressions.  GCC will also use
  it when printing out ``unspec_volatile`` expressions unless an
  ``unspecv`` enumeration is also defined.  You can therefore
  decide whether to keep separate enumerations for volatile and
  non-volatile expressions or whether to use the same enumeration
  for both.

.. index:: define_enum

.. _define_enum:

Another way of defining an enumeration is to use ``define_enum`` :

.. code-block::

  (define_enum "name" [
    value0
    value1
    ...
    valuen
  ])

This directive implies:

.. code-block::

  (define_c_enum "name" [
    cname_cvalue0
    cname_cvalue1
    ...
    cname_cvaluen
  ])

.. index:: define_enum_attr

where :samp:`{cvaluei}` is the capitalized form of :samp:`{valuei}`.
However, unlike ``define_c_enum``, the enumerations defined
by ``define_enum`` can be used in attribute specifications
(see :ref:`define_enum_attr`).
