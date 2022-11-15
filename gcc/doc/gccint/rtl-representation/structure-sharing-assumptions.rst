..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: sharing of RTL components, RTL structure sharing assumptions

.. _sharing:

Structure Sharing Assumptions
*****************************

The compiler assumes that certain kinds of RTL expressions are unique;
there do not exist two distinct objects representing the same value.
In other cases, it makes an opposite assumption: that no RTL expression
object of a certain kind appears in more than one place in the
containing structure.

These assumptions refer to a single function; except for the RTL
objects that describe global variables and external functions,
and a few standard objects such as small integer constants,
no RTL objects are common to two functions.

.. index:: reg, RTL sharing

* Each pseudo-register has only a single ``reg`` object to represent it,
  and therefore only a single machine mode.

  .. index:: symbolic label, symbol_ref, RTL sharing

* For any symbolic label, there is only one ``symbol_ref`` object
  referring to it.

  .. index:: const_int, RTL sharing

* All ``const_int`` expressions with equal values are shared.

  .. index:: const_poly_int, RTL sharing

* All ``const_poly_int`` expressions with equal modes and values
  are shared.

  .. index:: pc, RTL sharing

* There is only one ``pc`` expression.

  .. index:: const_double, RTL sharing

* There is only one ``const_double`` expression with value 0 for
  each floating point mode.  Likewise for values 1 and 2.

  .. index:: const_vector, RTL sharing

* There is only one ``const_vector`` expression with value 0 for
  each vector mode, be it an integer or a double constant vector.

  .. index:: label_ref, RTL sharing, scratch, RTL sharing

* No ``label_ref`` or ``scratch`` appears in more than one place in
  the RTL structure; in other words, it is safe to do a tree-walk of all
  the insns in the function and assume that each time a ``label_ref``
  or ``scratch`` is seen it is distinct from all others that are seen.

  .. index:: mem, RTL sharing

* Only one ``mem`` object is normally created for each static
  variable or stack slot, so these objects are frequently shared in all
  the places they appear.  However, separate but equal objects for these
  variables are occasionally made.

  .. index:: asm_operands, RTL sharing

* When a single ``asm`` statement has multiple output operands, a
  distinct ``asm_operands`` expression is made for each output operand.
  However, these all share the vector which contains the sequence of input
  operands.  This sharing is used later on to test whether two
  ``asm_operands`` expressions come from the same statement, so all
  optimizations must carefully preserve the sharing if they copy the
  vector at all.

* No RTL object appears in more than one place in the RTL structure
  except as described above.  Many passes of the compiler rely on this
  by assuming that they can modify RTL objects in place without unwanted
  side-effects on other insns.

  .. index:: unshare_all_rtl

* During initial RTL generation, shared structure is freely introduced.
  After all the RTL for a function has been generated, all shared
  structure is copied by ``unshare_all_rtl`` in :samp:`emit-rtl.cc`,
  after which the above rules are guaranteed to be followed.

  .. index:: copy_rtx_if_shared

* During the combiner pass, shared structure within an insn can exist
  temporarily.  However, the shared structure is copied before the
  combiner is finished with the insn.  This is done by calling
  ``copy_rtx_if_shared``, which is a subroutine of
  ``unshare_all_rtl``.
