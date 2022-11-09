..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: pmf, pointer to member function, bound pointer to member function

.. _bound-member-functions:

Extracting the Function Pointer from a Bound Pointer to Member Function
***********************************************************************

In C++, pointer to member functions (PMFs) are implemented using a wide
pointer of sorts to handle all the possible call mechanisms; the PMF
needs to store information about how to adjust the :samp:`this` pointer,
and if the function pointed to is virtual, where to find the vtable, and
where in the vtable to look for the member function.  If you are using
PMFs in an inner loop, you should really reconsider that decision.  If
that is not an option, you can extract the pointer to the function that
would be called for a given object/PMF pair and call it directly inside
the inner loop, to save a bit of time.

Note that you still pay the penalty for the call through a
function pointer; on most modern architectures, such a call defeats the
branch prediction features of the CPU.  This is also true of normal
virtual function calls.

The syntax for this extension is

.. code-block:: c++

  extern A a;
  extern int (A::*fp)();
  typedef int (*fptr)(A *);

  fptr p = (fptr)(a.*fp);

For PMF constants (i.e. expressions of the form :samp:`&Klasse::Member`),
no object is needed to obtain the address of the function.  They can be
converted to function pointers directly:

.. code-block:: c++

  fptr p1 = (fptr)(&A::foo);

.. index:: Wno-pmf-conversions

You must specify :option:`-Wno-pmf-conversions` to use this extension.
