..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _co_reduce:

CO_REDUCE --- Reduction of values on the current set of images
**************************************************************

.. index:: CO_REDUCE, Collectives, generic reduction

.. function:: CO_REDUCE(A, OPERATOR, RESULT_IMAGE, STAT, ERRMSG)

  ``CO_REDUCE`` determines element-wise the reduction of the value of :samp:`{A}`
  on all images of the current team.  The pure function passed as :samp:`{OPERATION}`
  is used to pairwise reduce the values of :samp:`{A}` by passing either the value
  of :samp:`{A}` of different images or the result values of such a reduction as
  argument.  If :samp:`{A}` is an array, the deduction is done element wise. If
  :samp:`{RESULT_IMAGE}` is present, the result values are returned in :samp:`{A}` on
  the specified image only and the value of :samp:`{A}` on the other images become
  undefined.  If :samp:`{RESULT_IMAGE}` is not present, the value is returned on all
  images.  If the execution was successful and :samp:`{STAT}` is present, it is
  assigned the value zero.  If the execution failed, :samp:`{STAT}` gets assigned
  a nonzero value and, if present, :samp:`{ERRMSG}` gets assigned a value describing
  the occurred error.

  :param A:
    is an ``INTENT(INOUT)`` argument and shall be
    nonpolymorphic. If it is allocatable, it shall be allocated; if it is a pointer,
    it shall be associated.  :samp:`{A}` shall have the same type and type parameters on
    all images of the team; if it is an array, it shall have the same shape on all
    images.

  :param OPERATION:
    pure function with two scalar nonallocatable
    arguments, which shall be nonpolymorphic and have the same type and type
    parameters as :samp:`{A}`.  The function shall return a nonallocatable scalar of
    the same type and type parameters as :samp:`{A}`.  The function shall be the same on
    all images and with regards to the arguments mathematically commutative and
    associative.  Note that :samp:`{OPERATION}` may not be an elemental function, unless
    it is an intrisic function.

  :param RESULT_IMAGE:
    (optional) a scalar integer expression; if
    present, it shall have the same value on all images and refer to an
    image of the current team.

  :param STAT:
    (optional) a scalar integer variable

  :param ERRMSG:
    (optional) a scalar character variable

  Standard:
    Technical Specification (TS) 18508 or later

  Class:
    Collective subroutine

  Syntax:
    .. code-block:: fortran

      CALL CO_REDUCE(A, OPERATION, [, RESULT_IMAGE, STAT, ERRMSG])

  Example:
    .. code-block:: fortran

      program test
        integer :: val
        val = this_image ()
        call co_reduce (val, result_image=1, operation=myprod)
        if (this_image() == 1) then
          write(*,*) "Product value", val  ! prints num_images() factorial
        end if
      contains
        pure function myprod(a, b)
          integer, value :: a, b
          integer :: myprod
          myprod = a * b
        end function myprod
      end program test

  Note:
    While the rules permit in principle an intrinsic function, none of the
    intrinsics in the standard fulfill the criteria of having a specific
    function, which takes two arguments of the same type and returning that
    type as result.

  See also:
    :ref:`CO_MIN`,
    :ref:`CO_MAX`,
    :ref:`CO_SUM`,
    :ref:`CO_BROADCAST`