..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _is_contiguous:

IS_CONTIGUOUS --- Test whether an array is contiguous
*****************************************************

.. index:: IS_IOSTAT_EOR, array, contiguity

.. function:: IS_CONTIGUOUS(ARRAY)

  ``IS_CONTIGUOUS`` tests whether an array is contiguous.

  :param ARRAY:
    Shall be an array of any type.

  :return:
    Returns a ``LOGICAL`` of the default kind, which ``.TRUE.`` if
    :samp:`{ARRAY}` is contiguous and false otherwise.

  Standard:
    Fortran 2008 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = IS_CONTIGUOUS(ARRAY)

  Example:
    .. code-block:: fortran

      program test
        integer :: a(10)
        a = [1,2,3,4,5,6,7,8,9,10]
        call sub (a)      ! every element, is contiguous
        call sub (a(::2)) ! every other element, is noncontiguous
      contains
        subroutine sub (x)
          integer :: x(:)
          if (is_contiguous (x)) then
            write (*,*) 'X is contiguous'
          else
            write (*,*) 'X is not contiguous'
          end if
        end subroutine sub
      end program test
