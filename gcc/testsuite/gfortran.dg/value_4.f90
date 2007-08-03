! { dg-do run }
! { dg-additional-sources value_4.c }
! { dg-options "-ff2c -w -O0" }
!
! Tests the functionality of the patch for PR29642, which requested the
! implementation of the F2003 VALUE attribute for gfortran, by calling
! external C functions by value and by reference.  This is effectively
! identical to c_by_val_1.f, which does the same for %VAL.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org> 
!
module global
  interface delta
    module procedure deltai, deltar, deltac
  end interface delta
  real(4) :: epsi = epsilon (1.0_4)
contains
  function deltai (a, b) result (c)
    integer(4) :: a, b
    logical :: c
    c = (a /= b)
  end function deltai

  function deltar (a, b) result (c)
    real(4) :: a, b
    logical :: c
    c = (abs (a-b) > epsi)
  end function deltar

  function deltac (a, b) result (c)
    complex(4) :: a, b
    logical :: c
    c = ((abs (real (a-b)) > epsi).or.(abs (aimag (a-b)) > epsi))
  end function deltac
end module global  

program value_4
  use global
  interface
    function f_to_f (x, y)
      real(4), pointer :: f_to_f
      real(4) :: x, y
      value :: x
    end function f_to_f
  end interface

  interface
    function i_to_i (x, y)
      integer(4), pointer :: i_to_i
      integer(4) :: x, y
      value :: x
    end function i_to_i
  end interface

  interface
    complex(4) function c_to_c (x, y)
      complex(4) :: x, y
      value :: x
    end function c_to_c
  end interface

  real(4)       a, b, c
  integer(4)    i, j, k
  complex(4)    u, v, w

  a = 42.0
  b = 0.0
  c = a
  b = f_to_f (a, c)
  if (delta ((2.0 * a), b)) call abort ()

  i = 99
  j = 0
  k = i
  j = i_to_i (i, k)
  if (delta ((3_4 * i), j)) call abort ()

  u = (-1.0, 2.0)
  v = (1.0, -2.0)
  w = u
  v = c_to_c (u, w)
  if (delta ((4.0 * u), v)) call abort ()
end program value_4
! { dg-final { cleanup-modules "global" } }
