! { dg-do run }
! Tests the fix for pr31214, in which the typespec for the entry would be lost,
! thereby causing the function to be disallowed, since the function and entry
! types did not match.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
module type_mod
  implicit none

  type x
     real x
  end type x
  type y
     real x
  end type y
  type z
     real x
  end type z

  interface assignment(=)
     module procedure equals
  end interface assignment(=)

  interface operator(//)
     module procedure a_op_b, b_op_a
  end interface operator(//)

  interface operator(==)
     module procedure a_po_b, b_po_a
  end interface operator(==)

  contains
     subroutine equals(x,y)
        type(z), intent(in) :: y
        type(z), intent(out) :: x

        x%x = y%x
     end subroutine equals

     function a_op_b(a,b)
        type(x), intent(in) :: a
        type(y), intent(in) :: b
        type(z) a_op_b
        type(z) b_op_a
        a_op_b%x = a%x + b%x
        return
     entry b_op_a(b,a)
        b_op_a%x = a%x - b%x
     end function a_op_b

     function a_po_b(a,b)
        type(x), intent(in) :: a
        type(y), intent(in) :: b
        type(z) a_po_b
        type(z) b_po_a
     entry b_po_a(b,a)
        a_po_b%x = a%x/b%x
     end function a_po_b
end module type_mod

program test
  use type_mod
  implicit none
  type(x) :: x1 = x(19.0_4)
  type(y) :: y1 = y(7.0_4)
  type(z) z1

  z1 = x1//y1
  if (abs(z1%x - (19.0_4 + 7.0_4)) > epsilon(x1%x)) STOP 1
  z1 = y1//x1
  if (abs(z1%x - (19.0_4 - 7.0_4)) > epsilon(x1%x)) STOP 2

  z1 = x1==y1
  if (abs(z1%x - 19.0_4/7.0_4) > epsilon(x1%x)) STOP 3
  z1 = y1==x1
  if (abs(z1%x - 19.0_4/7.0_4) > epsilon(x1%x)) STOP 4
end program test
