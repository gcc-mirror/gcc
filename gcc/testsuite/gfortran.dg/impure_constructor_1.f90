! { dg-do compile }
!
! PR fortran/43362
!
module m
  implicit none
  type t
    integer, pointer :: a
  end type t
  type t2
    type(t) :: b
  end type t2
  type t3
    type(t), pointer :: b
  end type t3
contains
 pure subroutine foo(x)
   type(t), target, intent(in) :: x
   type(t2) :: y
   type(t3) :: z

   ! The following gave an ICE but is valid:
   y = t2(x) ! Note: F2003, C1272 (3) and (4) do not apply
   
   ! Variant which is invalid as C1272 (3) applies
   z = t3(x) ! { dg-error "Invalid expression in the derived type constructor" }
 end subroutine foo
end module m


