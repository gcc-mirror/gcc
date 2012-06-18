! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/53643
!
type t
 integer, allocatable :: comp
end type t
contains
 subroutine foo(x,y)
   class(t), allocatable, intent(out) :: x(:)
   class(t), intent(out) :: y(:)
 end subroutine
 subroutine foo2(x,y)
   class(t), allocatable, intent(out) :: x
   class(t), intent(out) :: y
 end subroutine
 subroutine bar(x,y)
   class(t), intent(out) :: x(:)[*]
   class(t), intent(out) :: y[*]
 end subroutine
 subroutine bar2(x,y)
   type(t), intent(out) :: x(:)[*]
   type(t), intent(out) :: y[*]
 end subroutine
end
