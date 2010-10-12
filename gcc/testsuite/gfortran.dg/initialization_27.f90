! { dg-do run }
!
! PR fortran/45489
!
! Check that non-referenced variables are default
! initialized if they are INTENT(OUT) or function results.
! Only the latter (i.e. "x=f()") was not working before
! PR 45489 was fixed.
!
program test_init
  implicit none
  integer, target :: tgt
  type A
    integer, pointer:: p => null ()
    integer:: i=3
  end type A
  type(A):: x, y(3)
  x=f()
  if (associated(x%p) .or. x%i /= 3) call abort ()
  y(1)%p => tgt
  y%i = 99
  call sub1(3,y)
  if (associated(y(1)%p) .or. any(y(:)%i /= 3)) call abort ()
  y(1)%p => tgt
  y%i = 99
  call sub2(y)
  if (associated(y(1)%p) .or. any(y(:)%i /= 3)) call abort ()
contains
 function f() result (fr)
    type(A):: fr
 end function f
 subroutine sub1(n,x)
   integer :: n
   type(A), intent(out) :: x(n:n+2)
 end subroutine sub1
 subroutine sub2(x)
   type(A), intent(out) :: x(:)
 end subroutine sub2
end program test_init
