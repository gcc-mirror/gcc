! { dg-do run }
! { dg-options "-fno-automatic" }
!
! PR fortran/37835
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>.
!
subroutine foo(n)
  integer :: n
  type t
    integer :: i = 42
  end type t
  type(t) :: myt
  if(n==1) myt%i = 2
  print *, myt%i
  if (n > 1 .and. myt%i /= 2) stop 1
end subroutine foo

call foo(1)
call foo(2)
end
