! { dg-do run }
!
! TARGET actual to POINTER dummy with INTENT(IN)
!
program test
  implicit none
  integer, target :: a
  a = 66
  call foo(a)
  if (a /= 647) call abort()
contains
  subroutine foo(p)
    integer, pointer, intent(in) :: p
    if (a /= 66) call abort()
    if (p /= 66) call abort()
    p = 647
    if (p /= 647) call abort()
    if (a /= 647) call abort()
  end subroutine foo
end program test
