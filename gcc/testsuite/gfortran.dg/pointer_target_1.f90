! { dg-do run }
!
! TARGET actual to POINTER dummy with INTENT(IN)
!
program test
  implicit none
  integer, target :: a
  a = 66
  call foo(a)
  if (a /= 647) STOP 1
contains
  subroutine foo(p)
    integer, pointer, intent(in) :: p
    if (a /= 66) STOP 2
    if (p /= 66) STOP 3
    p = 647
    if (p /= 647) STOP 4
    if (a /= 647) STOP 5
  end subroutine foo
end program test
