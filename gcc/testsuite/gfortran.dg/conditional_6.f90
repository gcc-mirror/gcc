! { dg-do run }
! { dg-options "-std=f2023" }
program conditional_arg
  implicit none
  integer :: a = 4
  integer :: b = 5
  call five((a < 5 ? a : b))
  if (a /= 5) stop 1
contains
  subroutine five(x)
    integer, optional :: x
    if (present(x)) then
      x = 5
    end if
  end subroutine five
end program conditional_arg
