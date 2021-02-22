! { dg-do run }
! { dg-options "-O2" }
! PR99169 - Segfault passing allocatable scalar into intent(out) dummy argument

program p
  implicit none
  integer, allocatable :: i
  allocate (i)
  call set (i)
  if (i /= 5) stop 1
contains
  subroutine set (i)
    integer, intent(out) :: i
    i = 5
  end subroutine set
end program p
