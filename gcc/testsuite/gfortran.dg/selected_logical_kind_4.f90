! { dg-do run }

! Check that SELECTED_LOGICAL_KIND works in a non-constant context
! (which is rare but allowed)

subroutine foo(i, j)
  implicit none
  integer :: i, j
  if (selected_logical_kind(i) /= j) STOP j
end subroutine

program selected
  implicit none

  call foo(1, 1)
  call foo(8, 1)
  call foo(9, 2)
  call foo(16, 2)
  call foo(17, 4)
  call foo(32, 4)
  call foo(33, 8)
  call foo(64, 8)
end program
