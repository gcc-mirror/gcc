! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Unequal character length" }

! PR fortran/31822
! Verify that runtime checks for matching character length
! in pointer assignment work.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program ptr
  implicit none
  character(len=10), target :: s1
  character(len=5), pointer :: p1
  integer, volatile :: i
  i = 8
  p1 => s1(1:i) 
end program ptr

! { dg-output "Unequal character lengths \\(5/8\\)" }
