! { dg-options "-Wundefined-do-loop" }
! Program to check corner cases for DO statements.

program do_1
  implicit none
  integer i, j

  ! limit=HUGE(i), step 1
  j = 0
  do i = HUGE(i) - 10, HUGE(i), 1 ! { dg-warning "is undefined as it overflows" }
    j = j + 1
  end do
  if (j .ne. 11) STOP 1

  ! limit=-HUGE(i)-1, step -1
  j = 0
  do i = -HUGE(i) + 10 - 1, -HUGE(i) - 1, -1 ! { dg-warning "is undefined as it underflows" }
    j = j + 1
  end do
  if (j .ne. 11) STOP 2

end program
