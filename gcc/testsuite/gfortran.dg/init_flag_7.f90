! { dg-do run }
! { dg-options "-finit-integer=101" }

program init_flag_7
  call save_test1 (.true.)
  call save_test1 (.false.) 
  call save_test2 (.true.)
  call save_test2 (.false.)
end program init_flag_7

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine save_test1 (first)
  logical first
  integer :: i1 = -100
  integer i2
  integer i3
  save i2
  if (first) then
     if (i1 .ne. -100) STOP 1
     if (i2 .ne. 101) STOP 2
     if (i3 .ne. 101) STOP 3
  else
     if (i1 .ne. 1001) STOP 4
     if (i2 .ne. 1002) STOP 5
     if (i3 .ne. 101) STOP 6
  end if
  i1 = 1001
  i2 = 1002
  i3 = 1003
end subroutine save_test1
        
subroutine save_test2 (first)
  logical first
  integer :: i1 = -100
  integer i2
  save
  if (first) then
     if (i1 .ne. -100) STOP 7
     if (i2 .ne. 101) STOP 8
  else
     if (i1 .ne. 1001) STOP 9
     if (i2 .ne. 1002) STOP 10
  end if
  i1 = 1001
  i2 = 1002
end subroutine save_test2
