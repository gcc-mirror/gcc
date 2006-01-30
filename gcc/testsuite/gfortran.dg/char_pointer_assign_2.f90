! { dg-do compile }
! Tests the fix for PRs20895 and 25030, where pointer assignments
! of different length characters were accepted.
  character(4), target :: ch1(2)
  character(4), pointer :: ch2(:)
  character(5), pointer :: ch3(:)

  ch2 => ch1  ! Check correct is OK
  ch3 => ch1  ! { dg-error "Different character lengths" }

end