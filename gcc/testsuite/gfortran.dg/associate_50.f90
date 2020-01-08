! { dg-do compile }
! PR 92780 - this used to ICE instead of being rejected.
! Test case by Gerhard Steinmetz.

program p
  associate (y => p) ! { dg-error "Invalid association target" }
  end associate  ! { dg-error "Expecting END PROGRAM statement" }
end program p
