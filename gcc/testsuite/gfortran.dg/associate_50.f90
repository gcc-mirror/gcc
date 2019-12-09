! { dg-do compile }
! PR 92780 - this used to ICE instead of being rejected.
! Test case by Gerhard Steinmetz.

program p
  associate (y => p) ! { dg-error "is a PROGRAM" }
  end associate
end program p
