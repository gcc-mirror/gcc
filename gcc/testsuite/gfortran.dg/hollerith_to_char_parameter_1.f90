! { dg-do compile }
! { dg-options "-Wconversion -std=legacy" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>

program test
  character(*), parameter :: h = 5hABCDE ! { dg-warning "HOLLERITH to CHARACTER\\(\\*\\)" }

  write(*,*) h
end program

