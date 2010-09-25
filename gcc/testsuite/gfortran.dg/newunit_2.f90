! { dg-do compile }
! { dg-options "-std=f95" }

! PR40008 F2008: Add NEWUNIT= for OPEN statement 
! Check for rejection with pre-F2008 standard.

! Contributed by Daniel Kraft, d@domob.eu.

program main
  character(len=25) :: str
  integer(1) :: myunit

  open (newunit=myunit, file="some_file") ! { dg-error "Fortran 2008" }
  close (unit=myunit)
end program main
