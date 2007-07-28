! { dg-do compile }
! One of two tests for the fix of PR23152 - An ICE would
! ensue from assumed shape arrays in namelists.
!
! Conributed by Paul Thomas  <pault@gcc.gnu.org>
!
program assumed_size_nml
  real, dimension (10) :: z
  z = 42.0
  call foo (z)
contains
  subroutine foo (y)
    real, DIMENSION (*) :: y
    namelist /mynml/ y     ! { dg-error "is not allowed" }
    write (6, mynml)
  end subroutine foo
end program assumed_size_nml