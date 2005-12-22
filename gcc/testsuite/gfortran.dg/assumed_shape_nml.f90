! { dg-do compile }
! One of two tests for the fix of PR23152 - There used to be
! no warning for assumed shape arrays in namelists.
!
! Conributed by Paul Thomas  <pault@gcc.gnu.org>
!
program assumed_shape_nml
  real, dimension (10) :: z
  z = 42.0
  call foo (z)
contains
  subroutine foo (y)
    real, DIMENSION (1:) :: y
    namelist /mynml/ y     ! { dg-warning "is an extension" }
    write (*, mynml)
  end subroutine foo
end program assumed_shape_nml
