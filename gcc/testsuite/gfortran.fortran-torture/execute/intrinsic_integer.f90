! Program to test the real->integer conversion routines.
program intrinsic_integer
  implicit none

  call test (0.0, (/0, 0, 0, 0/))
  call test (0.3, (/0, 1, 0, 0/))
  call test (0.7, (/0, 1, 0, 1/))
  call test (-0.3, (/-1, 0, 0, 0/))
  call test (-0.7, (/-1, 0, 0, -1/))
contains
subroutine test(val, res)
  real :: val
  integer, dimension(4) :: res

  if ((floor(val) .ne. res(1)) .or. (ceiling(val) .ne. res(2)) &
      .or. (int(val) .ne. res(3)) .or. (nint(val) .ne. res(4))) call abort
end subroutine
end program
