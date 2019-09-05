! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-add-options ieee }
!
! PR fortran/34319
!
! Check support of INF/NaN for I/O.
!
program main
  implicit none
  real :: r
  complex :: z
  character(len=30) :: str

  str = "nan"
  read(str,*) r
  if (.not.isnan(r)) STOP 1
  str = "(nan,4.0)"
  read(str,*) z
  if (.not.isnan(real(z)) .or. aimag(z) /= 4.0) STOP 2
  str = "(7.0,nan)"
  read(str,*) z
  if (.not.isnan(aimag(z)) .or. real(z) /= 7.0) STOP 3

  str = "inFinity"
  read(str,*) r
  if (r <= huge(r)) STOP 4
  str = "(+inFinity,4.0)"
  read(str,*) z
  if ((real(z) <= huge(r)) .or. aimag(z) /= 4.0) STOP 5
  str = "(7.0,-inFinity)"
  read(str,*) z
  if ((aimag(z) >= -huge(r)) .or. real(z) /= 7.0) STOP 6

  str = "inf"
  read(str,*) r
  if (r <= huge(r)) STOP 7
  str = "(+inf,4.0)"
  read(str,*) z
  if ((real(z) <= huge(r)) .or. aimag(z) /= 4.0) STOP 8
  str = "(7.0,-inf)"
  read(str,*) z
  if ((aimag(z) >= -huge(r)) .or. real(z) /= 7.0) STOP 9

end program main
