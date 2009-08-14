! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-options "-fno-range-check -mieee" { target alpha*-*-* sh*-*-* } }
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
  if (.not.isnan(r)) call abort()
  str = "(nan,4.0)"
  read(str,*) z
  if (.not.isnan(real(z)) .or. aimag(z) /= 4.0) call abort()
  str = "(7.0,nan)"
  read(str,*) z
  if (.not.isnan(aimag(z)) .or. real(z) /= 7.0) call abort()

  str = "inFinity"
  read(str,*) r
  if (r <= huge(r)) call abort()
  str = "(+inFinity,4.0)"
  read(str,*) z
  if ((real(z) <= huge(r)) .or. aimag(z) /= 4.0) call abort()
  str = "(7.0,-inFinity)"
  read(str,*) z
  if ((aimag(z) >= -huge(r)) .or. real(z) /= 7.0) call abort()

  str = "inf"
  read(str,*) r
  if (r <= huge(r)) call abort()
  str = "(+inf,4.0)"
  read(str,*) z
  if ((real(z) <= huge(r)) .or. aimag(z) /= 4.0) call abort()
  str = "(7.0,-inf)"
  read(str,*) z
  if ((aimag(z) >= -huge(r)) .or. real(z) /= 7.0) call abort()

end program main
