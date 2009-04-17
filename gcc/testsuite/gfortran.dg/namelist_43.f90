! { dg-do run { target fd_truncate } }
! { dg-options "-mieee" { target alpha*-*-* sh*-*-* } }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
!
! PR fortran/34427
!
! Check that namelists and the real values Inf, NaN, Infinity
! properly coexist with interceding line ends and spaces.
!
PROGRAM TEST
  IMPLICIT NONE
  real , DIMENSION(10) ::foo 
  integer :: infinity
  integer :: numb
  NAMELIST /nl/ foo
  NAMELIST /nl/ infinity
  foo = -1.0
  infinity = -1

  open (10, status="scratch")

  write (10,'(a)') " &nl foo(1:6) = 5, 5, 5, nan, infinity"
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)') "infinity"
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)') "         "
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)')
  write (10,'(a)') "=1/"
  rewind (10)
  READ (10, NML = nl)
  CLOSE (10)
  if(infinity /= 1) call abort
  if(any(foo(1:3) /= [5.0, 5.0, 5.0]) .or. .not.isnan(foo(4)) &
     .or. (foo(5) <= huge(foo)) .or. any(foo(6:10) /= -1.0)) &
    call abort
END PROGRAM TEST
