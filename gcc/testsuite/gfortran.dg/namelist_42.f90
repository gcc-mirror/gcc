! { dg-do run { target fd_truncate } }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
!
! PR fortran/34427
!
! Check that namelists and the real values Inf, NaN, Infinity
! properly coexist.
!
 PROGRAM TEST
  IMPLICIT NONE
  real , DIMENSION(11) ::foo 
  integer :: infinity
  NAMELIST /nl/ foo
  NAMELIST /nl/ infinity
  foo = -1.0
  infinity = -1

  open (10, status="scratch")
! Works:
  write (10,*) " &nl foo = 5, 5, 5, nan, infinity, infinity "
  write (10,*)
  write (10,*) "      = 1, /"
  rewind (10)
  READ (10, NML = nl)
  close (10)

  if(infinity /= 1) STOP 1
  if(any(foo(1:3) /= [5.0, 5.0, 5.0]) .or. .not.isnan(foo(4)) &
     .or. foo(5) <= huge(foo) .or. any(foo(6:11) /= -1.0)) &
    STOP 2
! Works too:
  foo = -1.0
  infinity = -1

  open (10, status="scratch")
  rewind (10)
  write (10,'(a)') "&nl foo = 5, 5, 5, nan, infinity, infinity"
  write (10,'(a)') "=1,/"
  rewind (10)
  READ (10, NML = nl)
  CLOSE (10)

  if(infinity /= 1) STOP 3
  if(any(foo(1:3) /= [5.0, 5.0, 5.0]) .or. .not.isnan(foo(4)) &
     .or. foo(5) <= huge(foo) .or. any(foo(6:11) /= -1.0)) &
    STOP 4
 END PROGRAM TEST
