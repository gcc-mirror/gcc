! { dg-do compile }
! PR fortran/30799
! Inconsistent handling of bad (invalid) LOGICAL kinds
! Reporter: Harald Anlauf <anlauf@gmx.de>
! Testcase altered by Steven G. Kargl
program gfcbug57
  implicit none
  !
  ! These are logical kinds known by gfortran and many other compilers:
  !
  print *, kind (.true._1) ! This prints "1"
  print *, kind (.true._2) ! This prints "2"
  print *, kind (.true._4) ! This prints "4"
  print *, kind (.true._8) ! This prints "8"
  !
  ! These are very strange (read: bad (invalid?)) logical kinds,
  ! handled inconsistently by gfortran (there's no logical(kind=0) etc.)
  !
  print *, kind (.true._0)   ! { dg-error "kind for logical constant" }
  print *, kind (.true._3)   ! { dg-error "kind for logical constant" }
  print *, kind (.true._123) ! { dg-error "kind for logical constant" }
  !
  ! Here gfortran bails out with a runtime error:
  !
  print *, .true._3   ! { dg-error "kind for logical constant" }
end program gfcbug57
