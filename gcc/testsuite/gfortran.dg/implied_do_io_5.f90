! { dg-do  run }
! { dg-additional-options "-ffrontend-optimize" }
! PR fortran/85387 - incorrect output
! Original test case by Vittorio Zecca
program main
  real :: efg_pw(2,2)
  character (len=80) :: c1, c2
  efg_pw(1,1)=1
  efg_pw(2,1)=2
  efg_pw(1,2)=3
  efg_pw(2,2)=4
  write (unit=c1,fmt='(3F12.5)') ((efg_pw(i, j), i=1, j), j=1, 2)
  write (unit=c2,fmt='(3F12.5)') 1.0, 3.0, 4.0
  if (c1 /= c2) stop 1
end
