! { dg-do run }
!
! PR fortran/34404
!
! Contributed by Joost VandeVondele.
!
implicit none
complex :: x
character(len=80) :: t="(1.0E-7,4.0E-3)"
read(t,*) x
if (real(x) /= 1.0e-7 .or. aimag(x)/=4.0e-3) STOP 1
END
