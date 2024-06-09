! { dg-do run }
! { dg-options "-fcheck=bounds" }
! { dg-shouldfail "above upper bound" }
!
! PR fortran/29800
!
! Contributed by Joost VandeVondele
!

TYPE data
  INTEGER :: x(10)
END TYPE
TYPE data_areas
  TYPE(data) :: y(10)
END TYPE

TYPE(data_areas) :: z(10)

integer, volatile :: i,j,k
i=1 ; j=1 ; k=11

z(i)%y(j)%x(k)=0

END

! { dg-output "At line 22 of file .*bounds_check_17.f90.*Fortran runtime error: Index '11' of dimension 1 of array 'z\.\.\.%x' above upper bound of 10" }
