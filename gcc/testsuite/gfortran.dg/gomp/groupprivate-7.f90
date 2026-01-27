module m
implicit none
integer :: x
!$omp declare target local(x)
contains
subroutine f
integer, save :: y
integer  :: z
common /com/ z

! A variable that is a groupprivate variable or a device-local variable must
! not appear as a list item in a map clause.

!$omp groupprivate(y)
!$omp groupprivate(/com/)
!$omp target enter data map(x) ! { dg-error "'x' argument to MAP clause at .1. must not be a device-local variable, including GROUPPRIVATE" }
!$omp target enter data map(y) ! { dg-error "'y' argument to MAP clause at .1. must not be a device-local variable, including GROUPPRIVATE" }
!!$omp target enter data map(to : /com/) ! -> PR fortran/92730
end

end module
