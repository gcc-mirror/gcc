! { dg-do compile }
! PR fortran/91471
! Code contributed by Sameeran Joshi <SameeranJayant dot Joshi at amd dot com>
!
! This invalid code (x(1) is referenced, but never set) caused an ICE due
! to hitting a gfc_internal_error() in primary.c (gfc_variable_attr).  The
! fix is to remove that gfc_internal_error().
! 
program dynamic
   implicit none
   integer, dimension(:), allocatable :: x
   allocate(x(1))
   stop x(1)
end program dynamic
