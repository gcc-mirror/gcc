! { dg-do compile }
! PR Fortran/83093
! Contributed by Gerhard Steinmetz  <gscfq at t-online dot de>
program p
   integer, parameter :: n(2) = [1,2]
   real :: x = 2
   character(:), allocatable :: z, zz, zzz
   character(:), allocatable :: y, yy
   allocate (character(a) :: z)     ! { dg-error "Scalar INTEGER expression" }
   allocate (character(x) :: zz)    ! { dg-error "Scalar INTEGER expression" }
   allocate (character((1.0)) :: z) ! { dg-error "Scalar INTEGER expression" }
   allocate (character(y) :: y)     ! { dg-error "Scalar INTEGER expression" }
   allocate (character(n(1:2)) :: y)! { dg-error "Scalar INTEGER expression" }
end
