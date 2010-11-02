! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/45170
!
! Character deferred type parameter
!
implicit none
character(len=:), allocatable :: str(:) ! { dg-error "Fortran 2003: deferred type parameter" }

character(len=4) :: str2*(:) ! { dg-error "Fortran 2003: deferred type parameter" }
end
