! { dg-do compile }
! 
! PR/fortran 38432
! DO-loop compile-time checks
!
implicit none
integer :: i
real :: r
do i = 1, 0 ! { dg-warning "executed zero times" }
end do

do i = 1, -1, 1 ! { dg-warning "executed zero times" }
end do

do i = 1, 2, -1 ! { dg-warning "executed zero times" }
end do

do i = 1, 2, 0 ! { dg-error "cannot be zero" }
end do

do r = 1, 0 ! { dg-warning "must be integer|executed zero times" }
end do

do r = 1, -1, 1 ! { dg-warning "must be integer|executed zero times" }
end do

do r = 1, 2, -1 ! { dg-warning "must be integer|executed zero times" }
end do

do r = 1, 2, 0 ! { dg-error "must be integer|cannot be zero" }
end do
end
