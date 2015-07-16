! { dg-do compile }
! { dg-options "-Wuninitialized" }
! PR fortran/66545
!
program foo
   implicit none
   call p1
   call q1
end program foo

subroutine p1
   complex :: c5
   complex :: c6
   c5 = (c5)      ! { dg-warning "used uninitialized in this" }
   c6 = c6        ! { dg-warning "used uninitialized in this" }
end subroutine p1

subroutine q1
   real :: r5
   real :: r6
   r5 = (r5)   ! { dg-warning "used uninitialized in this" }
   r6 = r6     ! { dg-warning "used uninitialized in this" }
end subroutine q1
