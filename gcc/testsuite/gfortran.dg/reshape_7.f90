! { dg-do compile }
! PR fortran/66380
!
subroutine p0
   integer, parameter :: sh(2) = [2, 3]
   integer, parameter :: &
   & a(2,2) = reshape([1, 2, 3, 4], sh)   ! { dg-error "not enough elements" }
   if (a(1,1) /= 0) STOP 1
end subroutine p0


subroutine p1
   integer, parameter :: sh(2) = [2, 1]
   integer, parameter :: &
   &  a(2,2) = reshape([1, 2, 3, 4], sh)  ! { dg-error "Different shape" }
   if (a(1,1) /= 0) STOP 2
end subroutine p1
