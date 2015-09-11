! { dg-do compile }
! { dg-options "-Wall" }
! PR fortran/66545
!
subroutine p
   complex, parameter :: c1 = (c1) ! { dg-error "before its definition" }
   complex, parameter :: c2 = c2   ! { dg-error "before its definition" }
   complex :: c3 = (c3)            ! { dg-error "has not been declared or is a variable" }
   complex :: c4 = c4              ! { dg-error "has not been declared or is a variable" }
end subroutine p

subroutine q
   real, parameter :: r1 = (r1)  ! { dg-error "before its definition" }
   real, parameter :: r2 = r2    ! { dg-error "before its definition" }
   real :: r3 = (r3)             ! { dg-error "has not been declared or is a variable" }
   real :: r4 = r4               ! { dg-error "has not been declared or is a variable" }
end subroutine q
