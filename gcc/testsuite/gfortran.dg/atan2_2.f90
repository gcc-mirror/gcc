! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/33197
!
! Check for Fortran 2008's ATAN(Y,X) - which is equivalent
! to Fortran 77's ATAN2(Y,X).
!
real(4)    :: r4
real(8)    :: r8
complex(4) :: c4
complex(8) :: c8

r4 = atan2(r4,r4)
r8 = atan2(r8,r8)

r4 = atan(r4,r4) ! { dg-error "Too many arguments in call to 'atan'" }
r8 = atan(r8,r8) ! { dg-error "Too many arguments in call to 'atan'" }

r4 = atan2(r4,r8) ! { dg-error "same type and kind" }
r4 = atan2(r8,r4) ! { dg-error "same type and kind" }

r4 = atan2(c4,r8) ! { dg-error "must be REAL" }
r4 = atan2(c8,r4) ! { dg-error "must be REAL" }
r4 = atan2(r4,c8) ! { dg-error "same type and kind" }
r4 = atan2(r8,c4) ! { dg-error "same type and kind" }

r4 = atan2(c4,c8) ! { dg-error "must be REAL" }
r4 = atan2(c8,c4) ! { dg-error "must be REAL" }
end
