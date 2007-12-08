! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/34342
!
! Diagnose BOZ literal for non-integer variables in
! a DATA statement. And outside DATA statements.
!
real :: r
integer :: i
r = real(z'FFFF') ! { dg-error "outside a DATA statement" }
i = int(z'4455')  ! { dg-error "outside a DATA statement" }
r = z'FFFF' + 1.0 ! { dg-error "outside a DATA statement" }
i = z'4455' + 1   ! { dg-error "outside a DATA statement" }
end
