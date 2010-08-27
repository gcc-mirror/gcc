! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/33197
!
! Check implementation of L2 norm (Euclidean vector norm)
!
implicit none

print *, norm2([1, 2]) ! { dg-error "must be REAL" }
print *, norm2([cmplx(1.0,2.0)]) ! { dg-error "must be REAL" }
print *, norm2(1.0) ! { dg-error "must be an array" }
print *, norm2([1.0, 2.0], dim=2) ! { dg-error "not a valid dimension index" }
end
