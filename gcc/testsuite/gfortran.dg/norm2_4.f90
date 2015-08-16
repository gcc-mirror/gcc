! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/33197
!
! Check implementation of L2 norm (Euclidean vector norm)
!
implicit none

print *, norm2([1.0, 2.0]) ! { dg-error "has no IMPLICIT type" }
end
