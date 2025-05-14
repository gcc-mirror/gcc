! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/33197
!
! Check implementation of PARITY
!
implicit none
print *, parity([real ::]) ! { dg-error "must be LOGICAL" }
print *, parity([integer ::]) ! { dg-error "must be LOGICAL" }
print *, parity([logical ::])
print *, parity(.true.) ! { dg-error "must be an array" }
end
