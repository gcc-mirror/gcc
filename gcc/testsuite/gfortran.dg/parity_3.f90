! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/33197
!
! Check implementation of PARITY
!
implicit none
print *, parity([.true.]) ! { dg-error "has no IMPLICIT type" }
end
