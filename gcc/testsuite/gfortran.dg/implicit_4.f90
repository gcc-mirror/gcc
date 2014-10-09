! { dg-do compile }
! Verify error diagnosis for invalid combinations of IMPLICIT statements
IMPLICIT NONE
IMPLICIT NONE ! { dg-error "Duplicate" }
END

SUBROUTINE a
IMPLICIT REAL(b-j)
implicit none      ! { dg-error "IMPLICIT NONE .type. statement at .1. following an IMPLICIT statement" }
END SUBROUTINE a

subroutine b
implicit none
implicit real(g-k) ! { dg-error "IMPLICIT statement at .1. following an IMPLICIT NONE .type. statement" }
end subroutine b

subroutine c
implicit real(a-b)
implicit integer (b-c) ! { dg-error "already" }
implicit real(d-f), complex(f-g) ! { dg-error "already" }
end subroutine c
