! { dg-do compile }
!
! checking invalid code for PROCEDURE POINTERS
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

PROCEDURE(REAL), POINTER :: ptr
PROCEDURE(REAL), SAVE    :: noptr    ! { dg-error "attribute conflicts with" }
REAL :: x

ptr => cos(4.0)        ! { dg-error "Invalid procedure pointer assignment" }
ptr => x               ! { dg-error "Invalid procedure pointer assignment" }
ptr => sin(x)          ! { dg-error "Invalid procedure pointer assignment" }

ALLOCATE(ptr)          ! { dg-error "must be ALLOCATABLE" }

end
