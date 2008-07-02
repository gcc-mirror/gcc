! { dg-do compile }
!
! checking invalid code for PROCEDURE POINTERS
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

PROCEDURE(REAL), POINTER :: ptr
PROCEDURE(REAL), SAVE    :: noptr    ! { dg-error "attribute conflicts with" }

ptr => cos(4.0)        ! { dg-error "Invalid character" }

ALLOCATE(ptr)          ! { dg-error "must be ALLOCATABLE" }

end
