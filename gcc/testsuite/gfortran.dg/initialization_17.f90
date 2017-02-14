! { dg-do compile }
!
! PR fortran/34514
!
! Initialization and typespec changes.
!
integer :: n = 5, m = 7
parameter (n = 42) ! { dg-error "Initializing already initialized variable" }
dimension :: m(3)  ! { dg-error "after its initialization" } 
end
