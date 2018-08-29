! { dg-do compile }
! { dg-options "-std=f95" }
! { dg-shouldfail "Fortran 2003 feature with -std=f95" }
!
! Pointer intent test
! PR fortran/29624
!
! Fortran 2003 features in Fortran 95
program test
 implicit none
 integer, pointer :: p
 allocate(p)
 p = 33
 call a(p) ! { dg-error "Type mismatch in argument" }
contains
  subroutine a(p)! { dg-error "has no IMPLICIT type" }
    integer, pointer,intent(in) :: p ! { dg-error "POINTER attribute conflicts with INTENT attribute" }
  end subroutine
end program
