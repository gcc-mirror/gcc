! { dg-do compile }
! { dg-options "-std=f2003 -fall-intrinsics" }
! { dg-shouldfail "Invalid code" }
!
! Pointer intent test
! PR fortran/29624
!
! Valid program
program test
 implicit none
 type myT
    integer :: j = 5
    integer, pointer :: jp => null()
 end type myT
 integer, pointer :: p
 type(myT) :: t
 call a(p)
 call b(t)
contains
  subroutine a(p)
    integer, pointer,intent(in) :: p
    p => null(p)! { dg-error "Cannot assign to INTENT\\(IN\\) variable" }
    nullify(p)  ! { dg-error "Cannot assign to INTENT\\(IN\\) variable" }
    allocate(p) ! { dg-error "Cannot allocate INTENT\\(IN\\) variable" }
    call c(p)   ! { dg-error "is INTENT\\(IN\\) while interface specifies INTENT\\(INOUT\\)" }
    deallocate(p) ! { dg-error "Cannot deallocate INTENT\\(IN\\) variable" }
  end subroutine
  subroutine c(p)
    integer, pointer, intent(inout) :: p
    nullify(p)
  end subroutine c
  subroutine b(t)
    type(myT),intent(in) :: t
    t%jp = 5
    t%jp => null(t%jp)  ! { dg-error "Cannot assign to INTENT\\(IN\\) variable" }
    nullify(t%jp) ! { dg-error "Cannot assign to INTENT\\(IN\\) variable" }
    t%j = 7 ! { dg-error "Cannot assign to INTENT\\(IN\\) variable" }
    allocate(t%jp) ! { dg-error "Cannot allocate INTENT\\(IN\\) variable" }
    deallocate(t%jp) ! { dg-error "Cannot deallocate INTENT\\(IN\\) variable" }
  end subroutine b
end program
