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
    p => null(p)! { dg-error "pointer association context" }
    nullify(p)  ! { dg-error "pointer association context" }
    allocate(p) ! { dg-error "pointer association context" }
    call c(p)   ! { dg-error "pointer association context" }
    deallocate(p) ! { dg-error "pointer association context" }
  end subroutine
  subroutine c(p)
    integer, pointer, intent(inout) :: p
    nullify(p)
  end subroutine c
  subroutine b(t)
    type(myT),intent(in) :: t
    t%jp = 5
    t%jp => null(t%jp)  ! { dg-error "pointer association context" }
    nullify(t%jp) ! { dg-error "pointer association context" }
    t%j = 7 ! { dg-error "variable definition context" }
    allocate(t%jp) ! { dg-error "pointer association context" }
    deallocate(t%jp) ! { dg-error "pointer association context" }
  end subroutine b
end program
