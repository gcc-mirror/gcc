! { dg-do compile }
! { dg-shouldfail "Invalid Fortran 2003 code" }
! { dg-options "-std=f2003" }
! PR fortran/23994
!
! Test PROTECTED attribute. Within the module everything is allowed.
! Outside (use-associated): For pointers, their association status
! may not be changed. For nonpointers, their value may not be changed.
!
! Test of a invalid code

module protmod
  implicit none
  integer, Protected          :: a
  integer, protected, target  :: at
  integer, protected, pointer :: ap
end module protmod

program main
  use protmod
  implicit none
  a = 43       ! { dg-error "variable definition context" }
  ap => null() ! { dg-error "pointer association context" }
  nullify(ap)  ! { dg-error "pointer association context" }
  ap => &      ! { dg-error "pointer association context" }
       & at    ! { dg-error "Pointer assignment target has PROTECTED attribute" }
  ap = 3       ! OK
  allocate(ap) ! { dg-error "pointer association context" }
  ap = 73      ! OK
  call increment(a,at) ! { dg-error "variable definition context" }
  call pointer_assignments(ap) ! { dg-error "pointer association context" }
contains
  subroutine increment(a1,a3)
    integer, intent(inout) :: a1, a3
    a1 = a1 + 1
    a3 = a3 + 1
  end subroutine increment
  subroutine pointer_assignments(p)
    integer, pointer,intent (inout) :: p
    p => null()
  end subroutine pointer_assignments
end program main

module prot2
  implicit none
contains
  subroutine bar
    real, protected :: b ! { dg-error "only allowed in specification part of a module" }
  end subroutine bar
end module prot2
