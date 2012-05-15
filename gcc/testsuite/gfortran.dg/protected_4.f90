! { dg-do compile }
! { dg-shouldfail "Invalid Fortran 2003 code" }
! { dg-options "-std=f2003 -fall-intrinsics" }
! PR fortran/23994
!
! Test PROTECTED attribute. Within the module everything is allowed.
! Outside (use-associated): For pointers, their association status
! may not be changed. For nonpointers, their value may not be changed.
!
! Test of a invalid code

module protmod
  implicit none
  integer          :: a
  integer, target  :: at
  integer, pointer :: ap
  protected :: a, at, ap
end module protmod

program main
  use protmod
  implicit none
  integer   :: j 
  logical   :: asgnd
  protected :: j ! { dg-error "only allowed in specification part of a module" }
  a = 43       ! { dg-error "variable definition context" }
  ap => null() ! { dg-error "pointer association context" }
  nullify(ap)  ! { dg-error "pointer association context" }
  ap => at     ! { dg-error "pointer association context" }
  ap = 3       ! OK
  allocate(ap) ! { dg-error "pointer association context" }
  ap = 73      ! OK
  call increment(a,at) ! { dg-error "variable definition context" }
  call pointer_assignments(ap) ! { dg-error "pointer association context" }
  asgnd = pointer_check(ap)
contains
  subroutine increment(a1,a3)
    integer, intent(inout) :: a1, a3
    a1 = a1 + 1
    a3 = a3 + 1
  end subroutine increment
  subroutine pointer_assignments(p)
    integer, pointer,intent(out) :: p
    p => null()           
  end subroutine pointer_assignments
  function pointer_check(p)
    integer, pointer,intent(in) :: p
    logical :: pointer_check
    pointer_check = associated(p)
  end function pointer_check
end program main

module test
  real :: a
  protected :: test ! { dg-error "MODULE attribute conflicts with PROTECTED" }
end module test
