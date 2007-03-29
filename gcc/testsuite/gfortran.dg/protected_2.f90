! { dg-do run }
! { dg-options "-std=f2003 -fall-intrinsics" }
! PR fortran/23994
!
! Test PROTECTED attribute. Within the module everything is allowed.
! Outside (use-associated): For pointers, their association status
! may not be changed. For nonpointers, their value may not be changed.
!
! Test of a valid code

module protmod
  implicit none
  integer, protected          :: a
  integer, protected, target  :: at
  integer, protected, pointer :: ap
contains
  subroutine setValue()
    a = 43
    ap => null()
    nullify(ap)
    ap => at
    ap = 3
    allocate(ap)
    ap = 73
    call increment(a,ap,at)
    if(a /= 44 .or. ap /= 74 .or. at /= 4) call abort()
  end subroutine setValue
  subroutine increment(a1,a2,a3)
    integer, intent(inout) :: a1, a2, a3
    a1 = a1 + 1
    a2 = a2 + 1
    a3 = a3 + 1
  end subroutine increment
end module protmod

program main
  use protmod
  implicit none
  call setValue()
  if(a /= 44 .or. ap /= 74 .or. at /= 4) call abort()
  call plus5(ap)
  if(a /= 44 .or. ap /= 79 .or. at /= 4) call abort()
  call checkVal(a,ap,at)
contains
  subroutine plus5(j)
    integer, intent(inout) :: j
    j = j + 5
  end subroutine plus5
  subroutine checkVal(x,y,z)
    integer, intent(in) :: x, y, z
    if(a /= 44 .or. ap /= 79 .or. at /= 4) call abort()
  end subroutine
end program main

! { dg-final { cleanup-modules "protmod" } }
