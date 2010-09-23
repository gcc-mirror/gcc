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

module good1
  implicit none
  integer              :: a
  integer              :: b,c
  protected            :: c
  equivalence (a,c) ! { dg-error "Either all or none of the objects in the EQUIVALENCE" }
end module good1


module bad1
  implicit none
  integer, protected   :: a
  integer              :: b,c
  protected            :: c
  equivalence (a,b) ! { dg-error "Either all or none of the objects in the EQUIVALENCE" }
end module bad1

module bad2
  implicit none
  integer, protected   :: a
  integer              :: b,c,d
  protected            :: c
  common /one/ a,b  ! { dg-error "PROTECTED attribute conflicts with COMMON" }
  common /two/ c,d  ! { dg-error "PROTECTED attribute conflicts with COMMON" }
end module bad2

module good2
  implicit none
  type myT
     integer :: j
     integer, pointer :: p
     real, allocatable, dimension(:) :: array
  end type myT
  type(myT), save :: t
  protected :: t
end module good2

program main
  use good2
  implicit none
  t%j = 15             ! { dg-error "variable definition context" }
  nullify(t%p)         ! { dg-error "pointer association context" }
  allocate(t%array(15))! { dg-error "variable definition context" }
end program main

! { dg-final { cleanup-modules "good1 good2 bad1 bad2" } }
