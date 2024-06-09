! { dg-do compile }
! PR fortran/93635
!
! Test that some attribute conflicts are properly diagnosed

program p
  implicit none
  character(len=:),allocatable :: r,s
  namelist /args/ r,s
  equivalence(r,s) ! { dg-error "EQUIVALENCE attribute conflicts with ALLOCATABLE" }
  allocate(character(len=1024) :: r)
end

subroutine sub (p, q)
  implicit none
  real, pointer, intent(inout) :: p(:), q(:)
  namelist /nml/ p,q
  equivalence(p,q) ! { dg-error "EQUIVALENCE attribute conflicts with DUMMY" }
end
