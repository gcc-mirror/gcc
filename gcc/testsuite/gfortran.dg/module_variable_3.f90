! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/97927
!
! Did ICE due to the in tree-nested.c due to {clobber}
!

module mpi2
 interface
   subroutine MPI_Allreduce(i)
     implicit none
     INTEGER, OPTIONAL, INTENT(OUT) :: i
   end subroutine MPI_Allreduce
 end interface
end module

module modmpi
  implicit none
  integer ierror  ! module variable = context NAMESPACE_DECL
end module

subroutine exxengy
  use modmpi
  use mpi2, only: mpi_allreduce
  implicit none

  ! intent(out) implies: ierror = {clobber}
  call mpi_allreduce(ierror)

contains
  subroutine zrho2
    return
  end subroutine
end subroutine

! { dg-final { scan-tree-dump "ierror = {CLOBBER};" "original" } }
