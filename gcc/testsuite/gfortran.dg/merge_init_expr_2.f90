! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/56649
! MERGE was not properly compile-time simplified
!
! Contributed by Bill Long
!
module m
  implicit none

  integer, parameter :: int32 = 4
  type MPI_Datatype
    integer :: i
  end type MPI_Datatype

  integer,private,parameter :: dik = kind(0)
  type(MPI_Datatype),parameter,private :: MPIx_I4 = MPI_Datatype( 1275069467)
  type(MPI_Datatype),parameter,private :: MPIx_I8 = MPI_Datatype( 1275070491)
  type(MPI_Datatype),parameter :: MPI_INTEGER = merge(MPIx_I4, MPIx_I8, &
                                                      dik==int32)
contains
  subroutine foo
    integer :: check1
    check1 = MPI_INTEGER%i
  end subroutine foo
end module m

module m2
  implicit none
  integer, parameter :: int32 = 4
  type MPI_Datatype
    integer :: i
  end type MPI_Datatype

  integer,private,parameter :: dik = kind(0)
  type(MPI_Datatype),parameter,private :: MPIx_I4 = MPI_Datatype( 1275069467)
  type(MPI_Datatype),parameter,private :: MPIx_I8 = MPI_Datatype( 1275070491)
  type(MPI_Datatype),parameter :: MPI_INTEGER(1) = merge([MPIx_I4], [MPIx_I8], &
                                                      [dik==int32])
contains
  subroutine foo
    logical :: check2
    check2 = MPI_INTEGER(1)%i == 1275069467
  end subroutine foo
end module m2


subroutine test
  character(len=3) :: one, two, three
  logical, parameter :: true = .true.
  three = merge (one, two, true)
end subroutine test

! { dg-final { scan-tree-dump-times "check1 = 1275069467;" 1 "original" } }
! { dg-final { scan-tree-dump-times "check2 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memmove ..void .. &three, .void .. &one, 3.;" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
