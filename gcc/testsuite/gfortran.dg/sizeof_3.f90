! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/56650
! PR fortran/36437
!
module m
  use iso_c_binding, only: c_sizeof, c_int
  implicit none

  integer(c_int), bind(C) :: MPI_Status_C_obj
  integer,parameter :: MPI_STATUS_SIZE = c_sizeof(MPI_Status_C_obj)
end module m

module m2
  use iso_c_binding, only: c_sizeof, c_int
  implicit none

  integer(c_int), bind(C) :: MPI_Status_C_obj2
  integer,parameter :: MPI_STATUS_SIZE2 &
                    = c_sizeof(MPI_Status_C_obj2)*8/bit_size(0)
end module m2

subroutine test()
  use m
  use m2
  integer :: m1test, m2test
  m1test = MPI_STATUS_SIZE
  m2test = MPI_STATUS_SIZE2
end subroutine test

type t
  character(len=20) :: str
end type t
type(t):: x(5)
integer :: iii, jjj
iii = sizeof (x)       ! 5*20 (whole size in bytes)
jjj = storage_size (x) ! 8*20 (element size in bits)
end

! { dg-final { scan-tree-dump-times "m1test = 4;" 1 "original" } }
! { dg-final { scan-tree-dump-times "m2test = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "iii = 100;" 1 "original" } }
! { dg-final { scan-tree-dump-times "jjj = 160;" 1 "original" } }
