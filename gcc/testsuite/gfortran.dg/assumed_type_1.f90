! { dg-do compile }
!
! PR fortran/48820
!
! Test TYPE(*)
!
! Based on a contributed test case by Walter Spector
!
module mpi_interface
  implicit none

  interface mpi_send
    subroutine MPI_Send (buf, count, datatype, dest, tag, comm, ierr)
      type(*), intent(in) :: buf(:)
      integer, intent(in) :: count
      integer, intent(in) :: datatype
      integer, intent(in) :: dest
      integer, intent(in) :: tag
      integer, intent(in) :: comm
      integer, intent(out):: ierr
    end subroutine
  end interface

  interface mpi_send2
    subroutine MPI_Send2 (buf, count, datatype, dest, tag, comm, ierr)
      type(*), intent(in) :: buf(*)
      integer, intent(in) :: count
      integer, intent(in) :: datatype
      integer, intent(in) :: dest
      integer, intent(in) :: tag
      integer, intent(in) :: comm
      integer, intent(out):: ierr
    end subroutine
  end interface

end module

use mpi_interface
  real :: a(3)
  integer :: b(3)
  call foo(a)
  call foo(b)
  call foo(a(1:2))
  call foo(b(1:2))
  call MPI_Send(a, 1, 1,1,1,j,i)
  call MPI_Send(b, 1, 1,1,1,j,i)
  call MPI_Send2(a, 1, 1,1,1,j,i)
  call MPI_Send2(b, 1, 1,1,1,j,i)
contains
    subroutine foo(x)
    type(*):: x(*)
    call MPI_Send2(x, 1, 1,1,1,j,i)
  end
end

! { dg-final { cleanup-modules "mpi_interface" } }
