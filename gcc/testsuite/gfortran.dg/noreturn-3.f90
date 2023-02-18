! { dg-do compile }
! { dg-additional-options "-Wuninitialized -Wmaybe-uninitialized" }

subroutine foo
implicit none
integer :: i
!GCC$ ATTRIBUTES noreturn :: mpi_abort
if (getpid() == 1) then
  call mpi_abort()
else
  i = 8
endif
if (i > 0) print *, i
end subroutine
