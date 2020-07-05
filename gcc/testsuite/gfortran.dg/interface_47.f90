! PR fortran/27318
! { dg-do compile }
! This tests for mismatch between the interface for a global
! procedure and the procedure itself.

module test
implicit none
interface
   subroutine hello(n) ! { dg-warning "INTENT mismatch" }
     integer :: n
   end subroutine hello
end interface
end module test

subroutine hello(n)  ! { dg-warning "INTENT mismatch" }
  integer, intent(in) :: n
  integer :: i
  do i = 1,n; print *, 'hello'; end do
end subroutine hello
