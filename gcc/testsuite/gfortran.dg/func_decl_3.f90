! { dg-do compile }
! Tests the fix for PR24325 in which the lack of any declaration
! that foo is a function or even a procedure was not detected.
!
! Contributed by Jakub Jelinek <jakub@gcc.gnu.org>
!
  integer foo
  call test
contains
  subroutine test
    integer :: i
    i = foo () ! { dg-error "is not a function" }
  end subroutine test
end

