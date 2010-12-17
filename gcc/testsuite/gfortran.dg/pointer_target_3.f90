! { dg-do compile }
!
! TARGET actual to POINTER dummy with INTENT(IN)
!
program test
  implicit none
  integer, target :: a
  integer :: b
  call foo(a) ! OK
  call foo(b) ! { dg-error "must be a pointer" }
  call bar(a) ! { dg-error "must be a pointer" }
  call bar(b) ! { dg-error "must be a pointer" }
contains
  subroutine foo(p)
    integer, pointer, intent(in) :: p
  end subroutine foo
  subroutine bar(p)
    integer, pointer :: p
  end subroutine bar
end program test
