! { dg-do compile }
! PR 91556 - check that multiple errors are emitted for type mismatch
! (and that the check is also done in contained procedures).

program main
  real :: a
  call foo(a) ! { dg-error "Type mismatch" }
contains
  subroutine bar
    integer :: b
    complex :: c
    call foo(b) ! { dg-error "Type mismatch" }
    call foo(c) ! { dg-error "Type mismatch" }
  end subroutine bar
end program main
