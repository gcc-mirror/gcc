! { dg-do compile }
! { dg-options "-fallow-argument-mismatch" }
! PR 91556 - check that only a single warning iw emitted for type
! mismatch (and that the check is also done in contained procedures).

program main
  real :: a
  call foo(a) ! { dg-warning "Type mismatch" }
contains
  subroutine bar
    integer :: b
    complex :: c
    call foo(b) ! { dg-warning "Type mismatch" }
    call foo(c)
  end subroutine bar
end program main
