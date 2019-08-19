! { dg-do compile }
! PR 91443 - this was not caught.
module x
contains
  subroutine a
    call foo(1) ! { dg-error "Type mismatch in argument" }
  end subroutine a
end module x

subroutine foo(a)
  real :: a
  print *,a
end subroutine foo

program main
  use x
  call a
end program main
