! { dg-do compile }
! PR41909 ICE with "call foo" in "program foo"
program test ! { dg-error "Global name" }
  implicit none
  call test()  ! { dg-error "" }
contains
  subroutine one(a)
    real, dimension(:,:), intent(inout), optional :: a
    call two(a)
  end subroutine one
end program test

