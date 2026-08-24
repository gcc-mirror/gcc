! { dg-do run }
! PR 49802
! character(len=n), value with a non-constant specified length (n is a
! dummy argument) was rejected with "must have constant length".  Verify
! that it compiles and that VALUE semantics hold.

program test
  implicit none
  call sub_char1_n ("abc", 3)
contains
  subroutine sub_char1_n (x, n)
    integer, intent(in)            :: n
    character(len=n), value :: x
    x(1:1) = "1"
    if (x(1:1) /= "1") error stop 23
  end subroutine sub_char1_n
end program test
