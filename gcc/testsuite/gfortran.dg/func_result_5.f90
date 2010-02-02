! { dg-do compile }
!
! PR fortran/42650
!
! Result type was not working
!

type(t) function func2() result(res)
  type t
    sequence
    integer :: i = 5
  end type t
  res%i = 2
end function func2
