! { dg-do compile }

! PR fortran/37411
! This used to cause an ICE because of a missing array spec after interface
! mapping.

! Contributed by Kristjan Jonasson <jonasson@hi.is>

MODULE B1
CONTAINS
  subroutine sub()
    integer :: x(1)
    character(3) :: st
    st = fun(x)
  end subroutine sub

  function fun(x) result(st)
    integer, intent(in) :: x(1)
    character(lenf(x)) :: st
    st = 'abc'
  end function fun

  pure integer function lenf(x)
    integer, intent(in) :: x(1)
    lenf = x(1)
  end function lenf
END MODULE B1

! { dg-final { cleanup-modules "B1" } }
