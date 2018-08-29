! { dg-do run }
! { dg-options "-fbounds-check" }
! PR fortran/27524
    integer :: res(1)
    res = F()
    if (res(1) /= 1) STOP 1
    contains
      function F()
        integer :: F(1)
        f = 1
      end function F
    end
