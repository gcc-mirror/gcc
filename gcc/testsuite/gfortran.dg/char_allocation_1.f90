! PR fortran/31974
! { dg-do run }
  subroutine foo (n)
    integer :: n
    character (len = n) :: v(n)
    v = ''
    if (any (v /= '')) call abort
  end subroutine foo

  call foo(7)
  end
