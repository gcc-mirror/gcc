! { dg-do run }
! { dg-additional-options "-fcheck=all" }
!
! PR fortran/85781
!
! Co-contributed by G. Steinmetz 

  use iso_c_binding, only: c_char
  call s(c_char_'x', 1, 1)
  call s(c_char_'x', 1, 0)
  call s(c_char_'x', 0, -2)
contains
  subroutine s(x,m,n) bind(c)
    use iso_c_binding, only: c_char
    character(kind=c_char), value :: x
    call foo(x(m:n), m, n)
    if (n < m) then
      if (len(x(m:n)) /= 0) stop 1
      if (x(m:n) /= "") stop 2
    else if (n == 1) then
      if (len(x(m:n)) /= 1) stop 1
      if (x(m:n) /= "x") stop 2
    else
      stop 14
    end if
    call foo(x(1:1), 1, 1)
    call foo(x(1:0), 1, 0)
    call foo(x(2:1), 2, 1)
    call foo(x(0:-4), 0, -4)

    call foo(x(1:), 1, 1)
    call foo(x(2:), 2, 1)
    call foo(x(:1), 1, 1)
    call foo(x(:0), 1, 0)

    if (n == 1) call foo(x(m:), m, n)
    if (m == 1) call foo(x(:n), m, n)
  end
  subroutine foo(str, m, n)
    character(len=*) :: str
    if (n < m) then
      if (len(str) /= 0) stop 11
      if (str /= "") stop 12
    else if (n == 1) then
      if (len(str) /= 1) stop 13
      if (str /= "x") stop 14
    else
      stop 14
    end if
  end
end
