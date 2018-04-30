! { dg-do compile }

function foo(a, b, c, n)
  integer :: a(n), b(n), c(n), n, i, foo
  foo = 0
  do i = 1, n
    if (a(i) .eq. b(i)) then
      foo = 1
    else if (a(i) .eq. c(i)) then
      foo = 2
    end if
  end do
end function foo
