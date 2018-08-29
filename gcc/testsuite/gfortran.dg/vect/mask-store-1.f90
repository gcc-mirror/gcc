subroutine foo(a, b, x, n)
  real(kind=8) :: a(n), b(n), tmp
  logical(kind=1) :: x
  integer(kind=4) :: i, n
  do i = 1, n
     if (x) then
        a(i) = b(i)
     end if
     b(i) = b(i) + 10
  end do
end subroutine
