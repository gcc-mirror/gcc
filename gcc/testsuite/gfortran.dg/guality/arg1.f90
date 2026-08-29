! { dg-do run }
! { dg-options "-fno-shrink-wrap -g" }
  integer :: a(10), b(12)
  call sub (a, 10)
  call sub (b, 12)
  write (*,*) a, b
end

subroutine sub (a, n)
  integer :: a(n), n
  integer, volatile :: v
  v = 0
  do i = 1, n
    a(i) = i
  end do
  write (*,*) a
  v = v + 1	! { dg-final { gdb-test 17 "a(10)" "10" } }
end subroutine
