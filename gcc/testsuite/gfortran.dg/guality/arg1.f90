! { dg-do run }
! { dg-options "-g" }
  integer :: a(10), b(12)
  call sub (a, 10)
  call sub (b, 12)
  write (*,*) a, b
end

subroutine sub (a, n)
  integer :: a(n), n
  do i = 1, n
    a(i) = i
  end do
  write (*,*) a	! { dg-final { gdb-test 14 "a(10)" "10" } }
end subroutine
