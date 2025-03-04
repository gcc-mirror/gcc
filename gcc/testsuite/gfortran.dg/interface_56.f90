! { dg-do run }
! PR 119074 - the code is legal, but it makes sense to warn anyway.
! { dg-additional-options "-Wall" }

program memain
  external i1, i2
  integer i1, i2
  call foo (i1,1)
  call foo (i2,2)
end program memain

integer function i1(n)
  i1 = n + 1
end function i1

integer function i2(n,m)
  i2 = n + m + 1
end function i2

subroutine foo(f,n)
  integer, external :: f
  integer :: n
  integer :: s
  if (n == 1) then
     s = f(1)   ! { dg-warning "Different argument lists" }
     if (s /= 2) error stop
  end if
  if (n == 2) then
     s = f(2,3)  ! { dg-warning "Different argument lists" }
     if (s /= 6) error stop
  end if
end subroutine foo
