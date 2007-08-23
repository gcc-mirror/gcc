! This testcase was miscompiled, because ts.cl
! in function bar was initially shared between both
! dummy arguments.  Although it was later unshared,
! all expressions which copied ts.cl from bar2
! before that used incorrectly bar1's length
! instead of bar2.
! { dg-do run }

subroutine foo (foo1, foo2)
  implicit none
  integer, intent(in) :: foo2
  character(*), intent(in) :: foo1(foo2)
end subroutine foo

subroutine bar (bar1, bar2)
  implicit none
  character(*), intent(in) :: bar1, bar2

  call foo ((/ bar2 /), 1)
end subroutine bar

program test
  character(80) :: str1
  character(5) :: str2

  str1 = 'String'
  str2 = 'Strng'
  call bar (str2, str1)
end program test
