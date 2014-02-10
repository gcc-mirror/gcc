! { dg-do run }
! Test the fix for PR57522, in which the associate name had a
! 'span' of an INTEGER rather than that of 'mytype'.
!
! Contributed by A Briolat  <alan.briolat@gmail.com>
!
program test_associate
  type mytype
    integer :: a = 1, b = 2
  end type
  type(mytype) :: t(4), u(2,2)
  integer :: c(4)
  t%a = [0, 1, 2, 3]
  t%b = [4, 5, 6, 7]
  associate (a => t%a)
! Test 'a' is OK on lhs and/or rhs of assignments
    c = a - 1
    if (any (c .ne. [-1,0,1,2])) call abort
    a = a + 1
    if (any (a .ne. [1,2,3,4])) call abort
    a = t%b
    if (any (a .ne. t%b)) call abort
! Test 'a' is OK as an actual argument
    c = foo(a)
    if (any (c .ne. t%b + 10)) call abort
  end associate
! Make sure that the fix works for multi-dimensional arrays...
  associate (a => u%a)
    if (any (a .ne. reshape ([1,1,1,1],[2,2]))) call abort
  end associate
! ...and sections
  associate (a => t(2:3)%b)
    if (any (a .ne. [5,6])) call abort
  end associate
contains
  function foo(arg) result(res)
    integer :: arg(4), res(4)
    res = arg + 10
  end function
end program
