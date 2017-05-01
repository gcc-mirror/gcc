! { dg-do compile }
!
! PR 80392: [5/6/7 Regression] [OOP] ICE with allocatable polymorphic function result in a procedure pointer component
!
! Contributed by <zed.three@gmail.com>

module mwe

  implicit none

  type :: MyType
     procedure(my_op), nopass, pointer :: op
  end type

contains

  function my_op() result(foo)
    class(MyType), allocatable :: foo
  end function

end module
