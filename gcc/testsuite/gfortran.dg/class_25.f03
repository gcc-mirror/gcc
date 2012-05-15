! { dg-do run }
!
! PR [OOP] Compile-time errors on typed allocation and pointer function result assignment
!
! Contributed by Damian Rouson <damian@rouson.net>

module m

  implicit none

  type foo 
  end type

  type ,extends(foo) :: bar
  end type

contains

  function new_bar()
    class(foo) ,pointer :: new_bar
    allocate(bar :: new_bar) 
  end function

end module

end 
