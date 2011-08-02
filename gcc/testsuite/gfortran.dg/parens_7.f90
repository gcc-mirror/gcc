! { dg-do compile }
! PR34432 integer(kind=init_expression) function  is rejected
module m
  integer, parameter :: int_t = 4
end module m

program test
  print *, test4()
contains

integer(kind=(int_t)) function test4() ! This failed before patch
  use m
  test4 = 345
end function test4

 
end program test

! { dg-final { cleanup-modules "m" } }
