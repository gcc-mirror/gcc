! { dg-do compile }
!
! PR fortran/34254
!
! The character-kind parameter was not accepted.
!
module m
  integer, parameter :: char_t = kind('a')
end module m

character(1,char_t) function test1()
  use m
  test1 = 'A'
end function test1

character(len=1,kind=char_t) function test2()
  use m
  test2 = 'A'
end function test2

character(kind=char_t,len=1) function test3()
  use m
  test3 = 'A'
end function test3

character(1,kind=char_t) function test4()
  use m
  test4 = 'A'
end function test4
