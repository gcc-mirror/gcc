! { dg-do compile }
! Tests the fix for PR34429 in which function charlens that were
! USE associated would cause an error.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  integer, parameter :: strlen = 5
end module m

character(strlen) function test()
  use m
  test = 'A'
end function test

  interface
    character(strlen) function test()
      use m
    end function test
  end interface
  print *, test()
end
! { dg-final { cleanup-modules "m" } }
