! { dg-do compile }
!
! PR 57639: [OOP] ICE with polymorphism (and illegal code)
!
! Contributed by Walter Spector <w6ws@earthlink.net>

  implicit none

  class(*) :: t1, t2      ! { dg-error "must be dummy, allocatable or pointer" }

  print *, 'main: compare = ', compare (t1, t2)
  print *, SAME_TYPE_AS (t1, t2)

contains

 logical function compare (a, b)
    class(*), intent(in), allocatable :: a, b
  end function  
  
end
