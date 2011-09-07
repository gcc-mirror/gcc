! { dg-do link }
! { dg-additional-sources class_4a.f03 class_4b.f03 }
!
! Test the fix for PR41583, in which the different source files
! would generate the same 'vindex' for different class declared
! types.
!
! The test comprises class_4a, class_4b and class_4c.f03
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
  use m
  use m2
  type,extends(t) :: t3
  end type t3

  integer :: i
  class(t), allocatable :: a
  allocate(t3 :: a)
  select type(a)
    type is(t)
      i = 1
    type is(t2)
      i = 2
    type is(t3)
      i = 3
  end select
  print *, i
end

! { dg-final { cleanup-modules "m m2 m3" } }
