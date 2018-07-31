! { dg-do run }
!
! Test the fix for PR83318.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
type :: any_vector
  class(*), allocatable :: v(:)
end type
type(any_vector) :: x, y

! This did not work correctly
  x%v = ['foo','bar']
  call foo (x, 1)

! This was reported as not working correctly but was OK before the above was fixed
  y = x
  call foo (y, 2)

  x%v = [1_4,2_4]
  call foo (x, 3)

  y = x
  call foo (y, 4)

contains

  subroutine foo (arg, n)
    type (any_vector) :: arg
    integer :: n
    select type (v => arg%v)
        type is (character(*))
           if (any (v .ne. ["foo","bar"])) stop n
        type is (integer(4))
           if (any (v .ne. [1_4,2_4])) stop n
    end select
  end subroutine
end
