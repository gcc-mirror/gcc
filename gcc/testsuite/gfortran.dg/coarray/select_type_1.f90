!{ dg-do run }

! Check PR46371 is fixed.
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>

program pr46371
  type :: foo
    integer :: i = 0
  end type

  class(foo), allocatable :: o_foo[:]
  integer :: j

  allocate(foo :: o_foo[*])
  if (this_image() == 1) then

    select type(a => o_foo)
      type is(foo)
      j = a[1]%i
      a[1]%i = 3
    end select

    if (j /= 0) stop 1

    select type(o_foo)
      type is(foo)
      j = o_foo[1]%i
    end select

    if (o_foo[1]%i /= 3) stop 2
    if (j /= 3) stop 3
  end if
end program pr46371

