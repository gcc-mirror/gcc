!{ dg-do compile }

! Check PR46371 is fixed.
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>

program pr46371
  type :: foo
    integer :: i = 0
  end type

  class(foo), allocatable :: o_foo[:]
  integer :: j

  select type(a => o_foo[2])  !{ dg-error "must not be coindexed" }
    type is(foo)
    j = a%i
  end select
end program pr46371

