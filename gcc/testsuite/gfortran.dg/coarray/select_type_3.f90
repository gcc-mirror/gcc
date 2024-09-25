!{ dg-do run }

! Check pr56496 is fixed.
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>

program pr56496

  class(*), allocatable :: a[:]

  allocate(integer :: a[*])
  select type(a)
    type is (integer)
      a= 5
      if (a /= 5) stop 1
  end select

  select type(a)
    type is (integer)
      if (a /= 5) stop 2
  end select

end

