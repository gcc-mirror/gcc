! { dg-do run }
!
! PR fortran/55603
! Check that the allocatable result is properly freed after use.
!
! Contributed by Damian Rouson <damian@sourceryinstitute.org>

  type foo
  end type
  type(foo) a
  a = bar()
contains
  function bar()
    type(foo), allocatable :: bar
    allocate(bar)
  end function
end
