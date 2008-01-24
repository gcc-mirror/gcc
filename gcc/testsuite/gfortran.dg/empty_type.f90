! { dg-do compile }
! PR fortran/34202
! ICE on contruction of empty types
! Testcase contributed by Tobias Burnus

program bug4a
  implicit none
  type bug4
    ! Intentionally left empty
  end type bug4

  type compound
    type(bug4) b
  end type compound

  type(bug4), parameter :: f = bug4()
  type(compound), parameter :: g = compound(bug4())
end program bug4a

