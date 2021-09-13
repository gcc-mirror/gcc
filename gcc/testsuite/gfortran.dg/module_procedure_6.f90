! { dg-do run }
!
! Test the fix for the testcase in comment 24 of PR96320, which used to
! fail with the message: ‘set_user_defined’ must be a module procedure or
! an external procedure with an explicit interface at (1)
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module hole_interface
  type hole_t
    integer :: user_defined
    real :: hole_diameter
  contains
    procedure set_user_defined
    procedure set_diameter
  end type

  interface
    module subroutine set_diameter (this, diameter)
      class(hole_t) :: this
      real :: diameter
    end subroutine

    module subroutine set_user_defined(this, user_defined)
      class(hole_t) :: this
      integer :: user_defined
    end subroutine
  end interface

contains
  module procedure set_user_defined
    this%user_defined = user_defined
  end procedure

  module procedure set_diameter
    this%hole_diameter = diameter
    if (this%user_defined .lt. 0) then
      call this%set_user_defined (0)
    end if
  end procedure
end module

  use hole_interface ! Error was here
  type (hole_t) :: ht = hole_t (-1, 0.0)
  call ht%set_diameter(1.0)
  if ((ht%user_defined .ne. 0) .and. (ht%hole_diameter .ne. 1.0)) stop 1
  call ht%set_user_defined (5)
  if ((ht%user_defined .ne. 5) .and. (ht%hole_diameter .ne. 1.0)) stop 2
  call ht%set_diameter(2.0)
  if ((ht%user_defined .ne. 5) .and. (ht%hole_diameter .ne. 2.0)) stop 3
end
