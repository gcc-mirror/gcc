! { dg-do run }
!
! executing simple SELECT TYPE statements
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  type :: t1
    integer :: i
  end type t1

  type, extends(t1) :: t2
    integer :: j
  end type t2

  type, extends(t1) :: t3
    real :: r
  end type

  class(t1), pointer :: cp
  type(t1), target :: a
  type(t2), target :: b
  type(t3), target :: c
  integer :: i

  cp => a
  i = 0

  select type (cp)
  type is (t1)
    i = 1
  type is (t2)
    i = 2
! FIXME: CLASS IS is not yet supported
!  class is (t1)
!    i = 3
  end select

  if (i /= 1) call abort()

  cp => b
  i = 0

  select type (cp)
  type is (t1)
    i = 1
  type is (t2)
    i = 2
! FIXME: CLASS IS is not yet supported
!  class is (t2)
!    i = 3
  end select

  if (i /= 2) call abort()

  cp => c
  i = 0

  select type (cp)
  type is (t1)
    i = 1
  type is (t2)
    i = 2
  class default
    i = 3
  end select

  if (i /= 3) call abort()

end
