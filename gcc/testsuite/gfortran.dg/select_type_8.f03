! { dg-do run }
!
! executing SELECT TYPE statements with CLASS IS blocks
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  implicit none

  type :: t1
    integer :: i
  end type t1

  type, extends(t1) :: t2
    integer :: j
  end type t2

  type, extends(t2) :: t3
    real :: r
  end type

  class(t1), pointer :: cp
  type(t1), target :: a
  type(t2), target :: b
  type(t3), target :: c
  integer :: i

  cp => c
  i = 0
  select type (cp)
  type is (t1)
    i = 1
  type is (t2)
    i = 2
  class is (t1)
    i = 3
  class default
    i = 4
  end select
  print *,i
  if (i /= 3) STOP 1

  cp => a
  select type (cp)
  type is (t1)
    i = 1
  type is (t2)
    i = 2
  class is (t1)
    i = 3
  end select
  print *,i
  if (i /= 1) STOP 2

  cp => b
  select type (cp)
  type is (t1)
    i = 1
  class is (t3)
    i = 3
  class is (t2)
    i = 4
  class is (t1)
    i = 5
  end select
  print *,i
  if (i /= 4) STOP 3

  cp => b
  select type (cp)
  type is (t1)
    i = 1
  class is (t1)
    i = 5
  class is (t2)
    i = 4
  class is (t3)
    i = 3
  end select
  print *,i
  if (i /= 4) STOP 4

  cp => a
  select type (cp)
  type is (t2)
    i = 1
  class is (t2)
    i = 2
  class default
    i = 3
  class is (t3)
    i = 4
  type is (t3)
    i = 5
  end select
  print *,i
  if (i /= 3) STOP 5

end
