! { dg-do run }
!
! SELECT TYPE with temporaries
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  type :: t1
    integer :: i = -1
  end type t1

  type, extends(t1) :: t2
    integer :: j = -1
  end type t2

  class(t1), pointer :: cp
  type(t2), target :: b

  cp => b

  select type (cp)
  type is (t1)
    cp%i = 1
  type is (t2)
    cp%j = 2
  end select

  print *,b%i,b%j
  if (b%i /= -1) call abort()
  if (b%j /= 2) call abort()

  select type (cp)
  type is (t1)
    cp%i = 4
  type is (t2)
    cp%i = 3*cp%j
  end select

  print *,b%i,b%j
  if (b%i /= 6) call abort()
  if (b%j /= 2) call abort()

end
