! { dg-do run }
!
! SELECT TYPE with associate-name
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  type :: t1
    integer :: i = -1
    class(t1), pointer :: c
  end type t1

  type, extends(t1) :: t2
    integer :: j = -1
  end type t2

  type(t2), target :: b
  integer :: aa

  b%c => b
  aa = 5

  select type (aa => b%c)
  type is (t1)
    aa%i = 1
  type is (t2)
    aa%j = 2
  end select

  print *,b%i,b%j
  if (b%i /= -1) call abort()
  if (b%j /= 2) call abort()

  select type (aa => b%c)
  type is (t1)
    aa%i = 4
  type is (t2)
    aa%i = 3*aa%j
  end select

  print *,b%i,b%j
  if (b%i /= 6) call abort()
  if (b%j /= 2) call abort()

  print *,aa
  if (aa/=5) call abort()

end
