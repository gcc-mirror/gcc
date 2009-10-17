! { dg-do run }
!
! PR 41629: [OOP] gimplification error on valid code
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  type t1
    integer :: comp
  end type

  type(t1), target :: a

  class(t1) :: x
  pointer :: x       ! This is valid

  a%comp = 3
  x => a
  print *,x%comp
  if (x%comp/=3) call abort()

end
