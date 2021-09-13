! { dg-do run }
!
! Test the fix for PR100094
!

program foo_p

  implicit none

  integer, parameter :: n = 11
  
  integer, pointer :: pout(:)
  integer,  target :: a(n)
  integer          :: i
  
  a = [(i, i=1,n)]
  call foo(pout)
  if(.not.associated(pout)) stop 1
  if(.not.associated(pout, a)) stop 2
  if(any(pout/=a)) stop 3
  stop

contains

  subroutine foo(that)
    integer, pointer, intent(out) :: that(..)

    select rank(that)
    rank(1)
      that => a
    rank default
      stop 4
    end select
    return
  end subroutine foo

end program foo_p
