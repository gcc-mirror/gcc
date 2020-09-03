! { dg-do run }
!
! Test the fix for PR96726
!

module cref_m

  implicit none

  private

  public ::   &
    sizeish
  
contains

  pure function sizeish(a) result(s)
    integer, intent(in) :: a(..)

    integer :: s

    s = size(a)
    return
  end function sizeish
  
end module cref_m

program cref_p

  use cref_m, only: &
    sizeish

  implicit none
  
  integer            :: i

  integer, parameter :: n = 3
  integer, parameter :: p(*) = [(i, i=1,n*n)]
  
  integer :: a(n,n)
  integer :: b(n*n)

  a = reshape(p, shape=[n,n])
  call isub_a(a, b)
  if (any(b/=p)) stop 1
  call isub_b(a, b)
  if (any(b/=p)) stop 2
  stop

contains

  subroutine isub_a(a, b)
    integer, intent(in)  :: a(..)
    integer, intent(out) :: b(size(a))

    integer :: i
    
    b = [(i, i=1,size(b))]
    return
  end subroutine isub_a
  
  subroutine isub_b(a, b)
    integer, intent(in)  :: a(..)
    integer, intent(out) :: b(sizeish(a))

    integer :: i
    
    b = [(i, i=1,sizeish(b))]
    return
  end subroutine isub_b
  
end program cref_p
