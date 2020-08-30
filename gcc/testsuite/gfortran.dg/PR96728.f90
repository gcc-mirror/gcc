! { dg-do run }
!
! Test the fix for PR96728
!

module cref_m

  implicit none

  private

  public ::   &
    isub_a_m
  
contains

  subroutine isub_a_m(a, b)
    integer, intent(in)  :: a(..)
    integer, intent(out) :: b(size(a))

    integer :: i
    
    b = [(i, i=1,size(b))]
    return
  end subroutine isub_a_m
  
end module cref_m

program cref_p

  use cref_m, only: &
    isub_a_m

  implicit none
  
  integer            :: i

  integer, parameter :: n = 3
  integer, parameter :: p(*) = [(i, i=1,n*n)]
  
  integer :: a(n,n)
  integer :: b(n*n)

  a = reshape(p, shape=[n,n])
  call isub_a_m(a, b)
  if (any(b/=p)) stop 1
  stop

end program cref_p
