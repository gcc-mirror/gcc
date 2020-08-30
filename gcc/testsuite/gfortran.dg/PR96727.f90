! { dg-do run }
!
! Test the fix for PR96727
!

program cref_p

  implicit none
  
  integer                     :: i

  integer,          parameter :: n = 3
  integer,          parameter :: p(*) = [(i, i=1,n*n)]
  character(len=*), parameter :: q = repeat('a', n*n)
  
  integer            :: a(n,n)
  character(len=n*n) :: c

  a = reshape(p, shape=[n,n])
  call csub(a, c)
  if (c/=q) stop 1
  stop

contains

  subroutine csub(a, b)
    integer,                intent(in)  :: a(..)
    character(len=size(a)), intent(out) :: b

    b = repeat('a', len(b))
    return
  end subroutine csub
  
end program cref_p
