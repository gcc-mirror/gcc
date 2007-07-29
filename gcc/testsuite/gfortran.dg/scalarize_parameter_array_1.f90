! { dg-do run }
! Tests the fix for pr32682, in which the scalarization loop variables
! were not being determined when 'c' came first in an expression.
!
! Contributed by Janus Weil <jaydub66@gmail.com>
!
program matrix

  implicit none
  real,dimension(2,2),parameter::c=reshape((/1,2,3,4/),(/2,2/))
  real,dimension(2,2)::m, n

  m=f()+c
  if (any (m .ne. reshape((/2,3,4,5/),(/2,2/)))) call abort ()
  m=c+f()
  if (any (m .ne. reshape((/2,3,4,5/),(/2,2/)))) call abort ()
  call sub(m+f())
  if (any (n .ne. reshape((/3,4,5,6/),(/2,2/)))) call abort ()
  call sub(c+m)
  if (any (n .ne. reshape((/3,5,7,9/),(/2,2/)))) call abort ()
  call sub(f()+c)
  if (any (n .ne. reshape((/2,3,4,5/),(/2,2/)))) call abort ()
  call sub(c+f())
  if (any (n .ne. reshape((/2,3,4,5/),(/2,2/)))) call abort ()

contains

  function f()    
    implicit none
    real, dimension(2,2)::f
    f=1
  end function f

  subroutine sub(a)
    implicit none
    real, dimension(2,2)::a
    n = a
  end subroutine sub

end program matrix
