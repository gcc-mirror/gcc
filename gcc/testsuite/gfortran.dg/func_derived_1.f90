! { dg-do run }
! PR 17244
! verifies that functions returning derived type work
module m
  type t
     integer i
     real x
     character*5 c
     integer arr(5,5)
  end type t
end module m

use m
type(t) :: r
integer arr(5,5), vect(25), vect2(25)
do i=1,25
   vect = 0
   vect(i) = i
   arr = reshape (vect, shape(arr))
   r = f(i,real(i),"HALLO",arr)

   if (r%i .ne. i) call abort()
   if (r%x .ne. real(i)) call abort()
   if (r%c .ne. "HALLO") call abort()
   vect2 = reshape (r%arr, shape(vect2))
   if (any(vect2.ne.vect)) call abort()
end do
contains

function f(i,x,c,arr)
  type(t) :: f
  character*5 c
  integer arr(5,5)
  
  f = t(i,x,c,arr)
end function f

end

! { dg-final { cleanup-modules "m" } }
