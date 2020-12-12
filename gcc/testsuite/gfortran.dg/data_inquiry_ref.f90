! { dg-do run }
!
! Test the fix for PR98022.
!
! Contributed by Arseny Solokha  <asolokha@gmx.com>
!
module ur
contains
! The reporter's test.
  function kn1() result(hm2)
    complex :: hm(1:2), hm2(1:2)
    data (hm(md)%re, md=1,2)/1.0, 2.0/
    hm2 = hm
  end function kn1

! Check for derived types with complex components.
  function kn2() result(hm2)
    type t
      complex :: c
      integer :: i
    end type
    type (t) :: hm(1:2)
    complex :: hm2(1:2)
    data (hm(md)%c%im, md=1,2)/1.0, 2.0/
    data (hm(md)%i, md=1,2)/1, 2/
    hm2 = hm%c
  end function kn2
end module ur

  use ur
  if (any (kn1() .ne. [(1.0,0.0),(2.0,0.0)])) stop 1
  if (any (kn2() .ne. [(0.0,1.0),(0.0,2.0)])) stop 2
end
