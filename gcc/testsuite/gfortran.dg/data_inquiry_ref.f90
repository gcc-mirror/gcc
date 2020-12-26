! { dg-do compile }
!
! Test the fix for PR98022. Code is in place to deliver the expected result.
! However, it was determined that the data statements below violate F18(R841)
! and so an error results.
!
! Contributed by Arseny Solokha  <asolokha@gmx.com>
!
module ur
contains
! The reporter's test.
  function kn1() result(hm2)
    complex :: hm(1:2), hm2(1:3), scalar
    data (hm(md)%re, md=1,2)/1.0, 2.0/, scalar%re/42.0/     ! { dg-error "neither an array-element" }
    data (hm(md)%im, md=1,2)/0.0, 0.0/, scalar%im/-42.0/    ! { dg-error "neither an array-element" }
    hm2(1:2) = hm
    hm2(3) = scalar
  end function kn1

! Check for derived types with complex components.
  function kn2() result(hm2)
    type t
      complex :: c
      integer :: i
    end type
    type (t) :: hm(1:2), scalar
    complex :: hm2(1:3)
    data (hm(md)%c%re, md=1,2)/0.0, 0.0/, scalar%c%re/42.0/  ! { dg-error "neither an array-element" }
    data (hm(md)%c%im, md=1,2)/1.0, 2.0/, scalar%c%im/-42.0/ ! { dg-error "neither an array-element" }
    data (hm(md)%i, md=1,2)/1, 2/
    hm2(1:2) = hm%c
    hm2(3) = scalar%c
  end function kn2
end module ur

!  use ur
!  if (any (kn1() .ne. [(1.0,0.0),(2.0,0.0),(42.0,-42.0)])) stop 1
!  if (any (kn2() .ne. [(0.0,1.0),(0.0,2.0),(42.0,-42.0)])) stop 2
end
