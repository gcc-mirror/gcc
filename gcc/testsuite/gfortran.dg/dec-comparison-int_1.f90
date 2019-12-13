! { dg-do run }
! { dg-options "-fdec" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

program convert
  integer(4) :: a
  integer(4) :: b
  a = 4HABCD
  b = transfer("ABCD", b)
  ! Hollerith constants
  if (a.ne.4HABCD) stop 1
  if (a.eq.4HABCE) stop 2
  if (4HABCD.ne.b) stop 3
  if (4HABCE.eq.b) stop 4
  if (4HABCE.lt.a) stop 5
  if (a.gt.4HABCE) stop 6
  if (4HABCE.le.a) stop 7
  if (a.ge.4HABCE) stop 8
end program

