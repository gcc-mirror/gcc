! { dg-do run }
! { dg-options "-fdec" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

program convert
  complex(4) :: a
  complex(4) :: b
  a = 8HABCDABCD
  b = transfer("ABCDABCD", b);
  ! Hollerith constants
  if (a.ne.8HABCDABCD) stop 1
  if (a.eq.8HABCEABCE) stop 2
  if (8HABCDABCD.ne.b) stop 3
  if (8HABCEABCE.eq.b) stop 4
end program
