! { dg-do run }
! { dg-options "-fdec" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

program convert
  character(4) :: c = 4HJMAC
  if (4HJMAC.ne.4HJMAC) stop 1
  if (4HJMAC.ne."JMAC") stop 2
  if (4HJMAC.eq."JMAN") stop 3
  if ("JMAC".eq.4HJMAN) stop 4
  if ("AAAA".eq.5HAAAAA) stop 5
  if ("BBBBB".eq.5HBBBB ) stop 6
  if (4HJMAC.ne.c) stop 7
  if (c.ne.4HJMAC) stop 8
end program

