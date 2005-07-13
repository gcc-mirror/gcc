! { dg-do run }
! Test of the fix to the bug triggered by NIST fm908.for.
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
program past_eor
  character(len=82)         ::  buffer
  real                      ::  a(2), b(2), c(2), d(2), e(2)

  e = (/2.34,2.456/)

! tests 28-31 from fm908.for

  buffer = '  2.34 ,  2.456     2.34 ,  2.456     0.234E01,  2.456E00&
 &   0.234E+001, 2.456E-000'

  READ (UNIT=buffer,FMT=10) a, b, c, d
10 FORMAT (2(2(G7.5,1X),2X),2(G10.4E2,1X),1X,2(G11.7E4,1X))

  if (any (a.ne.e).or.any (b.ne.e).or.any (c.ne.e).or.any (d.ne.e)) call abort ()

end program past_eor

