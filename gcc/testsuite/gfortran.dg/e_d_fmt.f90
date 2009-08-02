! { dg-do run }
! { dg-options "-std=legacy" }
!
! Verify that the D format uses 'D' as the exponent character.
!   "     "    "  E   "     "   'E' "   "     "         " 
CHARACTER*10 c1, c2
REAL(kind=8) r
r = 1.0
write(c1,"(e9.2)") r
write(c2,"(d9.2)") r

if (trim(adjustl(c1)) .ne. "0.10E+01") call abort()
if (trim(adjustl(c2)) .ne. "0.10D+01") call abort()

END
