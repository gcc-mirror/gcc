! { dg-do compile }
! Tests the fix for PR26976, in which non-compliant elemental
! intrinsic function results were not detected.  At the same
! time, the means to tests the compliance of TRANSFER with the
! optional SIZE parameter was added.
!
! Contributed by Dominique Dhumieres  <dominiq@lps.ens.fr>
!
real(4) :: pi, a(2), b(3)
character(26) :: ch

pi = acos(-1.0)
b = pi

a = cos(b) ! { dg-error "different shape for Array assignment" }

a = -pi
b = cos(a) ! { dg-error "different shape for Array assignment" }

ch = "abcdefghijklmnopqrstuvwxyz"
a = transfer (ch, pi, 3) ! { dg-error "different shape for Array assignment" }

! This already generated an error
b = reshape ((/1.0/),(/1/)) ! { dg-error "different shape for Array assignment" }

end
