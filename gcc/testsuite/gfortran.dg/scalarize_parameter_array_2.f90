! { dg-do compile }
!
! Test the fix for PR45305. The if statements should simplify away so
! that 'I_do_not_exist' is not referenced.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
if (any (abs(bessel_jn([1,2], 1.0) - bessel_jn([1,2], 1.0)) &
         > epsilon(0.0))) &
  call I_do_not_exist()

if (any (abs(bessel_jn(1, 2, 1.0) - bessel_jn([1,2], 1.0)) &
         > epsilon(0.0))) &
  call I_do_not_exist()
end
