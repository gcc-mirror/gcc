! { dg-do run }
! Tests the fix for PR20938 in which dependencies between equivalenced 
! arrays were not detected.
! 
real, dimension (3) :: a = (/1., 2., 3./), b, c
equivalence (a(2), b), (a(1), c)
b = a;
if (any(b .ne. (/1., 2., 3./))) STOP 1
b = c
if (any(b .ne. (/1., 1., 2./))) STOP 2
end
