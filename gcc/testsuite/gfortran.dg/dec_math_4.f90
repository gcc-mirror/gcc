! { dg-options "-std=gnu" }
! { dg-do compile }

! Former ICE when simplifying complex cotan
complex, parameter :: z = cotan((1., 1.))
print *, z

end
