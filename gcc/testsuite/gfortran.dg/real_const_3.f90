!{ dg-do run }
!{ dg-options "-fno-range-check" }
! PR19310 and PR19904, allow disabling range check during compile.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program main
  real, parameter :: zero=0, nan=0/zero
  complex :: z = (-0.1,-2.2)/(0.0,0.0)
  complex :: z2 = (0.1,1)/0
  complex :: z3 = (1e300, -2e-200)/1234e-325
  complex :: z4 = (1e-300, -2e-200)/1234e325
  real :: a
  a = exp(1000.0)
  b = 1/exp(1000.0)
  print *, a
  print *, b
  print *, -1.0/b
  print *, b/0.0
  print *, 0.0/0.0
  print *, 1.0/-0.0
  print *, -2.0/0.0
  print *, 3.0/0.0
  print *, nan
  print *, z
  print *, z2
  print *, z3
  print *, z4

end program main
!{ dg-output "      \\+Infinity(\n|\r\n|\r)" }
!{ dg-output "   0.000000    (\n|\r\n|\r)" }
!{ dg-output "      -Infinity(\n|\r\n|\r)" }
!{ dg-output "            NaN(\n|\r\n|\r)" }
!{ dg-output "            NaN(\n|\r\n|\r)" }
!{ dg-output "      -Infinity(\n|\r\n|\r)" }
!{ dg-output "      -Infinity(\n|\r\n|\r)" }
!{ dg-output "      \\+Infinity(\n|\r\n|\r)" }
!{ dg-output "            NaN(\n|\r\n|\r)" }
!{ dg-output " \\(           NaN,           NaN\\)(\n|\r\n|\r)" }
!{ dg-output " \\(           NaN,           NaN\\)(\n|\r\n|\r)" }
!{ dg-output " \\(     \\+Infinity,     -Infinity\\)(\n|\r\n|\r)" }
!{ dg-output " \\(  0.000000    , -0.000000    \\)(\n|\r\n|\r)" }
