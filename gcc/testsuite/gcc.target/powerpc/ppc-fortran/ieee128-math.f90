! { dg-do compile { target { powerpc*-*-linux* } } }
! { dg-require-effective-target ppc_float128_sw }
! { dg-require-effective-target vsx_hw }
! { dg-options "-mvsx -mabi=ieeelongdouble -mfloat128" }
! { dg-excess-errors "expect error due to switching long double type" }
! Since the error message is not associated with a particular line
! number, we cannot use the dg-error directive and cannot specify a
! regexp to describe the expected error message.  The expected warning
! message is:
!  "Warning: Using IEEE extended precision long double [-Wpsabi]"

program test_qp
   implicit none
   real(16), volatile :: fp1, fp2;
   fp1 = 2.0
   fp2 = log (fp1)
end

! { dg-final { scan-assembler-not {\mbl logl\M}    } }
! { dg-final { scan-assembler     {\mbl logf128\M} } }
