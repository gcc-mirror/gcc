! { dg-require-effective-target pie }
! { dg-additional-options "-fdump-ipa-whole-program" }
! Add -fPIE or -mno-direct-extern-access to disable direct access to
! external symbol from executable.
! { dg-additional-options "-fPIE" { target { ! { i?86-*-* x86_64-*-* } } } }
! { dg-additional-options "-mno-direct-extern-access" { target { i?86-*-* x86_64-*-* } } }

integer :: i

common /c/ i

!$omp threadprivate (/c/)

i = 0

end

! tls_model should be tls-initial-exec due to common block.
! { dg-final { scan-ipa-dump "Varpool flags: tls-initial-exec" "whole-program" } }
