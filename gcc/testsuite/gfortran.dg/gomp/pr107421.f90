! { dg-additional-options "-fdump-ipa-whole-program" }
! { dg-additional-options "-mno-direct-extern-access" { target { i?86-*-* x86_64-*-* } } }

integer :: i

common /c/ i

!$omp threadprivate (/c/)

i = 0

end

! tls_model should be tls-initial-exec due to common block.
! { dg-final { scan-ipa-dump "Varpool flags: tls-initial-exec" "whole-program" } }
