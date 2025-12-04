# 1 "<test>"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "<test>"
! PR fortran/92613
! { dg-do preprocess }
! { dg-options "-cpp -fpreprocessed" }
! { dg-error ".-E. is not supported with .-fpreprocessed." "" { target *-*-* } 0 }
! { dg-prune-output "compilation terminated" }
program test
end program
