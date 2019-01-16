! { dg-do compile }
! { dg-options "-fbackslash -finit-local-zero -fno-init-local-zero -fdump-tree-original" }
!
! PR fortran/87919
!
! Make sure -fno-init-local-zero disables -finit-local-zero.
!

include 'init_flag_1.f90'

! Make sure no initialization code is generated.
! { dg-final { scan-tree-dump-times "r\[1-4] *= *\[0\{]" 0 "original" } }
! { dg-final { scan-tree-dump-times "l\[12] *= *\[0\{]" 0 "original" } }
! { dg-final { scan-tree-dump-times "i\[1-4] *= *\[0\{]" 0 "original" } }
! { dg-final { scan-tree-dump-times "memmove *\[(]\[^,]*c\[1-4]" 0 "original" } }
