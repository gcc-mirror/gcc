! { dg-lto-do link }
! { dg-lto-options {{ -g -flto }} }
! { dg-extra-ld-options "-r -nostdlib" }

MODULE globalvar_mod
integer        :: xstop
CONTAINS
END MODULE globalvar_mod
