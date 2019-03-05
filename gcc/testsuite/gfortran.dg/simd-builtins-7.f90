! { dg-do compile { target { i?86-*-linux* x86_64-*-linux* } } }
! { dg-additional-options "-msse2 -mno-avx -nostdinc -Ofast -fpre-include=simd-builtins-7.h -fdump-tree-optimized" }

program test_overloaded_intrinsic
  real(4) :: x4(3200), y4(3200)
  real(8) :: x8(3200), y8(3200)

  y4 = sin(x4)
  print *, y4

  y4 = sin(x8)
  print *, y8
end

! { dg-final { scan-tree-dump "sinf.simdclone" "optimized" { target ia32 } } } */
! { dg-final { scan-tree-dump-not "sin.simdclone" "optimized" { target ia32 } } } */

! { dg-final { scan-tree-dump "sin.simdclone" "optimized" { target lp64} } } */
! { dg-final { scan-tree-dump-not "sinf.simdclone" "optimized" { target lp64 } } } */
