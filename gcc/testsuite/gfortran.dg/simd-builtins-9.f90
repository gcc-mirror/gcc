! { dg-do compile { target { aarch64*-*-linux* } } }
! { dg-additional-options "-nostdinc -O3 -fpre-include=simd-builtins-9.h -fdump-tree-optimized" }

program test_overloaded_intrinsic
  real(8) :: x8(3200), y8(3200)

  y8 = sin(x8)
  print *, y8

  x8 = cos(y8)
  print *, x8
end

! { dg-final { scan-tree-dump-not "sin.simdclone" "optimized" } } */

! { dg-final { scan-tree-dump "cos.simdclone" "optimized" } } */
