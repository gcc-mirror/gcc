! { dg-do compile { target { aarch64*-*-linux* } } }
! { dg-additional-options "-nostdinc -Ofast -fpre-include=simd-builtins-8.h -fdump-tree-optimized" }

program test_overloaded_intrinsic
  real(4) :: x4(3200), y4(3200)
  real(8) :: x8(3200), y8(3200)

  y4 = sin(x4)
  print *, y4

  y4 = sin(x8)
  print *, y8
end

! { dg-final { scan-tree-dump "sinf.simdclone" "optimized" { target ilp32 } } } */
! { dg-final { scan-tree-dump-not "sin.simdclone" "optimized" { target ilp32 } } } */

! { dg-final { scan-tree-dump "sin.simdclone" "optimized" { target lp64 } } } */
! { dg-final { scan-tree-dump-not "sinf.simdclone" "optimized" { target lp64  } } } */
