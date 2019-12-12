! { dg-do compile { target { i?86-*-linux* x86_64-*-linux* aarch64*-*-linux* } } }
! { dg-additional-options "-nostdinc -Ofast -fdump-tree-optimized" }
! { dg-additional-options "-msse2" { target i?86-*-linux* x86_64-*-linux* } }

program test_overloaded_intrinsic
  real(4) :: x4(3200), y4(3200)
  real(8) :: x8(3200), y8(3200)

  ! this should not be using simd clone
  y4 = sin(x4)
  print *, y4

  ! this should not be using simd clone
  y4 = sin(x8)
  print *, y8
end

! { dg-final { scan-tree-dump "__builtin_sinf" "optimized" } } */
! { dg-final { scan-tree-dump "__builtin_sin" "optimized" } } */
! { dg-final { scan-tree-dump-not "simdclone" "optimized" } } */
! { dg-final { scan-assembler-not "call.*_ZGVbN4v_sinf" } }
! { dg-final { scan-assembler-not "bl.*_ZGVnN4v_sinf" } }
