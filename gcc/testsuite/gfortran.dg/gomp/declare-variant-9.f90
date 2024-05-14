! { dg-do compile }
! { dg-additional-options "-cpp -fdump-tree-gimple" }
! { dg-additional-options "-mno-sse3" { target { i?86-*-* x86_64-*-* } } }

program main
  implicit none
contains
  subroutine f01 ()
  end subroutine
  subroutine f02 ()
    !$omp declare variant (f01) match (device={isa("avx512f",avx512bw)})
  end subroutine
  subroutine f05 ()
  end subroutine
  subroutine f06 ()
    !$omp declare variant (f05) match (device={kind(gpu)})
  end subroutine
  subroutine f07 ()
  end subroutine
  subroutine f08 ()
    !$omp declare variant (f07) match (device={kind("cpu")})
  end subroutine
  subroutine f09 ()
  end subroutine
  subroutine f10 ()
    !$omp declare variant (f09) match (device={isa(sm_35)})
  end subroutine
  subroutine f11 ()
  end subroutine
  subroutine f12 ()
    !$omp declare variant (f11) match (device={arch(nvptx)})
  end subroutine
  subroutine f13 ()
  end subroutine
  subroutine f14 ()
    !$omp declare variant (f13) match (device={arch("i386"),isa(sse4)})
  end subroutine
  subroutine f17 ()
  end subroutine
  subroutine f18 ()
    !$omp declare variant (f17) match (device={kind("fpga")})
  end subroutine

  subroutine test1 ()
    integer ::  i;
    call f02 ()	! { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" } }
    call f14 ()	! { dg-final { scan-tree-dump-times "f14 \\\(\\\);" 1 "gimple" } }
    call f18 ()	! { dg-final { scan-tree-dump-times "f18 \\\(\\\);" 1 "gimple" } }
  end subroutine

  subroutine test3 ()
    call f06 ()	! { dg-final { scan-tree-dump-times "f06 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } }
    call f08 ()	! { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } }
    call f10 ()	! { dg-final { scan-tree-dump-times "f10 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } }
    call f12 ()	! { dg-final { scan-tree-dump-times "f12 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* } } } } }
		! { dg-final { scan-tree-dump-times "f11 \\\(\\\);" 1 "gimple" { target { nvptx*-*-* } } } }
  end subroutine
end program
