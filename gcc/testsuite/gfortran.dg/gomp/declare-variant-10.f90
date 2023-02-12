! { dg-do compile }
! { dg-additional-options "-cpp -foffload=disable -fdump-tree-gimple" }
! { dg-additional-options "-mavx512bw" { target { i?86-*-* x86_64-*-* } } }

#undef i386

program main
  !$omp declare target to (test3)
contains
  subroutine f01 ()
  end subroutine
  subroutine f02 ()
    !$omp declare variant (f01) match (device={isa(avx512f,avx512bw)})
  end subroutine
  subroutine f03 ()
  end subroutine
  subroutine f04 ()
    !$omp declare variant (f03) match (device={kind("any"),arch(x86_64),isa(avx512f,avx512bw)})
  end subroutine
  subroutine f05 ()
  end subroutine
  subroutine f06 ()
    !$omp declare variant (f05) match (device={kind(gpu)})
  end subroutine
  subroutine f07 ()
  end subroutine
  subroutine f08 ()
    !$omp declare variant (f07) match (device={kind(cpu)})
  end subroutine
  subroutine f09 ()
  end subroutine
  subroutine f10 ()
    !$omp declare variant (f09) match (device={isa(sm_35)})
  end subroutine
  subroutine f11 ()
  end subroutine
  subroutine f12 ()
    !$omp declare variant (f11) match (device={arch("nvptx")})
  end subroutine
  subroutine f13 ()
  end subroutine
  subroutine f14 ()
    !$omp declare variant (f13) match (device={arch(i386),isa("sse4")})
  end subroutine
  subroutine f15 ()
  end subroutine
  subroutine f16 ()
    !$omp declare variant (f15) match (device={isa(sse4,ssse3),arch(i386)})
  end subroutine
  subroutine f17 ()
  end subroutine
  subroutine f18 ()
    !$omp declare variant (f17) match (device={kind(any,fpga)})
  end subroutine

  subroutine test1 ()
    !$omp declare target
    integer :: i

    call f02 ()	  ! { dg-final { scan-tree-dump-times "f01 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } }
		  ! { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } }
    call f14 ()	  ! { dg-final { scan-tree-dump-times "f13 \\\(\\\);" 1 "gimple" { target ia32 } } }
		  ! { dg-final { scan-tree-dump-times "f14 \\\(\\\);" 1 "gimple" { target { ! ia32 } } } }
    call f18 ()	  ! { dg-final { scan-tree-dump-times "f18 \\\(\\\);" 1 "gimple" } } */
  end subroutine

#if defined(__i386__) || defined(__x86_64__)
  __attribute__((target ("avx512f,avx512bw")))
#endif
  subroutine test2 ()
    !$omp target
      call f04 () ! { dg-final { scan-tree-dump-times "f03 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && { ! ilp32 } } } } }
                  ! { dg-final { scan-tree-dump-times "f04 \\\(\\\);" 1 "gimple" { target { { ilp32 } || { ! { i?86-*-* x86_64-*-* } } } } } }
    !$omp end target
    !$omp target
      call f16 ()	! { dg-final { scan-tree-dump-times "f15 \\\(\\\);" 1 "gimple" { target ia32 } } }
			! { dg-final { scan-tree-dump-times "f16 \\\(\\\);" 1 "gimple" { target { ! ia32 } } } }
    !$omp end target
  end subroutine

  subroutine test3 ()
    call f06 ()	  ! { dg-final { scan-tree-dump-times "f06 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } }
    call f08 ()	  ! { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } }
  end subroutine

  subroutine test4 ()
    !$omp target
      call f10 ()	! { dg-final { scan-tree-dump-times "f10 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } }
    !$omp end target

    !$omp target
      call f12 ()	! { dg-final { scan-tree-dump-times "f12 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* } } } } }
			! { dg-final { scan-tree-dump-times "f11 \\\(\\\);" 1 "gimple" { target { nvptx*-*-* } } } }
    !$omp end target
  end subroutine
end program

