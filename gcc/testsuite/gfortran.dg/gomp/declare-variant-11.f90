! { dg-do compile }
! { dg-additional-options "-foffload=disable -fdump-tree-gimple" }
! { dg-additional-options "-mavx512bw -mavx512vl" { target { i?86-*-* x86_64-*-* } } }

program main
  implicit none
contains
  subroutine f01 ()
  end subroutine

  subroutine f02 ()
  end subroutine

  subroutine f03 ()
    !$omp declare variant (f01) match (device={isa(avx512f,"avx512vl")})
    !$omp declare variant (f02) match (device={isa(avx512bw,avx512vl,"avx512f")})
  end subroutine

  subroutine f04 ()
  end subroutine

  subroutine f05 ()
  end subroutine

  subroutine f06 ()
    !$omp declare variant (f04) match (device={isa(avx512f,avx512vl)})
    !$omp declare variant (f05) match (device={isa(avx512bw,avx512vl,avx512f)})
  end subroutine

  subroutine f07 ()
  end subroutine

  subroutine f08 ()
  end subroutine

  subroutine f09 ()
    !$omp declare variant (f07) match (device={isa(sse4,"sse4.1","sse4.2",sse3,"avx")})
    !$omp declare variant (f08) match (device={isa("avx",sse3)})
  end subroutine

  subroutine f10 ()
  end subroutine

  subroutine f11 ()
  end subroutine

  subroutine f12 ()
  end subroutine

  subroutine f13 ()
    !$omp declare variant (f10) match (device={isa("avx512f")})
    !$omp declare variant (f11) match (user={condition(1)},device={isa(avx512f)},implementation={vendor(gnu)})
    !$omp declare variant (f12) match (user={condition(2 + 1)},device={isa(avx512f)})
  end subroutine

  subroutine f14 ()
  end subroutine

  subroutine f15 ()
  end subroutine

  subroutine f16 ()
  end subroutine

  subroutine f17 ()
  end subroutine

  subroutine f18 ()
    !$omp declare variant (f14) match (construct={teams,do})
    !$omp declare variant (f15) match (construct={teams,parallel,do})
    !$omp declare variant (f16) match (construct={do})
    !$omp declare variant (f17) match (construct={parallel,do})
  end subroutine

  subroutine f19 ()
  end subroutine

  subroutine f20 ()
  end subroutine

  subroutine f21 ()
  end subroutine

  subroutine f22 ()
  end subroutine

  subroutine f23 ()
    !$omp declare variant (f19) match (construct={teams,do})
    !$omp declare variant (f20) match (construct={teams,parallel,do})
    !$omp declare variant (f21) match (construct={do})
    !$omp declare variant (f22) match (construct={parallel,do})
  end subroutine

  subroutine f24 ()
  end subroutine

  subroutine f25 ()
  end subroutine

  subroutine f26 ()
  end subroutine

  subroutine f27 ()
    !$omp declare variant (f24) match (device={kind(cpu)})
    !$omp declare variant (f25) match (device={kind(cpu),isa(avx512f),arch(x86_64)})
    !$omp declare variant (f26) match (device={arch(x86_64),kind(cpu)})
  end subroutine

  subroutine test1
    integer :: i
    call f03 ()	! { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } }
		! { dg-final { scan-tree-dump-times "f03 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } }
    call f09 ()	! { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } }
		! { dg-final { scan-tree-dump-times "f09 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } }
    call f13 ()	! { dg-final { scan-tree-dump-times "f11 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } }
		! { dg-final { scan-tree-dump-times "f13 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } }
    !$omp teams distribute parallel do
    do i = 1, 2
      call f18 ()	! { dg-final { scan-tree-dump-times "f15 \\\(\\\);" 1 "gimple" } }
    end do
    !$omp end teams distribute parallel do

    !$omp parallel do
    do i = 1, 2
      call f23 ()	! { dg-final { scan-tree-dump-times "f22 \\\(\\\);" 1 "gimple" } }
    end do
    !$omp end parallel do

    call f27 ()	! { dg-final { scan-tree-dump-times "f25 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && lp64 } } } }
		! { dg-final { scan-tree-dump-times "f24 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && { ! lp64 } } } } }
		! { dg-final { scan-tree-dump-times "f24 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* i?86-*-* x86_64-*-* } } } } }
		! { dg-final { scan-tree-dump-times "f27 \\\(\\\);" 1 "gimple" { target { nvptx*-*-* amdgcn*-*-* } } } }
  end subroutine
end program
