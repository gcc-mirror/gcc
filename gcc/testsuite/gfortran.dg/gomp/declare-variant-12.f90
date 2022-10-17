! { dg-do compile }
! { dg-additional-options "-foffload=disable -fdump-tree-gimple" }
! { dg-additional-options "-mavx512bw -mavx512vl" { target { i?86-*-* x86_64-*-* } } }

program main
  !$omp requires atomic_default_mem_order(seq_cst)
contains
  subroutine f01 ()
  end subroutine

  subroutine f02 ()
  end subroutine

  subroutine f03 ()
  end subroutine

  subroutine f04 ()
    !$omp declare variant (f01) match (device={isa("avx512f","avx512vl")}) ! 16
    !$omp declare variant (f02) match (implementation={vendor(score(15):gnu)})
    !$omp declare variant (f03) match (user={condition(score(11):1)})
  end subroutine

  subroutine f05 ()
  end subroutine

  subroutine f06 ()
  end subroutine

  subroutine f07 ()
  end subroutine

  subroutine f08 ()
    !$omp declare variant (f05) match (device={isa(avx512f,avx512vl)}) ! 16
    !$omp declare variant (f06) match (implementation={vendor(score(15):gnu)})
    !$omp declare variant (f07) match (user={condition(score(17):1)})
  end subroutine

  subroutine f09 ()
  end subroutine

  subroutine f10 ()
  end subroutine

  subroutine f11 ()
  end subroutine

  subroutine f12 ()
  end subroutine

  subroutine f13 ()
    !$omp declare variant (f09) match (device={arch(x86_64)},user={condition(score(65):1)}) ! 64+65
    !$omp declare variant (f10) match (implementation={vendor(score(127):"gnu")})
    !$omp declare variant (f11) match (device={isa(ssse3)}) ! 128
    !$omp declare variant (f12) match (implementation={atomic_default_mem_order(score(126):seq_cst)})
  end subroutine

  subroutine f14 ()
  end subroutine

  subroutine f15 ()
  end subroutine

  subroutine f16 ()
  end subroutine

  subroutine f17 ()
    !$omp declare variant (f14) match (construct={teams,parallel,do}) ! 16+8+4
    !$omp declare variant (f15) match (construct={parallel},user={condition(score(19):1)}) ! 8+19
    !$omp declare variant (f16) match (implementation={atomic_default_mem_order(score(27):seq_cst)})
  end subroutine

  subroutine f18 ()
  end subroutine

  subroutine f19 ()
  end subroutine

  subroutine f20 ()
  end subroutine

  subroutine f21 ()
    !$omp declare variant (f18) match (construct={teams,parallel,do}) ! 16+8+4
    !$omp declare variant (f19) match (construct={do},user={condition(score(25):1)}) ! 4+25
    !$omp declare variant (f20) match (implementation={atomic_default_mem_order(score(28):seq_cst)})
  end subroutine

  subroutine f22 ()
  end subroutine

  subroutine f23 ()
  end subroutine

  subroutine f24 ()
  end subroutine

  subroutine f25 ()
    !$omp declare variant (f22) match (construct={parallel,do}) ! 2+1
    !$omp declare variant (f23) match (construct={do}) ! 0
    !$omp declare variant (f24) match (implementation={atomic_default_mem_order(score(2):seq_cst)})
  end subroutine

  subroutine f26 ()
  end subroutine

  subroutine f27 ()
  end subroutine

  subroutine f28 ()
  end subroutine

  subroutine f29 ()
    !$omp declare variant (f26) match (construct={parallel,do}) ! 2+1
    !$omp declare variant (f27) match (construct={do},user={condition(1)}) ! 4
    !$omp declare variant (f28) match (implementation={atomic_default_mem_order(score(3):seq_cst)})
  end subroutine

  subroutine test1 ()
    integer :: i, j

    !$omp parallel do	! 2 constructs in OpenMP context, isa has score 2^4.
    do i = 1, 2
      call f04 ()	! { dg-final { scan-tree-dump-times "f01 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } }
			! { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } }
    end do
    !$omp end parallel do

    !$omp target teams	! 2 constructs in OpenMP context, isa has score 2^4.
      call f08 ()	! { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" } }
    !$omp end target teams

    !$omp teams
    !$omp parallel do
    do i = 1, 2
      !$omp parallel do	! 5 constructs in OpenMP context, arch is 2^6, isa 2^7.
      do j = 1, 2
	  call f13 ()	! { dg-final { scan-tree-dump-times "f09 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && lp64 } } } }
			! { dg-final { scan-tree-dump-times "f11 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && { ! lp64 } } } } }
			! { dg-final { scan-tree-dump-times "f10 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } }
	  call f17 ()	! { dg-final { scan-tree-dump-times "f14 \\\(\\\);" 1 "gimple" } }
	  call f21 ()	! { dg-final { scan-tree-dump-times "f19 \\\(\\\);" 1 "gimple" } }
      end do
      !$omp end parallel do
    end do
    !$omp end parallel do
    !$omp end teams

    !$omp do
    do i = 1, 2
      !$omp parallel do
      do j = 1, 2
	call f25 ();	! { dg-final { scan-tree-dump-times "f22 \\\(\\\);" 1 "gimple" } }
	call f29 ();	! { dg-final { scan-tree-dump-times "f27 \\\(\\\);" 1 "gimple" } }
      end do
      !$omp end parallel do
    end do
    !$omp end do
  end subroutine
end program

