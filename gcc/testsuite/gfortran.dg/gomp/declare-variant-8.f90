! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

program main
  !$omp requires atomic_default_mem_order(seq_cst)
  !$omp declare target to (test3)
contains
  subroutine f01 ()
  end subroutine

  subroutine f02 ()
    !$omp declare variant (f01) match (user={condition(6 == 7)},implementation={vendor(gnu)})
  end subroutine

  subroutine f03 ()
  end subroutine

  subroutine f04 ()
    !$omp declare variant (f03) match (user={condition(6 == 6)},implementation={atomic_default_mem_order(seq_cst)})
  end subroutine

  subroutine f05 ()
  end subroutine

  subroutine f06 ()
    !$omp declare variant (f05) match (user={condition(.true.)},implementation={atomic_default_mem_order(relaxed)})
  end subroutine

  subroutine f07 ()
  end subroutine

  subroutine f08 ()
    !$omp declare variant (f07) match (construct={parallel,do},device={kind("any")})
  end subroutine

  subroutine f09 ()
  end subroutine

  subroutine f10 ()
    !$omp declare variant (f09) match (construct={parallel,do},implementation={vendor("gnu")})
  end subroutine

  subroutine f11 ()
  end subroutine

  subroutine f12 ()
    !$omp declare variant (f11) match (construct={parallel,do})
  end subroutine

  subroutine f13 ()
  end subroutine

  subroutine f14 ()
    !$omp declare variant (f13) match (construct={parallel,do})
  end subroutine

  subroutine f15 ()
    !$omp declare target to (f13, f14)
  end subroutine

  subroutine f16 ()
    !$omp declare variant (f15) match (implementation={vendor(llvm)})
  end subroutine

  subroutine f17 ()
  end subroutine

  subroutine f18 ()
    !$omp declare variant (f17) match (construct={target,parallel})
  end subroutine

  subroutine f19 ()
  end subroutine

  subroutine f20 ()
    !$omp declare variant (f19) match (construct={target,parallel})
  end subroutine

  subroutine f22 ()
    !$omp declare variant (f21) match (construct={teams,parallel})
  end subroutine

  subroutine f23 ()
  end subroutine

  subroutine f24 ()
    !$omp declare variant (f23) match (construct={teams,parallel,do})
  end subroutine

  subroutine f25 ()
  end subroutine

  subroutine f27 ()
  end subroutine

  subroutine f28 ()
    !$omp declare variant (f27) match (construct={teams,parallel,do})
  end subroutine

  subroutine f30 ()
    !$omp declare variant (f29) match (implementation={vendor(gnu)})
  end subroutine

  subroutine f31 ()
  end subroutine

  subroutine f32 ()
    !$omp declare variant (f31) match (construct={teams,parallel,do})
  end subroutine

  subroutine f33 ()
  end subroutine

  subroutine f34 ()
    !$omp declare variant (f33) match (device={kind("any\0any")})	! { dg-warning "unknown property '.any..0any.' of 'kind' selector" }
  end subroutine

  subroutine f35 ()
  end subroutine

  subroutine f36 ()
    !$omp declare variant (f35) match (implementation={vendor("gnu\0")})	! { dg-warning "unknown property '.gnu..0.' of 'vendor' selector" }
  end subroutine

  subroutine test1 ()
    integer :: i

    call f02 ()	! { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" } }
    call f04 ()	! { dg-final { scan-tree-dump-times "f03 \\\(\\\);" 1 "gimple" } }
    call f06 ()	! { dg-final { scan-tree-dump-times "f06 \\\(\\\);" 1 "gimple" } }

    !$omp parallel
      !$omp do
      do i = 1, 2
	call f08 ()		! { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" } }
      end do
      !$omp end do
    !$omp end parallel

    !$omp parallel do
      do i = 1, 2
	call f10 ()		! { dg-final { scan-tree-dump-times "f09 \\\(\\\);" 1 "gimple" } }
      end do
    !$omp end parallel do

    !$omp do
      do i = 1, 2
	!$omp parallel
	  call f12 ()	! { dg-final { scan-tree-dump-times "f12 \\\(\\\);" 1 "gimple" } }
	!$omp end parallel
      end do
    !$omp end do

    !$omp parallel
      !$omp target
	!$omp do
	do i = 1, 2
	  call f14 ()		! { dg-final { scan-tree-dump-times "f14 \\\(\\\);" 1 "gimple" } }
	end do
	!$omp end do
      !$omp end target
    !$omp end parallel

    call f16 ()	! { dg-final { scan-tree-dump-times "f16 \\\(\\\);" 1 "gimple" } }
    call f34 ()	! { dg-final { scan-tree-dump-times "f34 \\\(\\\);" 1 "gimple" } }
    call f36 ()	! { dg-final { scan-tree-dump-times "f36 \\\(\\\);" 1 "gimple" } }
  end subroutine

  subroutine test2 ()
    ! OpenMP 5.0 specifies that the 'target' trait should be added for
    ! functions within a declare target block, but Fortran does not have
    ! the notion of a declare target _block_, so the variant is not used here.
    ! This may change in later versions of OpenMP.

    !$omp declare target
    !$omp parallel
      call f18 ()	! { dg-final { scan-tree-dump-times "f18 \\\(\\\);" 1 "gimple" } }
    !$omp end parallel
  end subroutine

  subroutine test3 ()
    ! In the C version, this test was used to check that the
    ! 'declare target to' form of the directive did not result in the variant
    ! being used.
    !$omp parallel
      call f20 ()	! { dg-final { scan-tree-dump-times "f20 \\\(\\\);" 1 "gimple" } }
    !$omp end parallel
  end subroutine

  subroutine f21 ()
    integer :: i
    !$omp do
      do i = 1, 2
	call f24 ()	! { dg-final { scan-tree-dump-times "f23 \\\(\\\);" 1 "gimple" } }
      end do
    !$omp end do
  end subroutine

  subroutine f26 ()
    !$omp declare variant (f25) match (construct={teams,parallel})

    integer :: i
    !$omp do
      do i = 1, 2
	call f28 ()	! { dg-final { scan-tree-dump-times "f28 \\\(\\\);" 1 "gimple" } }
      end do
    !$omp end do
  end subroutine

  subroutine f29 ()
    integer :: i
    !$omp do
      do i = 1, 2
	call f32 ()	! { dg-final { scan-tree-dump-times "f32 \\\(\\\);" 1 "gimple" } }
      end do
    !$omp end do
  end subroutine
end program
