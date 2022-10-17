! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! Test 'declare variant' directive with an explicit base procedure name.

module main
  implicit none
  
  !$omp declare variant (base: variant) match (construct={target,parallel})
contains
  subroutine variant ()
  end subroutine

  subroutine base ()
  end subroutine

  subroutine variant2 ()
  end subroutine

  subroutine base2 ()
    !$omp declare variant (base2: variant2) match (construct={parallel})
  end subroutine

  subroutine test1 ()
    !$omp target
      !$omp parallel
	call base ()	! { dg-final { scan-tree-dump-times "variant \\\(\\\);" 1 "gimple" } }
      !$omp end parallel
    !$omp end target
  end subroutine

  subroutine test2 ()
    !$omp parallel
	call base2 ()	! { dg-final { scan-tree-dump-times "variant2 \\\(\\\);" 1 "gimple" } }
    !$omp end parallel
  end subroutine
end module
