! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! Test that 'declare variant' works when applied to an external subroutine

module main
  implicit none
  
  interface
    subroutine base ()
      !$omp declare variant (variant) match (construct={parallel})
    end subroutine
    
    subroutine base2 ()
      !$omp declare variant (base2: variant2) match (construct={target})
    end subroutine
  end interface
contains
  subroutine variant ()
  end subroutine

  subroutine variant2 ()
  end subroutine

  subroutine test ()
    !$omp parallel
      call base ()  ! { dg-final { scan-tree-dump-times "variant \\\(\\\);" 1 "gimple" } }
    !$omp end parallel
  end subroutine

  subroutine test2 ()
    !$omp target
      call base2 ()  ! { dg-final { scan-tree-dump-times "variant2 \\\(\\\);" 1 "gimple" } }
    !$omp end target
  end subroutine
end module
