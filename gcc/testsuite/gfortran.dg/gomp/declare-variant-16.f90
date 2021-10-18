! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! Test that 'declare variant' works when applied to an external subroutine

module main
  implicit none
  
  interface
    subroutine base ()
      !$omp declare variant (variant) match (construct={parallel})
    end subroutine
  end interface

contains
  subroutine variant ()
  end subroutine

  subroutine test ()
    !$omp parallel
      call base ()  ! { dg-final { scan-tree-dump-times "variant \\\(\\\);" 1 "gimple" } }
    !$omp end parallel
  end subroutine
end module
