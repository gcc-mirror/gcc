! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

module main
  implicit none
    interface
      subroutine f2 ()
      end subroutine
    end interface
  contains
  
  subroutine test ()
  !$omp dispatch  ! { dg-final { scan-tree-dump-not "#pragma omp task" "gimple" } }
    call f2 ()
  !$omp dispatch nowait ! { dg-final { scan-tree-dump-not "nowait" "gimple" } }
    call f2 ()
  end subroutine
end module

