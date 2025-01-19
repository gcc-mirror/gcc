! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

module main
  implicit none
    interface
      integer function f0 ()
      end function

      integer function f1 ()
      end function

      integer function f2 ()
        !$omp declare variant (f0) match (construct={dispatch})
        !$omp declare variant (f1) match (implementation={vendor(gnu)})
      end function
    end interface
  contains
  
  integer function test ()
    integer :: a

    !$omp dispatch
      a = f2 ()
    !$omp dispatch novariants(.TRUE.)
      a = f2 ()
    !$omp dispatch novariants(.FALSE.)
      a = f2 ()
    !$omp dispatch nocontext(.TRUE.)
      a = f2 ()
    !$omp dispatch nocontext(.FALSE.)
      a = f2 ()
  end function
end module


! { dg-final { scan-tree-dump-times "a = f0 \\\(\\\);" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "a = f1 \\\(\\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "a = f2 \\\(\\\);" 1 "gimple" } }
