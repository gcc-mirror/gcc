! { dg-do compile }
! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

! Check that the right call to f is wrapped in a GOMP_DISPATCH internal function
! before translation and that it is stripped during gimplification.

subroutine g(x,f)
  interface
    integer function f(y)
       allocatable :: f
       integer :: y
    end
  end interface
  integer, allocatable :: X(:)
  
  !$omp dispatch
    x(f(3)) = f(f(2))
end

! { dg-final { scan-tree-dump-times "D\.\[0-9]+ = \.GOMP_DISPATCH \\(f \\(&D\.\[0-9]+\\)\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9]+ = f \\(&D\.\[0-9]+\\);" 1 "gimple" } }
