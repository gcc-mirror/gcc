! { dg-additional-options "-fdump-tree-gimple" }
subroutine foo
  integer :: A(10), B(10), C(10)
  interface
    integer function ibar(x)
      integer :: x
    end function ibar
  end interface

  !$omp task affinity (iterator(j=ibar(0):ibar(1):ibar(2)) : a(ibar(j)), b(j), c(j))
  !$omp end task
end
! { dg-final { scan-tree-dump-times "= ibar \\(&C\\." 3 "gimple" } }
! { dg-final { scan-tree-dump-times "= ibar \\(&j" 1 "gimple" } }
