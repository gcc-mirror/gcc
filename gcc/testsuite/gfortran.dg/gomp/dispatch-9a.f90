! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

module m
contains
subroutine f1 (ar)
  integer :: arr(10)
end
subroutine f0 (ar)
  integer :: arr(10)
   !$omp declare variant (f1) match (construct={dispatch})
end
end module

subroutine call_it(x, arr)
  logical :: x
  integer :: arr(:)
  !$omp dispatch depend(inout:x) nowait
    call f0(arr)
  !$omp end dispatch        ! valid since 5.2
  !$omp dispatch depend(inout:x)
    call f0(arr)
  !$omp end dispatch nowait ! likewise valid (unless there is a 'nowait' at '!$omp dispatch')
end

! { dg-final { scan-tree-dump-times "#pragma omp taskwait depend\\(inout:x\\) nowait" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch nowait" 2 "gimple" } }
