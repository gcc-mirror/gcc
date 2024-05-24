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

subroutine call_it(ctx, arr)
  logical :: ctx
  integer :: arr(:)
  !$omp dispatch nocontext(ctx)
    call f0(arr)
  !$omp end dispatch        ! valid since 5.2
  !$omp dispatch nocontext(ctx)
    call f0(arr)
  !$omp end dispatch nowait ! likewise valid (unless there is a 'nowait' at '!$omp dispatch')
  !$omp dispatch nowait
    call f0(arr)
  !$omp end dispatch nowait !{ dg-error "Duplicated NOWAIT clause on !.OMP DISPATCH and !.OMP END DISPATCH at .1." }
end 
