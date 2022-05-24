subroutine foo (p)
  integer :: p(*)
  !$omp taskwait depend(iterator(i = 1:17) , in : p(i)) nowait depend(out : p(32))
end

subroutine bar (p)
  implicit none
  integer :: p(*)
  !$omp taskwait depend(mutexinoutset : p(1)) nowait	! { dg-error "'mutexinoutset' kind in 'depend' clause on a 'taskwait' construct" }
end

subroutine baz
  !$omp taskwait nowait	! { dg-error "'taskwait' construct with 'nowait' clause but no 'depend' clauses" }
end
