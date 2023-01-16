module m
  integer ::x 
! Nonsense but OpenMP-semantically valid:
  !$omp assumes contains(target) holds(x > 0.0)
  !$omp assumes absent(target)
  !$omp assumes holds(0.0)
! { dg-error "HOLDS expression at .1. must be a scalar logical expression" "" { target *-*-* } .-1 }
end module

module m2
interface
  subroutine foo
    !$omp assumes contains(target) contains(teams,target) ! { dg-error "'TARGET' directive mentioned multiple times in CONTAINS clause in !.OMP ASSUMES directive" }
    !$omp assumes absent(declare target) ! { dg-error "Invalid 'DECLARE TARGET' directive at .1. in ABSENT clause: declarative, informational and meta directives not permitted" }
    !$omp assumes absent(parallel) absent(do,simd,parallel,distribute) ! { dg-error "'PARALLEL' directive mentioned multiple times in ABSENT clause in !.OMP ASSUMES directive" }
    !$omp assumes contains(barrier,atomic) absent(cancel,simd,atomic,distribute) ! { dg-error "'SIMD' directive mentioned both times in ABSENT and CONTAINS clauses in !.OMP ASSUMES directive" }
  end subroutine foo
end interface
end module m2
