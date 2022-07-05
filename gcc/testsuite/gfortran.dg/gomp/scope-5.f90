! { dg-do compile }

subroutine foo ()
  integer f
  f = 0;
  !$omp scope firstprivate(f)	! { dg-error "firstprivate variable 'f' is private in outer context" }
    f = f + 1
  !$omp end scope
end
