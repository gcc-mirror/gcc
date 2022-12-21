! { dg-do compile }

program p
   integer, allocatable :: a
   !$omp target map(tofrom: a, a) ! { dg-error "Symbol 'a' present on multiple clauses" }
   !$omp end target
end
