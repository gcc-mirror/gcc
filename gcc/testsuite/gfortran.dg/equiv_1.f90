      program broken_equiv
      real d (2)	! { dg-error "Inconsistent equivalence rules" "d" }
      real e		! { dg-error "Inconsistent equivalence rules" "e" }
      equivalence (d (1), e), (d (2), e)

      real f (2)	! { dg-error "Inconsistent equivalence rules" "f" }
      double precision g (2) ! { dg-error "Inconsistent equivalence rules" "g" }
      equivalence (f (1), g (1)), (f (2), g (2)) ! Not standard conforming
      end
