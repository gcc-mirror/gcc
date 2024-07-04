! { dg-do compile }
! { dg-options "-w" }
! PR fortran/104649
! Contributed by G.Steinmetz

module m
  interface
    module subroutine s(x)
      real :: x
    end
  end interface
end
submodule(m) m2
contains
  module subroutine s(*) ! { dg-error "conflicts with alternate return" }
  end
end

module n
  interface
     module subroutine s(*)
     end
  end interface
end
submodule(n) n2
contains
  module subroutine s(x) ! { dg-error "formal argument is alternate return" }
    real :: x
  end
end

module p
  interface
     module subroutine s(x)
       real :: x
     end
  end interface
end
submodule(p) p2
contains
  module subroutine s(y) ! { dg-error "Mismatch in MODULE PROCEDURE formal argument names" }
    real :: y
  end
end
