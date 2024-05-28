subroutine bar2
  block
    !$omp requires unified_shared_memory ! { dg-error "must appear in the specification part of a program unit" }
  end block
end

subroutine bar
contains
  subroutine foo
    !$omp requires unified_shared_memory ! { dg-error "must appear in the specification part of a program unit" }
  end
end

module m
contains
  subroutine foo
    !$omp requires unified_shared_memory ! { dg-error "must appear in the specification part of a program unit" }
  end
end

module m2
 interface
  module subroutine foo()
  end
 end interface
end

submodule (m2) m2_sub
    !$omp requires unified_shared_memory
contains
  module procedure foo
  end
end

program main
contains
  subroutine foo
    !$omp requires unified_shared_memory ! { dg-error "must appear in the specification part of a program unit" }
  end
end
