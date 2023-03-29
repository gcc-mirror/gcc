! All of the following (up to PROGRAM) are okay:
!
subroutine sub
  interface
    subroutine sub_iterface()
      !$omp assumes no_openmp_routines absent(simd) !  OK inferface of an external subroutine/subprogram
    end
  end interface
  !$omp assumes no_openmp_routines absent(simd) !  OK external subroutine/subprogram
contains
  subroutine inner_sub
     !$omp assumes no_parallelism absent(teams) ! OK internal subroutine/subprogram
  end
end

integer function func ()
  !$omp assumes no_openmp_routines absent(simd) !  OK external function/subprogram
  interface
    integer function func_iterface()
      !$omp assumes no_openmp_routines absent(simd) !  OK inferface of an external function/subprogram
    end
  end interface
  func = 0
contains
  integer function inner_func()
     !$omp assumes no_parallelism absent(teams) ! OK internal function/subprogram
     inner_sub2 = 0
  end
end

module m
  integer ::x 
  !$omp assumes contains(target) holds(x > 0.0)

    interface
      subroutine mod_mod_sub_iterface()
        !$omp assumes no_openmp_routines absent(simd) !  OK inferface of an external subroutine/subprogram
      end
      integer function mod_mod_func_iterface()
        !$omp assumes no_openmp_routines absent(error) !  OK inferface of an external subroutine/subprogram
      end
    end interface

contains
  subroutine mod_sub
    interface
      subroutine mod_sub_iterface()
        !$omp assumes no_openmp_routines absent(simd) !  OK inferface of an external subroutine/subprogram
      end
    end interface
    !$omp assumes no_openmp_routines absent(simd) !  OK module subroutine/subprogram
  contains
    subroutine mod_inner_sub
       !$omp assumes no_parallelism absent(teams) ! OK internal subroutine/subprogram
    end
  end

  integer function mod_func ()
    !$omp assumes no_openmp_routines absent(simd) !  OK module function/subprogram
    interface
      integer function mod_func_iterface()
        !$omp assumes no_openmp_routines absent(simd) !  OK inferface of an external function/subprogram
      end
    end interface
    mod_func = 0
  contains
    integer function mod_inner_func()
       !$omp assumes no_parallelism absent(teams) ! OK internal function/subprogram
       mod_inner_sub2 = 0
    end
  end
end module m


! PROGRAM - invalid as:
!  main program is a program unit that is not a subprogram
!$omp assumes no_openmp absent(simd)  ! { dg-error "must be in the specification part of a subprogram or module" }
  block
    ! invalid: block
    !$omp assumes no_openmp absent(target)  ! { dg-error "must be in the specification part of a subprogram or module" }
  end block
end
