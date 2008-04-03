! PR fortran/35786
! { dg-do compile }
! { dg-options "-fopenmp" }

module pr35768
  real, parameter :: one = 1.0
contains
  subroutine fn1
    !$omp parallel firstprivate (one)	! { dg-error "is not a variable" }
    !$omp end parallel
  end subroutine fn1
  subroutine fn2 (doit)
    external doit
    !$omp parallel firstprivate (doit)	! { dg-error "is not a variable" }
      call doit ()
    !$omp end parallel
  end subroutine fn2
  subroutine fn3
    interface fn4
      subroutine fn4 ()
      end subroutine fn4
    end interface
    !$omp parallel private (fn4)	! { dg-error "is not a variable" }
      call fn4 ()
    !$omp end parallel
  end subroutine fn3
  subroutine fn5
    interface fn6
      function fn6 ()
        integer :: fn6
      end function fn6
    end interface
    integer :: x
    !$omp parallel private (fn6, x)	! { dg-error "is not a variable" }
      x = fn6 ()
    !$omp end parallel
  end subroutine fn5
  function fn7 () result (re7)
    integer :: re7
    !$omp parallel private (fn7)	! { dg-error "is not a variable" }
    !$omp end parallel
  end function fn7
  function fn8 () result (re8)
    integer :: re8
    call fn9
  contains
    subroutine fn9
      !$omp parallel private (fn8)	! { dg-error "is not a variable" }
      !$omp end parallel
    end subroutine fn9
  end function fn8
  function fn10 () result (re10)
    integer :: re10, re11
    entry fn11 () result (re11)
    !$omp parallel private (fn10)	! { dg-error "is not a variable" }
    !$omp end parallel
    !$omp parallel private (fn11)	! { dg-error "is not a variable" }
    !$omp end parallel
  end function fn10
  function fn12 () result (re12)
    integer :: re12, re13
    entry fn13 () result (re13)
    call fn14
  contains
    subroutine fn14
      !$omp parallel private (fn12)	! { dg-error "is not a variable" }
      !$omp end parallel
      !$omp parallel private (fn13)	! { dg-error "is not a variable" }
      !$omp end parallel
    end subroutine fn14
  end function fn12
end module

! { dg-final { cleanup-modules "pr35768" } }
