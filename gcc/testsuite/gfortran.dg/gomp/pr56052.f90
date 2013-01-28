! PR fortran/56052
! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine middle(args)
  type args_t
  end type
  type, extends(args_t) :: scan_args_t
  end type
  class(args_t),intent(inout) :: args
  !$omp single
    select type (args)
      type is (scan_args_t)
    end select
  !$omp end single
end subroutine middle
