! PR fortran/48117
! { dg-do compile }
! { dg-options "-O2 -fopenmp" }

subroutine foo(x)
  character(len=*), optional :: x
  character(len=80) :: v
  !$omp master
    if (present(x)) v = adjustl(x)
  !$omp end master
end subroutine foo
