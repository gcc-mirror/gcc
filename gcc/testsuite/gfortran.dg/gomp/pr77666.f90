! PR fortran/77666
! { dg-do compile }

subroutine foo(x)
  interface
    subroutine baz(x, y)
      integer, allocatable :: x(:), y
    end subroutine
  end interface
  integer, allocatable :: x(:), y
!$omp parallel private(x, y)
  call baz (x, y)
!$omp end parallel
end
subroutine bar
  interface
    subroutine baz(x, y)
      integer, allocatable :: x(:), y
    end subroutine
  end interface
  integer, allocatable :: x(:), y
  call baz (x, y)
!$omp parallel private(x, y)
  call baz (x, y)
!$omp end parallel
end
