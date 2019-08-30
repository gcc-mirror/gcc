! PR fortran/88377
! { dg-do compile }

program pr88377
  call s(3)
contains
  subroutine s(n)
    integer :: n
    character(n), allocatable :: x
    x = 'abc'
    !$omp task
    print *, x, (x == 'abc')
    !$omp end task
  end
end
