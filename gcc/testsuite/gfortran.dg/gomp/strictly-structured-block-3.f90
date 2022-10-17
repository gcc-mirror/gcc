! { dg-do compile }
! { dg-options "-fopenmp" }

program main
  integer :: x, y

  x = 0
  y = 0

  !$omp parallel
    !$omp parallel
    block
      call do_work
    end block
    !$omp end parallel
  !$omp end parallel

  !$omp workshare
  block
    x = 1
    !$omp critical
    block
      y = 3
    end block
  end block

  !$omp sections
  block
    !$omp section
    block
      x = 1
    end block
    x = x + 2
    !$omp section
    call do_work
  end block

  !$omp sections
    !$omp section
    block
    end block
    x = 1
  !$omp end sections

  !$omp sections
  block
    block
    end block
    x = 1
  end block

end program main
