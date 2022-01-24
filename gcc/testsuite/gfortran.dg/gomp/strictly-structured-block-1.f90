! { dg-do compile }
! { dg-options "-fopenmp" }

program main
  integer :: x, i, n

  x = 0
  n = 10

  !$omp parallel
  block
    call do_work
  end block

  !$omp parallel
  block
    call do_work
  end block
  !$omp end parallel

  !$omp teams
  block
    call do_work
  end block

  !$omp teams
  block
    call do_work
  end block
  !$omp end teams

  !$omp masked
  block
    x = x + 1
  end block

  !$omp masked
  block
    x = x + 1
  end block
  !$omp end masked

  !$omp scope
  block
    call do_work
  end block

  !$omp scope
  block
    call do_work
  end block
  !$omp end scope

  !$omp single
  block
    x = x + 1
  end block

  !$omp single
  block
    x = x + 1
  end block
  !$omp end single

  !$omp workshare
  block
    x = x + 1
  end block

  !$omp workshare
  block
    x = x + 1
  end block
  !$omp end workshare

  !$omp task
  block
    call do_work
  end block

  !$omp task
  block
    call do_work
  end block
  !$omp end task

  !$omp target data map(x)
  block
    x = x + 1
  end block

  !$omp target data map(x)
  block
    x = x + 1
  end block
  !$omp end target data

  !$omp target
  block
    x = x + 1
  end block

  !$omp target
  block
    x = x + 1
  end block
  !$omp end target

  !$omp parallel workshare
  block
    x = x + 1
  end block

  !$omp parallel workshare
  block
    x = x + 1
  end block
  !$omp end parallel workshare

  !$omp parallel masked
  block
    x = x + 1
  end block

  !$omp parallel masked
  block
    x = x + 1
  end block
  !$omp end parallel masked

  !$omp target parallel
  block
    call do_work
  end block

  !$omp target parallel
  block
    call do_work
  end block
  !$omp end target parallel

  !$omp target teams
  block
    call do_work
  end block

  !$omp target teams
  block
    call do_work
  end block
  !$omp end target teams

  !$omp critical
  block
    x = x + 1
  end block

  !$omp critical
  block
    x = x + 1
  end block
  !$omp end critical

  !$omp taskgroup
  block
    x = x + 1
  end block

  !$omp taskgroup
  block
    x = x + 1
  end block
  !$omp end taskgroup

  !$omp do ordered
  do i = 1, n
     !$omp ordered
     block
       call do_work
     end block
  end do

  !$omp do ordered
  do i = 1, n
     !$omp ordered
     block
       call do_work
     end block
     !$omp end ordered
  end do

  !$omp master
  block
    x = x + 1
  end block

  !$omp master
  block
    x = x + 1
  end block
  !$omp end master

  !$omp parallel master
  block
    x = x + 1
  end block

  !$omp parallel master
  block
    x = x + 1
  end block
  !$omp end parallel master

end program
