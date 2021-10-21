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
  call do_work
  !$omp end parallel ! { dg-error "Unexpected !.OMP END PARALLEL statement" }

  !$omp teams
  block
    call do_work
  end block
  call do_work
  !$omp end teams ! { dg-error "Unexpected !.OMP END TEAMS statement" }

  !$omp masked
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end masked ! { dg-error "Unexpected !.OMP END MASKED statement" }

  !$omp scope
  block
    call do_work
  end block
  call do_work
  !$omp end scope ! { dg-error "Unexpected !.OMP END SCOPE statement" }

  !$omp single
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end single ! { dg-error "Unexpected !.OMP END SINGLE statement" }

  !$omp workshare
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end workshare ! { dg-error "Unexpected !.OMP END WORKSHARE statement" }

  !$omp task
  block
    call do_work
  end block
  call do_work
  !$omp end task ! { dg-error "Unexpected !.OMP END TASK statement" }

  !$omp target data map(x)
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end target data ! { dg-error "Unexpected !.OMP END TARGET DATA statement" }

  !$omp target
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end target ! { dg-error "Unexpected !.OMP END TARGET statement" }

  !$omp parallel workshare
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end parallel workshare ! { dg-error "Unexpected !.OMP END PARALLEL WORKSHARE statement" }

  !$omp parallel masked
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end parallel masked ! { dg-error "Unexpected !.OMP END PARALLEL MASKED statement" }

  !$omp target parallel
  block
    call do_work
  end block
  call do_work
  !$omp end target parallel ! { dg-error "Unexpected !.OMP END TARGET PARALLEL statement" }

  !$omp target teams
  block
    call do_work
  end block
  call do_work
  !$omp end target teams ! { dg-error "Unexpected !.OMP END TARGET TEAMS statement" }

  !$omp critical
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end critical ! { dg-error "Unexpected !.OMP END CRITICAL statement" }

  !$omp taskgroup
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end taskgroup ! { dg-error "Unexpected !.OMP END TASKGROUP statement" }

  !$omp do ordered
  do i = 1, n
     !$omp ordered
     block
       call do_work
     end block
     call do_work
     !$omp end ordered ! { dg-error "Unexpected !.OMP END ORDERED statement" }
  end do

  !$omp master
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end master ! { dg-error "Unexpected !.OMP END MASTER statement" }

  !$omp parallel master
  block
    x = x + 1
  end block
  x = x + 1
  !$omp end parallel master ! { dg-error "Unexpected !.OMP END PARALLEL MASTER statement" }

end program
