subroutine nogroup_reduction
  integer :: i, r
  r = 0
!$omp taskloop nogroup reduction(+:r) ! { dg-error "'REDUCTION' clause at .1. must not be used together with 'NOGROUP' clause" }
  do i = 1, 32
    r = r + i
  end do
end
subroutine grainsize_num_tasks
  integer :: i
!$omp taskloop grainsize(2) num_tasks(2) ! { dg-error "'GRAINSIZE' clause at .1. must not be used together with 'NUM_TASKS' clause" }
  do i = 1, 32
  end do
end
