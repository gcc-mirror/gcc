! { dg-additional-options "-cpp" }

subroutine f1
  !$omp cancel parallel			! { dg-error "orphaned" }
  !$omp cancel do			! { dg-error "orphaned" }
  !$omp cancel sections			! { dg-error "orphaned" }
  !$omp cancel taskgroup			! { dg-error "orphaned" }
  !$omp cancellation point parallel	! { dg-error "orphaned" }
  !$omp cancellation point do		! { dg-error "orphaned" }
  !$omp cancellation point sections	! { dg-error "orphaned" }
  !$omp cancellation point taskgroup	! { dg-error "orphaned" }
end

subroutine f2
  integer :: i, j
  j = 0
  !$omp parallel
    !$omp cancel parallel
    !$omp cancel do			! { dg-error "not closely nested inside" }
    !$omp cancel sections			! { dg-error "not closely nested inside" }
    !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
    !$omp cancellation point parallel
    !$omp cancellation point do		! { dg-error "not closely nested inside" }
    !$omp cancellation point sections	! { dg-error "not closely nested inside" }
    !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }

    !$omp master
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end master

    !$omp masked
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end masked

    !$omp scope
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end scope

    !$omp single
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end single

    !$omp critical
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end critical

    !$omp taskgroup
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end taskgroup

    !$omp task
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "construct not closely nested inside of .taskgroup. region" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "construct not closely nested inside of .taskgroup. region" }
    !$omp end task

    !$omp taskgroup
    !$omp task
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup
    !$omp end task
    !$omp end taskgroup

    !$omp taskloop
    do i = 0, 9
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do			! { dg-error "not closely nested inside" }
        !$omp cancel sections		! { dg-error "not closely nested inside" }
        !$omp cancel taskgroup
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do	! { dg-error "not closely nested inside" }
        !$omp cancellation point sections	! { dg-error "not closely nested inside" }
        !$omp cancellation point taskgroup
        !$omp task
          !$omp cancellation point taskgroup
          !$omp cancel taskgroup
        !$omp end task
    end do
    !$omp taskloop nogroup
    do i = 0, 9
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do			! { dg-error "not closely nested inside" }
        !$omp cancel sections		! { dg-error "not closely nested inside" }
        !$omp cancel taskgroup		! { dg-error "construct not closely nested inside of .taskgroup. region" }
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do	! { dg-error "not closely nested inside" }
        !$omp cancellation point sections	! { dg-error "not closely nested inside" }
        !$omp cancellation point taskgroup! { dg-error "construct not closely nested inside of .taskgroup. region" }
        !$omp task
          !$omp cancellation point taskgroup! { dg-error "construct not closely nested inside of .taskgroup. region" }
          !$omp cancel taskgroup		! { dg-error "construct not closely nested inside of .taskgroup. region" }
        !$omp end task
    end do
    !$omp taskgroup
      !$omp task
        !$omp task
          !$omp cancellation point taskgroup
          !$omp cancel taskgroup
        !$omp end task
      !$omp end task
      !$omp taskloop nogroup
      do i = 0, 9
          !$omp task
            !$omp cancellation point taskgroup
            !$omp cancel taskgroup
          !$omp end task
          !$omp cancellation point taskgroup
          !$omp cancel taskgroup
      end do
    !$omp end taskgroup

    !$omp taskgroup
      !$omp parallel
        !$omp task
          !$omp cancel taskgroup		! { dg-error "construct not closely nested inside of .taskgroup. region" }
          !$omp cancellation point taskgroup ! { dg-error "construct not closely nested inside of .taskgroup. region" }
        !$omp end task
        !$omp taskloop
        do i = 0, 9
            !$omp cancel taskgroup
            !$omp cancellation point taskgroup
        end do
        !$omp taskloop nogroup
        do i = 0, 9
            !$omp cancel taskgroup	     ! { dg-error "construct not closely nested inside of .taskgroup. region" }
            !$omp cancellation point taskgroup ! { dg-error "construct not closely nested inside of .taskgroup. region" }
        end do
      !$omp end parallel
      !$omp target
        !$omp task
          !$omp cancel taskgroup		! { dg-error "construct not closely nested inside of .taskgroup. region" }
          !$omp cancellation point taskgroup ! { dg-error "construct not closely nested inside of .taskgroup. region" }
        !$omp end task
      !$omp end target
      !$omp target
      !$omp teams
      !$omp distribute
      do i = 0, 9
          !$omp task
            !$omp cancellation point taskgroup ! { dg-error "construct not closely nested inside of .taskgroup. region" }
          !$omp end task
      end do
      !$omp end distribute
      !$omp end teams
      !$omp end target
      !$omp target data map(i)
        !$omp task
          !$omp cancel taskgroup
          !$omp cancellation point taskgroup
        !$omp end task
      !$omp end target data
    !$omp end taskgroup

    !$omp taskloop
    do i = 0, 9
        !$omp parallel
          !$omp task
            !$omp cancel taskgroup	     ! { dg-error "construct not closely nested inside of .taskgroup. region" }
            !$omp cancellation point taskgroup ! { dg-error "construct not closely nested inside of .taskgroup. region" }
          !$omp end task
        !$omp end parallel
        !$omp target
          !$omp task
            !$omp cancel taskgroup	     ! { dg-error "construct not closely nested inside of .taskgroup. region" }
            !$omp cancellation point taskgroup ! { dg-error "construct not closely nested inside of .taskgroup. region" }
          !$omp end task
        !$omp end target
        !$omp target
        !$omp teams
        !$omp distribute
        do j = 0, 9
            !$omp task
              !$omp cancel taskgroup	! { dg-error "construct not closely nested inside of .taskgroup. region" }
              !$omp cancellation point taskgroup ! { dg-error "construct not closely nested inside of .taskgroup. region" }
            !$omp end task
        end do
        !$omp end distribute
        !$omp end teams
        !$omp end target
        !$omp target data map(i)
          !$omp task
            !$omp cancel taskgroup
            !$omp cancellation point taskgroup
          !$omp end task
        !$omp end target data
    end do

    !$omp do
    do i = 0, 9
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do
        !$omp cancel sections		! { dg-error "not closely nested inside" }
        !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do
        !$omp cancellation point sections	! { dg-error "not closely nested inside" }
        !$omp cancellation point taskgroup! { dg-error "not closely nested inside" }
    end do

    !$omp do ordered
    do i = 0, 9
      !$omp ordered
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do			! { dg-error "not closely nested inside" }
        !$omp cancel sections		! { dg-error "not closely nested inside" }
        !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do	! { dg-error "not closely nested inside" }
        !$omp cancellation point sections	! { dg-error "not closely nested inside" }
        !$omp cancellation point taskgroup! { dg-error "not closely nested inside" }
      !$omp end ordered
    end do
    !$omp end do
    !$omp sections
      block
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do			! { dg-error "not closely nested inside" }
        !$omp cancel sections
        !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do	! { dg-error "not closely nested inside" }
        !$omp cancellation point sections
        !$omp cancellation point taskgroup! { dg-error "not closely nested inside" }
      end block
      !$omp section
      block
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do			! { dg-error "not closely nested inside" }
        !$omp cancel sections
        !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do	! { dg-error "not closely nested inside" }
        !$omp cancellation point sections
        !$omp cancellation point taskgroup! { dg-error "not closely nested inside" }
      end block
    !$omp target data map(j)
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target data
    !$omp target
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target
  !$omp end sections
  !$omp end parallel
  !$omp target data map(j)
    !$omp cancel parallel			! { dg-error "not closely nested inside" }
    !$omp cancel do			! { dg-error "not closely nested inside" }
    !$omp cancel sections			! { dg-error "not closely nested inside" }
    !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
    !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
    !$omp cancellation point do		! { dg-error "not closely nested inside" }
    !$omp cancellation point sections	! { dg-error "not closely nested inside" }
    !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
  !$omp end target data
  !$omp target
    !$omp cancel parallel			! { dg-error "not closely nested inside" }
    !$omp cancel do			! { dg-error "not closely nested inside" }
    !$omp cancel sections			! { dg-error "not closely nested inside" }
    !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
    !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
    !$omp cancellation point do		! { dg-error "not closely nested inside" }
    !$omp cancellation point sections	! { dg-error "not closely nested inside" }
    !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
  !$omp end target
  !$omp target teams
    !$omp cancel parallel			! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
    !$omp cancel do			! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
    !$omp cancel sections			! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
    !$omp cancel taskgroup		! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
    !$omp cancellation point parallel	! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
    !$omp cancellation point do		! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
    !$omp cancellation point sections	! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
    !$omp cancellation point taskgroup	! { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" }
  !$omp end target teams
  !$omp target teams distribute
  do i = 0, 9
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
  end do
  !$omp end target teams distribute
  !$omp do
  do i = 0, 9
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
  end do
  !$omp do
  do i = 0, 9
    !$omp target data map(j)
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target data
  end do
  !$omp do
  do i = 0, 9
    !$omp target
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target
  end do
  !$omp do ordered
  do i = 0, 9
    !$omp ordered
      !$omp target data map(j)
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do			! { dg-error "not closely nested inside" }
        !$omp cancel sections		! { dg-error "not closely nested inside" }
        !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do	! { dg-error "not closely nested inside" }
        !$omp cancellation point sections	! { dg-error "not closely nested inside" }
        !$omp cancellation point taskgroup! { dg-error "not closely nested inside" }
      !$omp end target data
    !$omp end ordered
  end do
  do i = 0, 9
    !$omp ordered
      !$omp target
        !$omp cancel parallel		! { dg-error "not closely nested inside" }
        !$omp cancel do			! { dg-error "not closely nested inside" }
        !$omp cancel sections		! { dg-error "not closely nested inside" }
        !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
        !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
        !$omp cancellation point do	! { dg-error "not closely nested inside" }
        !$omp cancellation point sections	! { dg-error "not closely nested inside" }
        !$omp cancellation point taskgroup! { dg-error "not closely nested inside" }
      !$omp end target
    !$omp end ordered
  end do
  !$omp sections
    block
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    end block
    !$omp section
    block
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    end block
  !$omp end sections
  !$omp sections
    !$omp target data map(j)
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target data
    !$omp section
    !$omp target data map(j)
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target data
  !$omp end sections
  !$omp sections
    !$omp target
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target
    !$omp section
    !$omp target
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end target
  !$omp end sections
  !$omp task
    !$omp cancel parallel			! { dg-error "not closely nested inside" }
    !$omp cancel do			! { dg-error "not closely nested inside" }
    !$omp cancel sections			! { dg-error "not closely nested inside" }
    !$omp cancel taskgroup
    !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
    !$omp cancellation point do		! { dg-error "not closely nested inside" }
    !$omp cancellation point sections	! { dg-error "not closely nested inside" }
    !$omp cancellation point taskgroup
    !$omp taskgroup
      !$omp cancel parallel		! { dg-error "not closely nested inside" }
      !$omp cancel do			! { dg-error "not closely nested inside" }
      !$omp cancel sections		! { dg-error "not closely nested inside" }
      !$omp cancel taskgroup		! { dg-error "not closely nested inside" }
      !$omp cancellation point parallel	! { dg-error "not closely nested inside" }
      !$omp cancellation point do	! { dg-error "not closely nested inside" }
      !$omp cancellation point sections	! { dg-error "not closely nested inside" }
      !$omp cancellation point taskgroup	! { dg-error "not closely nested inside" }
    !$omp end taskgroup
  !$omp end task
end

subroutine f3
  integer i
  !$omp do
  do i = 0, 9
      !$omp cancel do		! { dg-warning "nowait" }
  end do
  !$omp end do nowait
  !$omp sections
    block
      !$omp cancel sections	! { dg-warning "nowait" }
    end block
    !$omp section
    block
      !$omp cancel sections	! { dg-warning "nowait" }
    end block
  !$omp end sections nowait
  !$omp do ordered
  do i = 0, 9
      !$omp cancel do		! { dg-warning "ordered" }
      !$omp ordered
      !$omp end ordered
  end do
end


subroutine f4
!  if (.false.) then
!$omp cancellation point do ! { dg-error "orphaned 'cancellation point' construct" }
!  end if
end
