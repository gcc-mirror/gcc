! { dg-do compile }

program main
  integer, parameter :: N = 100
  integer :: x = 0
  integer :: y = 0
  integer :: i

  ! Test implicit default directive
  !$omp metadirective &
  !$omp&	when (device={arch("nvptx")}: barrier)
    x = 1

  ! Test implicit default directive combined with a directive that takes a
  ! do loop.
  !$omp metadirective &
  !$omp&	when (device={arch("nvptx")}: parallel do)
    do i = 1, N
      x = x + i
    end do

  ! Test with multiple standalone directives.
  !$omp metadirective &
  !$omp&	when (device={arch("nvptx")}: barrier) &
  !$omp&	default (flush)
    x = 1

  ! Test combining a standalone directive with one that takes a do loop.
  !$omp metadirective &
  !$omp&	when (device={arch("nvptx")}: parallel do) &
  !$omp&	default (barrier)
    do i = 1, N
      x = x + i
    end do

  ! Test combining a directive that takes a do loop with one that takes
  ! a statement body.
  !$omp begin metadirective &
  !$omp&	when (device={arch("nvptx")}: parallel do) &
  !$omp&	default (parallel)
    do i = 1, N
      x = x + i
    end do
  !$omp end metadirective
  
  ! Test labels in the body.
  !$omp begin metadirective &
  !$omp&	when (device={arch("nvptx")}: parallel do) &
  !$omp&	when (device={arch("gcn")}: parallel)
    do i = 1, N
      x = x + i
      if (x .gt. N/2) goto 10
10    x = x + 1
      goto 20
      x = x + 2
20    continue
    end do
  !$omp end metadirective

  ! Test that commas are permitted before each clause.
  !$omp begin metadirective, &
  !$omp&	when (device={arch("nvptx")}: parallel do) &
  !$omp&	, when (device={arch("gcn")}: parallel) &
  !$omp&	, default (parallel)
    do i = 1, N
      x = x + i
    end do
  !$omp end metadirective

  ! Test empty metadirective.
  !$omp metadirective
end program
