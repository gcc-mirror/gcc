! { dg-do compile }

program main
  integer, parameter :: N = 10
  integer, dimension(N) :: a
  integer, dimension(N) :: b
  integer, dimension(N) :: c
  integer :: i

  do i = 1, N
    a(i) = i * 2
    b(i) = i * 3
  end do

  !$omp metadirective &
  !$omp&	default (teams loop) &
  !$omp&	default (parallel loop)	! { dg-error "there can only be one default clause in a metadirective at .1." }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective default (xyz) ! { dg-error "Unclassifiable OpenMP directive at .1." }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective &
  !$omp&	default (teams loop) & ! { dg-error "expected 'default' or 'when' at .1." }
  !$omp&	where (device={arch("nvptx")}: parallel loop)
    do i = 1, N
      c(i) = a(i) * b(i)
    end do
    
  !$omp begin metadirective &
  !$omp&	when (device={arch("nvptx")}: parallel do) &
  !$omp&	default (barrier) ! { dg-error "variant directive used in OMP BEGIN METADIRECTIVE at .1. must have a corresponding end directive" }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do
  !$omp end metadirective ! { dg-error "Unexpected !OMP END METADIRECTIVE statement at .1." }
end program
