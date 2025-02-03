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
  !$omp&	default (parallel loop)	! { dg-error "too many 'otherwise' or 'default' clauses in 'metadirective' at .1." }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective &
  !$omp&	otherwise (teams loop) &
  !$omp&	default (parallel loop)	! { dg-error "too many 'otherwise' or 'default' clauses in 'metadirective' at .1." }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective &
  !$omp&	otherwise (teams loop) &
  !$omp&	otherwise (parallel loop)	! { dg-error "too many 'otherwise' or 'default' clauses in 'metadirective' at .1." }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective default (xyz) ! { dg-error "Unclassifiable OpenMP directive at .1." }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective &
  !$omp&	default (teams loop) &
  !$omp&	where (device={arch("nvptx")}: parallel loop) ! { dg-error "expected 'when', 'otherwise', or 'default' at .1." }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective &
  !$omp&	otherwise (teams loop) &
  !$omp&	when (device={arch("nvptx")}: parallel loop) ! { dg-error "'otherwise' or 'default' clause must appear last" }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  !$omp metadirective &
  !$omp&	when (device={arch("nvptx")} parallel loop) & ! { dg-error "expected .:." } 
  !$omp&	default (teams loop)
    do i = 1, N
      c(i) = a(i) * b(i)
    end do

  ! Test improperly nested metadirectives - even though the second
  ! metadirective resolves to 'omp nothing', that is not the same as there
  ! being literally nothing there.
  !$omp metadirective &
  !$omp&    when (implementation={vendor("gnu")}: parallel do)
    !$omp metadirective &
    !$omp& when (implementation={vendor("cray")}: parallel do) ! { dg-error "Unexpected !.OMP METADIRECTIVE statement" }
      do i = 1, N
        c(i) = a(i) * b(i)
      end do

!$omp begin metadirective &
  !$omp&	when (device={arch("nvptx")}: parallel do) &
  !$omp&	default (barrier) ! { dg-error "variant directive used in OMP BEGIN METADIRECTIVE at .1. must have a corresponding end directive" }
    do i = 1, N
      c(i) = a(i) * b(i)
    end do
  !$omp end metadirective ! { dg-error "Unexpected !.OMP END METADIRECTIVE statement at .1." }
end program
