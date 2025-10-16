! { dg-do compile }

! This test case checks that a function call in a context selector is accepted.

module m
  implicit none (type, external)
contains
  integer function f(n)
    integer :: i, n
    f = 0
    !$omp metadirective &
    !$omp&  when(user={condition(use_target())}: target parallel do map(f) reduction(+:f)) &
    !$omp&  otherwise(parallel do reduction(+:f))
    do i = 1, n
      f = f + 1
    end do
  end
  logical function use_target()
    use_target = .false.
  end
end
