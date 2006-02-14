! { dg-do compile }

interface
  subroutine foo
  end subroutine
  function bar ()
  integer :: bar
  end function bar
  elemental function baz ()
  integer :: baz
  end function baz
end interface

  integer :: i, j
  real :: a, b (10), c
  a = 0.5
  b = 0.25
!$omp parallel workshare
  a = sin (a)
  b = sin (b)
  forall (i = 1:10) b(i) = cos (b(i)) - 0.5
  j = baz ()
!$omp parallel if (bar () .gt. 2) &
!$omp & num_threads (bar () + 1)
  i = bar ()
!$omp end parallel
!$omp parallel do schedule (static, bar () + 4)
  do j = 1, 10
    i = bar ()
  end do
!$omp end parallel do
!$omp end parallel workshare
!$omp parallel workshare
  call foo			! { dg-error "CALL statement" }
  i = bar ()			! { dg-error "non-ELEMENTAL" }
!$omp critical
  i = bar ()			! { dg-error "non-ELEMENTAL" }
!$omp end critical
!$omp atomic
  j = j + bar ()		! { dg-error "non-ELEMENTAL" }
!$omp end parallel workshare
end
