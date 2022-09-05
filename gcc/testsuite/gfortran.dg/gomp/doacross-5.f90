subroutine foo (n)
  integer i, n

  !$omp do ordered
  do i = 1, 8, n
    !$omp ordered doacross(source:)
    !$omp ordered doacross(sink: i - 2)
  end do
end

subroutine bar (n)
  integer :: i, j, n

  !$omp do collapse(2) ordered(2)
  do i = 1, 8, n
    do j = 1, 8, n
      !$omp ordered doacross(source:omp_cur_iteration)
      !$omp ordered doacross(sink: i - 2, j + 2)
    end do
  end do
end

subroutine baz ()
  integer :: i, j

  !$omp do ordered(1)
  do i = 1, 64
    !$omp ordered			! { dg-error "'ordered' construct without 'doacross' or 'depend' clauses must not have the same binding region as 'ordered' construct with those clauses" }
    !$omp end ordered

    !$omp ordered doacross(source:)

    !$omp ordered doacross(sink: i - 1)
  end do

  !$omp do ordered
  do i = 1, 64
    !$omp ordered doacross(source: omp_cur_iteration )

    !$omp ordered doacross(sink: i - 1)

    !$omp ordered threads		! { dg-error "'ordered' construct without 'doacross' or 'depend' clauses must not have the same binding region as 'ordered' construct with those clauses" }
    !$omp end ordered
  end do
  !$omp do ordered(2)
  do i = 1, 64
    do j = 1, 64
	!$omp ordered			! { dg-error "'ordered' construct without 'doacross' or 'depend' clauses binds to loop where 'collapse' argument 1 is different from 'ordered' argument 2" }
	!$omp end ordered
    end do
  end do
  !$omp do ordered(2) collapse(1)
  do i = 1, 8
    do j = 1, 8
      !$omp ordered threads		! { dg-error "'ordered' construct without 'doacross' or 'depend' clauses binds to loop where 'collapse' argument 1 is different from 'ordered' argument 2" }
      !$omp end ordered
    end do
  end do
end

subroutine qux ()
  integer :: i, j
  j = 0
  !$omp do ordered linear(j)
  do i = 1, 64
    j = j + 1
    !$omp ordered
    !$omp end ordered
  end do
  !$omp do ordered linear(j)		! { dg-error "'linear' clause may not be specified together with 'ordered' clause if stand-alone 'ordered' construct is nested in it" }
  do i = 1, 64
    j = j + 1
    !$omp ordered doacross(source:)
    !$omp ordered doacross(sink:i-1)
  end do
  !$omp do ordered(1) linear(j)
  do i = 1, 64
    j = j + 1
    !$omp ordered
    !$omp end ordered
  end do
  !$omp do ordered(1) linear(j)		! { dg-error "'linear' clause may not be specified together with 'ordered' clause if stand-alone 'ordered' construct is nested in it" }
  do i = 1, 64
    j = j + 1
    !$omp ordered doacross(source:)
    !$omp ordered doacross(sink:i-1)
  end do
end
