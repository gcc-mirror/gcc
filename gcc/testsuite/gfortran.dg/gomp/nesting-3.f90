subroutine f1
  integer i, j
  !$omp do
  do i = 0, 2
      !$omp do		! { dg-error "may not be closely nested" }
      do j = 0, 2
        block; end block
      end do
      !$omp sections	! { dg-error "may not be closely nested" }
        block; end block
        !$omp section
        block; end block
      !$omp end sections
      !$omp single	! { dg-error "may not be closely nested" }
        block; end block
      !$omp end single
      !$omp master	! { dg-error "may not be closely nested" }
        block; end block
      !$omp end master
      !$omp masked	! { dg-error "may not be closely nested" }
        block; end block
      !$omp end masked
      !$omp barrier	! { dg-error "may not be closely nested" }
      !$omp scope		! { dg-error "may not be closely nested" }
        block; end block
      !$omp end scope
  end do
  !$omp sections
    !$omp do		! { dg-error "may not be closely nested" }
    do j = 0, 2
      block; end block
    end do
  !$omp end sections
  !$omp sections
    !$omp sections	! { dg-error "may not be closely nested" }
      block; end block
    !$omp section
      block; end block
    !$omp end sections
  !$omp end sections
  !$omp sections
    !$omp single		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end single
  !$omp end sections
  !$omp sections
    !$omp master		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end master
  !$omp end sections
  !$omp sections
    !$omp masked		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end masked
  !$omp end sections
  !$omp sections
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end sections
  !$omp sections
    !$omp section
      block; end block
  !$omp end sections
  !$omp sections
    !$omp section
    !$omp do		! { dg-error "may not be closely nested" }
    do j = 0, 2
      block; end block
    end do
  !$omp end sections
  !$omp sections
    !$omp section
    !$omp sections	! { dg-error "may not be closely nested" }
      block; end block
    !$omp section
      block; end block
    !$omp end sections
  !$omp end sections
  !$omp sections
    !$omp section
    !$omp single		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end single
  !$omp end sections
  !$omp sections
    !$omp section
    !$omp master		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end master
    !$omp section
    !$omp masked		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end masked
  !$omp end sections
  !$omp sections
    !$omp section
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end sections
  !$omp single
    !$omp do		! { dg-error "may not be closely nested" }
    do j = 0, 2
      block; end block
    end do
    !$omp sections	! { dg-error "may not be closely nested" }
      block; end block
    !$omp section
      block; end block
    !$omp end sections
    !$omp single		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end single
    !$omp master		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end master
    !$omp masked		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end masked
    !$omp barrier		! { dg-error "may not be closely nested" }
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end single
  !$omp master
    !$omp do		! { dg-error "may not be closely nested" }
    do j = 0, 2
      block; end block
    end do
    !$omp sections	! { dg-error "may not be closely nested" }
      block; end block
    !$omp section
      block; end block
    !$omp end sections
    !$omp single		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end single
    !$omp master
      block; end block
    !$omp end master
    !$omp barrier		! { dg-error "may not be closely nested" }
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end master
  !$omp masked filter (1)
    !$omp do		! { dg-error "may not be closely nested" }
    do j = 0, 2
      block; end block
    end do
    !$omp sections	! { dg-error "may not be closely nested" }
      block; end block
    !$omp section
      block; end block
    !$omp end sections
    !$omp single		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end single
    !$omp master
      block; end block
    !$omp end master
    !$omp barrier		! { dg-error "may not be closely nested" }
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end masked
  !$omp task
    !$omp do		! { dg-error "may not be closely nested" }
    do j = 0, 2
      block; end block
    end do
    !$omp sections	! { dg-error "may not be closely nested" }
      block; end block
    !$omp section
      block; end block
    !$omp end sections
    !$omp single		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end single
    !$omp master		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end master
    !$omp masked		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end masked
    !$omp barrier		! { dg-error "may not be closely nested" }
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end task
  !$omp parallel
    !$omp do
    do j = 0, 2
      block; end block
    end do
    !$omp sections
      block; end block
    !$omp section
      block; end block
    !$omp end sections
    !$omp single
      block; end block
    !$omp end single
    !$omp master
      block; end block
    !$omp end master
    !$omp masked
      block; end block
    !$omp end masked
    !$omp barrier
    !$omp scope
      block; end block
    !$omp end scope
    !$omp scope
      !$omp scope
        block; end block
      !$omp end scope
    !$omp end scope
  !$omp end parallel
  !$omp scope
    !$omp do
    do j = 0, 2
      block; end block
    end do
    !$omp sections
      block; end block
    !$omp section
      block; end block
    !$omp end sections
    !$omp single
      block; end block
    !$omp end single
    !$omp master
      block; end block
    !$omp end master
    !$omp masked
      block; end block
    !$omp end masked
    !$omp barrier
    !$omp scope
      block; end block
    !$omp end scope
    !$omp scope
      !$omp scope
        block; end block
      !$omp end scope
    !$omp end scope
  !$omp end scope
end

subroutine f2
  integer i, j
  !$omp ordered
    !$omp do		! { dg-error "may not be closely nested" }
    do j = 0, 2
      block; end block
    end do
    !$omp sections	! { dg-error "may not be closely nested" }
      block; end block
      !$omp section
      block; end block
    !$omp end sections
    !$omp single		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end single
    !$omp master
      block; end block
    !$omp end master
    !$omp masked
      block; end block
    !$omp end masked
    !$omp barrier		! { dg-error "may not be closely nested" }
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end ordered
end

subroutine f3 (void)
  !$omp critical
    !$omp ordered		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end ordered
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end critical
end

subroutine f4 (void)
  !$omp task
    !$omp ordered		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end ordered
    !$omp scope		! { dg-error "may not be closely nested" }
      block; end block
    !$omp end scope
  !$omp end task
end

subroutine f5 (void)
  integer i
  !$omp do
  do i = 0, 9
     !$omp ordered		! { dg-error "must be closely nested" }
       block; end block
     !$omp end ordered
  end do
  !$omp do ordered
  do i = 0, 9
     !$omp ordered
       block; end block
     !$omp end ordered
  end do
end

subroutine f6 (void)
  !$omp critical (foo)
    !$omp critical (bar)
      block; end block
    !$omp end critical (bar)
  !$omp end critical (foo)
  !$omp critical
    !$omp critical (baz)
      block; end block
    !$omp end critical (baz)
  !$omp end critical
end

subroutine f7 (void)
  !$omp critical (foo2)
    !$omp critical
      block; end block
    !$omp end critical
  !$omp end critical (foo2)
  !$omp critical (bar)
    !$omp critical (bar)		! { dg-error "may not be nested" }
      block; end block
    !$omp end critical (bar)
  !$omp end critical (bar)
  !$omp critical
    !$omp critical		! { dg-error "may not be nested" }
      block; end block
    !$omp end critical
  !$omp end critical
end
