subroutine foo
  integer :: i, j
  !$omp taskloop
  do i = 0, 63
      !$omp do			! { dg-error "region may not be closely nested inside of" }
      do j = 0, 9
      end do
      !$omp single		! { dg-error "region may not be closely nested inside of" }
      !$omp end single
      !$omp sections		! { dg-error "region may not be closely nested inside of" }
        !$omp section
        block
        end block
      !$omp end sections
      !$omp barrier		! { dg-error "region may not be closely nested inside of" }
      !$omp master		! { dg-error "region may not be closely nested inside of" }  -- ?
        block; end block ! otherwise not generated
      !$omp end master
      !$omp masked		! { dg-error "region may not be closely nested inside of" }  -- ?
        block; end block ! otherwise not generated
      !$omp end masked
      !$omp scope			! { dg-error "region may not be closely nested inside of" }  -- ?
        block; end block ! otherwise not generated
      !$omp end scope
      !$omp ordered		! { dg-error "region may not be closely nested inside of" }
      !$omp end ordered
      !$omp ordered threads	! { dg-error "region may not be closely nested inside of" }
      !$omp end ordered
      !$omp ordered simd threads	! { dg-error ".ordered. .simd. must be closely nested inside .simd. region" }
      !$omp end ordered
      !$omp simd
      do j = 0, 9
        !$omp ordered simd
        !$omp end ordered
      end do
      !$omp critical
        !$omp simd
        do j = 0, 9
          !$omp ordered simd
          !$omp end ordered
        end do
      !$omp end critical
  end do
  !$omp taskloop
  do i = 0, 63
    !$omp parallel
      !$omp do
      do j = 0, 9
      end do
      !$omp single
      !$omp end single
      !$omp sections
        !$omp section
        block; end block
      !$omp end sections
      !$omp barrier
      !$omp master
        block; end block ! otherwise not generated
      !$omp end master
      !$omp masked
        block; end block ! otherwise not generated
      !$omp end masked
      !$omp scope
        block; end block ! otherwise not generated
      !$omp end scope
      !$omp ordered		! { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" }
      !$omp end ordered
      !$omp ordered threads	! { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" }
      !$omp end ordered
      !$omp simd
      do j = 0, 9
        !$omp ordered simd
        !$omp end ordered
      end do
      !$omp critical
        !$omp simd
        do j = 0, 9
          !$omp ordered simd
          !$omp end ordered
        end do
      !$omp end critical
    !$omp end parallel
  end do
  !$omp taskloop
  do i = 0, 63
    !$omp target
      !$omp do
      do j = 0, 9
      end do
      !$omp single
      !$omp end single
      !$omp sections
        !$omp section
        block; end block
      !$omp end sections
      !$omp barrier
      !$omp master
        block; end block ! otherwise not generated
      !$omp end master
      !$omp masked
        block; end block ! otherwise not generated
      !$omp end masked
      !$omp scope
        block; end block ! otherwise not generated
      !$omp end scope
      !$omp ordered		! { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" }
      !$omp end ordered
      !$omp ordered threads	! { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" }
      !$omp end ordered
      !$omp simd
      do j = 0, 9
        !$omp ordered simd
        !$omp end ordered
      end do
      !$omp critical
        !$omp simd
        do j = 0, 9
          !$omp ordered simd
          !$omp end ordered
        end do
      !$omp end critical
    !$omp end target
  end do
  !$omp ordered
    !$omp ordered			! { dg-error "region may not be closely nested inside of" }
    !$omp end ordered
  !$omp end ordered
  !$omp ordered threads
    !$omp ordered			! { dg-error "region may not be closely nested inside of" }
    !$omp end ordered
  !$omp end ordered
  !$omp ordered
    !$omp ordered threads		! { dg-error "region may not be closely nested inside of" }
    !$omp end ordered
  !$omp end ordered
  !$omp ordered threads
    !$omp ordered threads		! { dg-error "region may not be closely nested inside of" }
    !$omp end ordered
  !$omp end ordered
  !$omp critical
    !$omp ordered simd		! { dg-error ".ordered. .simd. must be closely nested inside .simd. region" }
    !$omp end ordered
  !$omp end critical
  !$omp do ordered
  do i = 0, 63
    !$omp parallel
      !$omp ordered threads	! { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" }
      !$omp end ordered
    !$omp end parallel
  end do
  !$omp do ordered
  do i = 0, 63
    !$omp parallel
      !$omp ordered		! { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" }
      !$omp end ordered
    !$omp end parallel
  end do
  !$omp do ordered(1)
  do i = 0, 63
    !$omp parallel
      !$omp ordered depend(source)	! { dg-error ".ordered. construct with .depend. clause must be closely nested inside a loop with .ordered. clause" }
      !$omp ordered depend(sink: i - 1)	! { dg-error ".ordered. construct with .depend. clause must be closely nested inside a loop with .ordered. clause" }
    !$omp end parallel
  end do
end
