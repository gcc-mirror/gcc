module m
  use iso_c_binding
  implicit none (type, external)
  interface
    subroutine foo()
    end subroutine foo
    integer function omp_get_thread_num ()
    end
    integer function omp_get_num_threads ()
    end
    integer function omp_target_is_present (x, i)
      import :: c_ptr
      type(c_ptr) :: x
      integer, value :: i
    end
    integer function omp_get_cancellation ()
    end
  end interface
  integer :: v
contains
subroutine f1 (a)
  integer, target :: a(*)
  integer :: i
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp parallel		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end parallel
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp simd
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp critical		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end critical
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end ordered
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = v + 1
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = v
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = a(i)
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc (a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end

subroutine f2 (a)
  integer, target :: a(*)
  integer :: i
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp parallel		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end parallel
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp simd
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp critical		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end critical
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end ordered
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = v + 1
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = v
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = a(i)
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads () ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc(a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end

subroutine f3 (a)
  integer, target :: a(*)
  integer :: i
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
      !$omp parallel
      call foo ()
      !$omp end parallel
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp simd
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
      !$omp critical		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end critical
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
      !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end ordered
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = v + 1
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = v
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
      !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = a(i)
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
      !$omp task			! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = a(i) + 1
      !$omp end task
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp taskloop		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc (a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(reproducible:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end

subroutine f4 (a)
  integer, target :: a(*)
  integer :: i
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp parallel		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end parallel
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp simd
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp critical		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end critical
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end ordered
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = v + 1
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = v
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = a(i)
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc (a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end

subroutine f5 (a)
  integer, target :: a(*)
  integer :: i
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp parallel		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end parallel
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp simd
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp critical		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end critical
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end ordered
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = v + 1
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = v
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = a(i)
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc (a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end

subroutine f6 (a)
  integer, target :: a(*)
  integer :: i
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
      !$omp parallel
      call foo ()
      !$omp end parallel
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp simd
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
      !$omp critical		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end critical
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
      !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      call foo ()
      !$omp end ordered
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = v + 1
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = v
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
      !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = a(i)
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
      !$omp task			! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      a(i) = a(i) + 1
      !$omp end task
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
    block
      integer j
      !$omp taskloop		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      do j = 1, 64
        a(64 * i + j) = i + j
      end do
    end block
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc (a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(unconstrained:concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end
end module m
