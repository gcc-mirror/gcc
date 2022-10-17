module m
  use iso_c_binding, only: c_loc
  implicit none (type, external)
  integer :: v
  interface
    subroutine foo (); end
    integer function omp_get_thread_num (); end
    integer function omp_get_num_threads (); end
    integer function omp_get_cancellation (); end
    integer(c_int) function omp_target_is_present(ptr, device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int
      type(c_ptr), value :: ptr
      integer(c_int), value :: device_num
    end
  end interface

contains
subroutine f1(a)
  integer :: a(0:)
  integer :: i, j
  !$omp simd order(concurrent)
    do i = 0, 63
      !$omp loop
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
end

subroutine f2 (a)
  integer :: a(0:)
  integer :: i, j
  !$omp do simd order(concurrent)
    do i = 0, 63
      !$omp loop
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do    
end

subroutine f3 (a)
  integer :: a(0:)
  integer :: i, j
  !$omp do order(concurrent)
    do i = 0, 63
      !$omp loop
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
end

subroutine f4 (a)
  integer, target :: a(0:)
  integer :: i, j
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp parallel
        call foo ()
      !$omp end parallel
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp simd
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp loop
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp critical  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end critical
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp ordered simd  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end ordered
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp atomic  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      v = v + 1
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp atomic read   ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      a(i) = v
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      !$omp atomic write  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      v = a(i)
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      a(i) = a(i) + omp_target_is_present (c_loc (a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop order(concurrent) bind(parallel)
    do i = 0, 63
      a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
end

subroutine f5 (a)
 integer, target :: a(0:)
 integer :: i, j
 !$omp parallel
  !$omp loop
    do i = 0, 63
      !$omp parallel
        call foo ()
      !$omp end parallel
    end do
  !$omp loop
    do i = 0, 63
      !$omp simd
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
  !$omp loop
    do i = 0, 63
      !$omp loop
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
  !$omp loop
    do i = 0, 63
      !$omp critical  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end critical
    end do
  !$omp loop
    do i = 0, 63
      !$omp ordered simd  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end ordered
    end do
  !$omp loop
    do i = 0, 63
      !$omp atomic  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      v = v + 1
    end do
  !$omp loop
    do i = 0, 63
      !$omp atomic read  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      a(i) = v
    end do
  !$omp loop
    do i = 0, 63
      !$omp atomic write  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      v = a(i)
    end do
  !$omp loop
    do i = 0, 63
      !$omp master  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end master
    end do
  !$omp loop
    do i = 0, 63
      !$omp masked  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end masked
    end do
  !$omp loop
    do i = 0, 63
      !$omp scope  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end scope
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_target_is_present (c_loc(a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp end parallel
end

subroutine f6 (a)
 integer, target :: a(0:)
 integer :: i, j
 !$omp master
  !$omp loop
    do i = 0, 63
      !$omp parallel
        call foo ()
      !$omp end parallel
    end do
  !$omp loop
    do i = 0, 63
      !$omp simd
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
  !$omp loop
    do i = 0, 63
      !$omp loop
      do j = 0, 63
        a(64 * i + j) = i + j
      end do
    end do
  !$omp loop
    do i = 0, 63
      !$omp critical  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end critical
    end do
  !$omp loop
    do i = 0, 63
      !$omp ordered simd  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
        call foo ()
      !$omp end ordered
    end do
  !$omp loop
    do i = 0, 63
      !$omp atomic  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      v = v + 1
    end do
  !$omp loop
    do i = 0, 63
      !$omp atomic read  !  { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      a(i) = v
    end do
  !$omp loop
    do i = 0, 63
      !$omp atomic write  ! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" }
      v = a(i)
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_target_is_present (c_loc(a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
  !$omp loop
    do i = 0, 63
      a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
    end do
 !$omp end master
end
end module
