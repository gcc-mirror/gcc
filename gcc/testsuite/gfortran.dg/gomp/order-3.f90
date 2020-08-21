module my_omp_mod
 use iso_c_binding, only: c_loc
 implicit none
 integer :: v
 interface
   integer function omp_get_thread_num () bind(C)
   end
   integer function omp_get_num_threads () bind(C)
   end
   integer function omp_get_cancellation () bind(C)
   end
   integer function omp_target_is_present (ptr, device_num) bind(C)
     use iso_c_binding, only: c_ptr
     type(c_ptr), value :: ptr
     integer :: device_num
   end
  end interface
contains
  subroutine foo ()
  end
end 

subroutine f1 (a, b)
  use my_omp_mod
  implicit none
  integer :: a(:), b(:,:)
  target :: a
  integer i, j
  !$omp simd order(concurrent)
  do i = 1, 64
    !$omp parallel		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
    call foo ()
    !$omp end parallel
  end do
  !$omp end simd
  !$omp simd order(concurrent)
  do i = 1, 64
    !$omp simd
    do j = 1, 64
      b(j, i) = i + j
    end do
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
      !$omp critical		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
      call foo ()
      !$omp end critical
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
    !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    call foo ()
    !$omp end ordered
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
    !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    v = v + 1
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
    !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause"  }
    a(i) = v
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
      !$omp atomic write	! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
      v = a(i)
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc(a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end

subroutine f2 (a, b)
  use my_omp_mod
  implicit none
  integer a(:), b(:,:)
  target :: a
  integer i, j
  !$omp do simd order(concurrent)
  do i = 1, 64
    !$omp parallel		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
    call foo ()
    !$omp end parallel
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    !$omp simd
    do j = 1, 64
      b (j, i) = i + j
    end do
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    !$omp critical		! { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" }
    call foo ()
    !$omp end critical
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    call foo ()
    !$omp end ordered
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    v = v + 1
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    a(i) = v
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    v = a(i)
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc(a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do simd order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end

subroutine f3 (a, b)
  use my_omp_mod
  implicit none
  integer :: a(:), b(:,:)
  target :: a
  integer i, j
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp parallel
    call foo ()
    !$omp end parallel
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp simd
    do j = 1, 64
      b(j, i) = i + j
    end do
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp critical		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    call foo ()
    !$omp end critical
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp ordered simd		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    call foo ()
    !$omp end ordered
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp atomic		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    v = v + 1
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp atomic read		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    a(i) = v
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp atomic write		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    v = a(i)
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp task			! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    a(i) = a(i) + 1
    !$omp end task
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    !$omp taskloop		! { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" }
    do j = 1, 64
      b(j, i) = i + j
    end do
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_thread_num ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_num_threads ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_target_is_present (c_loc(a(i)), 0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
  !$omp do order(concurrent)
  do i = 1, 64
    a(i) = a(i) + omp_get_cancellation ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" }
  end do
end
