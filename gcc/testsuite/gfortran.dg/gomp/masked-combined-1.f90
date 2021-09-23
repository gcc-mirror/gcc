subroutine foo (a, f)
  implicit none (type, external)
  interface
    subroutine bar (x)
      integer :: x
    end subroutine
  end interface

  integer, value :: f
  integer, contiguous :: a(0:)
  integer :: i, j, k, u, v, w, x, y, z

  !$omp parallel masked default(none) private (k) filter (f) firstprivate (f)
    call bar (k)
  !$omp end parallel masked

  !$omp parallel masked default(none) private (k)
    call bar (k)
  !$omp end parallel masked
  
  !$omp parallel default(none) firstprivate(a, f) shared(x, y, z)
    !$omp masked taskloop reduction (+:x) default(none) firstprivate(a) filter (f)
      do i = 0, 63
        x = x + a(i)
      end do
    !$omp end masked taskloop
    !$omp masked taskloop simd reduction (+:y) default(none) firstprivate(a) private (i) filter (f)
      do i = 0, 63
        y = y + a(i)
      end do
    !$omp end masked taskloop simd
    !$omp masked taskloop simd reduction (+:y) default(none) firstprivate(a) private (i)
      do i = 0, 63
        y = y + a(i)
      end do
    !$omp end masked taskloop simd
    !$omp masked taskloop simd collapse(2) reduction (+:z) default(none) firstprivate(a) private (i, j) filter (f)
      do j = 0, 0
        do i = 0, 63
          z = z + a(i)
        end do
      end do
    !$omp end masked taskloop simd
  !$omp end parallel

  !$omp parallel masked taskloop reduction (+:u) default(none) firstprivate(a, f) filter (f)
    do i = 0, 63
      u = u + a(i)
    end do
  !$omp end parallel masked taskloop

  !$omp parallel masked taskloop simd reduction (+:v) default(none) firstprivate(a, f) filter (f)
    do i = 0, 63
      v = v + a(i)
    end do
  !$omp end parallel masked taskloop simd

  !$omp parallel masked taskloop simd collapse(2) reduction (+:w) default(none) firstprivate(a, f) filter (f)
    do j = 0, 0
      do i = 0, 63
        w = w + a(i)
      end do
    end do
  !$omp end parallel masked taskloop simd
end
