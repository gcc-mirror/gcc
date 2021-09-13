! PR middle-end/100905
!
PROGRAM test_loop_order_concurrent
  implicit none
  integer :: a, cc(64), dd(64)

  dd = 54
  cc = 99

  call test_loop()
  call test_affinity(a)
  if (a /= 5) stop 3
  call test_scan(cc, dd)
  if (any (cc /= 99)) stop 4
  if (dd(1) /= 5  .or. dd(2) /= 104) stop 5

CONTAINS

  SUBROUTINE test_loop()
    INTEGER,DIMENSION(1024):: a, b, c
    INTEGER:: i

    DO i = 1, 1024
       a(i) = 1
       b(i) = i + 1
       c(i) = 2*(i + 1)
    END DO

   !$omp loop order(concurrent) bind(thread)
    DO i = 1, 1024
       a(i) = a(i) + b(i)*c(i)
    END DO

    DO i = 1, 1024
       if (a(i) /= 1 + (b(i)*c(i))) stop 1
    END DO
  END SUBROUTINE test_loop

  SUBROUTINE test_affinity(aa)
    integer :: aa
    !$omp task affinity(aa)
      a = 5
    !$omp end task
  end 

  subroutine test_scan(c, d)
    integer i, c(*), d(*)
    !$omp simd reduction (inscan, +: a)
    do i = 1, 64
      d(i) = a
      !$omp scan exclusive (a)
      a = a + c(i)
    end do
  end
END PROGRAM test_loop_order_concurrent
