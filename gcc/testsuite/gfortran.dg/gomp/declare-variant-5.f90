! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-additional-options "-mavx2" }

module main
  implicit none
contains
  function f1 (x, y, z)
    integer, dimension(4) :: f1
    real, dimension(4), intent(in) :: x, y
    real, intent(out) :: z

    f1 = x
  end function

  function f2 (x, y, z)
    integer, dimension(8) :: f2
    real, dimension(8), intent(in) :: x, y
    real, intent(out) :: z

    f2 = x
  end function

  function f3 (x, y, z)
    integer, dimension(4) :: f3
    real, dimension(4), intent(in) :: x, z
    integer, intent(in) :: y

    f3 = x
  end function

  integer function f4 (x, y, z)
    real, intent(in) :: x, y
    real, intent(out) :: z
    !$omp declare variant (f1) match (construct={parallel,do,simd(simdlen(4),notinbranch,uniform(z),aligned(z:16))})
    !$omp declare variant (f2) match (construct={do,simd(uniform(z),simdlen(8),notinbranch)})
  end function

  integer function f5 (x, y)
    integer, intent(in) :: x, y
    !$omp declare variant (f3) match (construct={simd(simdlen(4),inbranch,linear(y:1))})
  end function

  subroutine test (x, y, z, w)
    integer, dimension(8192), intent(inout) :: x
    real, dimension(8192), intent(inout) :: y, z
    real, pointer, intent(out) :: w
    integer :: i

    !$omp parallel
    !$omp do simd aligned (w:16)
    do i = 1, 1024
      x(i) = f4 (y(i), z(i), w)
    end do
    !$omp end do simd
    !$omp end parallel

    !$omp parallel do simd aligned (w:16) simdlen(4)
    do i = 1025, 2048
      x(i) = f4 (y(i), z(i), w)
    end do
    !$omp end parallel do simd

    !$omp simd aligned (w:16)
    do i = 2049, 4096
      x(i) = f4 (y(i), z(i), w)
    end do
    !$omp end simd

    !$omp simd
    do i = 4097, 8192
      if (x(i) .gt. 10) x(i) = f5 (x(i), i)
    end do
    !$omp end simd
  end subroutine
end module
