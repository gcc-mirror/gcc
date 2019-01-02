! { dg-options "-fcray-pointer -floop-nest-optimize" }

  use iso_c_binding, only : c_ptr, c_ptrdiff_t, c_loc
  interface
    subroutine foo (x, y, z, w)
      use iso_c_binding, only : c_ptr
      real, pointer :: x(:), y(:), w(:)
      type(c_ptr) :: z
    end subroutine
    subroutine bar (x, y, z, w)
      use iso_c_binding, only : c_ptr
      real, pointer :: x(:), y(:), w(:)
      type(c_ptr) :: z
    end subroutine
    subroutine baz (x, c)
      real, pointer :: x(:)
      real, allocatable :: c(:)
    end subroutine
  end interface
  type dt
    real, allocatable :: a(:)
  end type
  type (dt) :: b(64)
  real, target :: a(4096+63)
  real, pointer :: p(:), q(:), r(:), s(:)
  real, allocatable :: c(:)
  integer(c_ptrdiff_t) :: o
  integer :: i
  o = 64 - mod (loc (a), 64)
  if (o == 64) o = 0
  o = o / sizeof(0.0)
  p => a(o + 1:o + 1024)
  q => a(o + 1025:o + 2048)
  r => a(o + 2049:o + 3072)
  s => a(o + 3073:o + 4096)
  do i = 1, 1024
    p(i) = i
    q(i) = i
    r(i) = i
    s(i) = i
  end do
  call foo (p, q, c_loc (r(1)), s)
  do i = 1, 1024
    if (p(i) /= i * i + 3 * i + 2) STOP 1
    p(i) = i
  end do
  call bar (p, q, c_loc (r(1)), s)
  do i = 1, 1024
    if (p(i) /= i * i + 3 * i + 2) STOP 2
  end do
  ! Attempt to create 64-byte aligned allocatable
  do i = 1, 64
    allocate (c(1023 + i))
    if (iand(int(loc(c(1)), 8), 63_8) == 0) exit
    deallocate (c)
    allocate (b(i)%a(1023 + i))
    allocate (c(1023 + i))
    if (iand(int(loc(c(1)), 8), 63_8) == 0) exit
    deallocate (c)
  end do
  if (allocated (c)) then
    do i = 1, 1024
      c(i) = 2 * i
    end do
    call baz (p, c)
    do i = 1, 1024
      if (p(i) /= i * i + 5 * i + 2) STOP 3
    end do
  end if
end
subroutine foo (x, y, z, w)
  use iso_c_binding, only : c_ptr, c_f_pointer
  real, pointer :: x(:), y(:), w(:), p(:)
  type(c_ptr) :: z
  integer :: i
  real :: pt(1024)
  pointer (ip, pt)
  ip = loc (w)
!$omp simd aligned (x, y : 64)
  do i = 1, 1024
    x(i) = x(i) * y(i) + 2.0
  end do
!$omp simd aligned (x, z : 64) private (p)
  do i = 1, 1024
    call c_f_pointer (z, p, shape=[1024])
    x(i) = x(i) + p(i)
  end do
!$omp simd aligned (x, ip : 64)
  do i = 1, 1024
    x(i) = x(i) + 2 * pt(i)
  end do
!$omp end simd
end subroutine
subroutine bar (x, y, z, w)
  use iso_c_binding, only : c_ptr, c_f_pointer
  real, pointer :: x(:), y(:), w(:), a(:), b(:)
  type(c_ptr) :: z, c
  integer :: i
  real :: pt(1024)
  pointer (ip, pt)
  ip = loc (w)
  a => x
  b => y
  c = z
!$omp simd aligned (a, b : 64)
  do i = 1, 1024
    a(i) = a(i) * b(i) + 2.0
  end do
!$omp simd aligned (a, c : 64)
  do i = 1, 1024
    block
      real, pointer :: p(:)
      call c_f_pointer (c, p, shape=[1024])
      a(i) = a(i) + p(i)
    end block
  end do
!$omp simd aligned (a, ip : 64)
  do i = 1, 1024
    a(i) = a(i) + 2 * pt(i)
  end do
!$omp end simd
end subroutine
subroutine baz (x, c)
  real, pointer :: x(:)
  real, allocatable :: c(:)
  integer :: i
!$omp simd aligned (x, c : 64)
  do i = 1, 1024
    x(i) = x(i) + c(i)
  end do
!$omp end simd
end subroutine baz
