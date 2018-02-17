! { dg-do run }
! { dg-additional-options "-fno-inline" }

#define N 200

#define TEST_VALUE(I) ((I) * 5 / 2)

subroutine setup(a)
  real :: a(N)
  do i = 1, N
     a(i) = TEST_VALUE(i)
  end do
end subroutine

subroutine check(a, x, gap)
  real :: a(N), temp, x
  integer :: gap
  do i = 1, N - gap
     temp = a(i + gap) + x
     if (a(i) /= temp) STOP 1
  end do
  do i = N - gap + 1, N
     temp = TEST_VALUE(i)
     if (a(i) /= temp) STOP 2
  end do
end subroutine

subroutine testa(a, x, base, n)
  real :: a(n), x
  integer :: base, n
  do i = n, 2, -1
     a(base + i - 1) = a(base + i) + x
  end do
end subroutine testa

subroutine testb(a, x, base, n)
  real :: a(n), x
  integer :: base
  do i = n, 4, -1
     a(base + i - 3) = a(base + i) + x
  end do
end subroutine testb

subroutine testc(a, x, base, n)
  real :: a(n), x
  integer :: base
  do i = n, 8, -1
     a(base + i - 7) = a(base + i) + x
  end do
end subroutine testc

subroutine testd(a, x, base, n)
  real :: a(n), x
  integer :: base
  do i = n, 16, -1
     a(base + i - 15) = a(base + i) + x
  end do
end subroutine testd

subroutine teste(a, x, base, n)
  real :: a(n), x
  integer :: base
  do i = n, 32, -1
     a(base + i - 31) = a(base + i) + x
  end do
end subroutine teste

subroutine testf(a, x, base, n)
  real :: a(n), x
  integer :: base
  do i = n, 64, -1
     a(base + i - 63) = a(base + i) + x
  end do
end subroutine testf

program main
  real :: a(N)

  call setup(a)
  call testa(a, 91.0, 0, N)
  call check(a, 91.0, 1)

  call setup(a)
  call testb(a, 55.0, 0, N)
  call check(a, 55.0, 3)

  call setup(a)
  call testc(a, 72.0, 0, N)
  call check(a, 72.0, 7)

  call setup(a)
  call testd(a, 69.0, 0, N)
  call check(a, 69.0, 15)

  call setup(a)
  call teste(a, 44.0, 0, N)
  call check(a, 44.0, 31)

  call setup(a)
  call testf(a, 39.0, 0, N)
  call check(a, 39.0, 63)
end program
