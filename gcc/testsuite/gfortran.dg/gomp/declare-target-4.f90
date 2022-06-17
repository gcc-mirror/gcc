! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine f1
  !$omp declare target device_type (any)  ! { dg-warning "OMP DECLARE TARGET directive at .1. with only DEVICE_TYPE clause is ignored" }
end subroutine

subroutine f2
  !$omp declare target to (f2) device_type (any)
end subroutine

subroutine f3
  !$omp declare target device_type (any) to (f3)
end subroutine

subroutine f4
  !$omp declare target device_type (host) to (f4)
end subroutine

subroutine f5
  !$omp declare target device_type (nohost) to (f5)
end subroutine

subroutine f6
  !$omp declare target enter (f6) device_type (any)
end subroutine

module mymod
  ! device_type is ignored for variables in OpenMP 5.0
  ! but TR8 and later apply those rules to variables as well
  implicit none
  integer :: a, b(4), c, d
  integer :: e, f, g
  integer :: m, n, o, p, q, r, s, t, u, v, w, x
  common /block1/ m, n
  common /block2/ o, p
  common /block3/ q, r
  common /block4/ s, t
  common /block5/ u, v
  common /block6/ w, x

  !$omp declare target to(a) device_type(nohost)
  !$omp declare target to(b) device_type(host)
  !$omp declare target to(c) device_type(any)
 ! Fails in ME with "Error: wrong number of arguments specified for 'omp declare target link' attribute"
 ! !$omp declare target link(e) device_type(nohost)
 ! !$omp declare target link(f) device_type(host)
 ! !$omp declare target link(g) device_type(any)

  !$omp declare target to(/block1/) device_type(nohost)
  !$omp declare target to(/block2/) device_type(host)
  !$omp declare target to(/block3/) device_type(any)
  !$omp declare target link(/block4/) device_type(nohost)
  !$omp declare target link(/block5/) device_type(host)
  !$omp declare target link(/block6/) device_type(any)
contains
  subroutine s1
    !$omp declare target to (s1) device_type (any)
  end
  subroutine s2
    !$omp declare target to (s2) device_type (nohost)
  end
  subroutine s3
    !$omp declare target to (s3) device_type (host)
  end
end module

module m2
  use mymod
  implicit none
  public
  private :: s1, s2, s3, a, b, c, d, e, f, g
  public :: m, n, o, p, q, r, s, t, u, v, w, x
end module m2

! { dg-final { scan-tree-dump-times "omp declare target" 8 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(" 8 "original" } }
! { dg-final { scan-tree-dump-not "__attribute__\\(\\(omp declare target \[^\n\r\]*\[\n\r\]void f1" "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(any\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r]void f2" 1 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(any\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r\]void f3" 1 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(host\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r\]void f4" 1 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(nohost\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r\]void f5" 1 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(any\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r]void f6" 1 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(any\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r\]void s1" 1 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(nohost\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r\]void s2" 1 "original" } }
! { dg-final { scan-tree-dump-times "__attribute__\\(\\(omp declare target \\(device_type\\(host\\)\\)\\)\\)\[\n\r]__attribute__\[^\n\r]+\[\n\r\]void s3" 1 "original" } }
