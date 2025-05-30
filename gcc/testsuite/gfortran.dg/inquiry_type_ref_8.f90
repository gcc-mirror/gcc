! { dg-do compile }
! { dg-additional-options "-O0 -fdump-tree-original -std=f2018" }
!
! PR fortran/102599 - type parameter inquiries and constant complex arrays
! PR fortran/114022 - likewise
!
! Everything below shall be simplified at compile time.

module mod
  implicit none
  public :: wp, c0, z0, y, test1
  private

  integer            :: j
  integer, parameter :: n  = 5
  integer, parameter :: wp = 8
  type :: cx
     real(wp) :: re
     real(wp) :: im
  end type cx
  type(cx),    parameter :: c0(*) = [(cx   (j,-j),   j=1,n)]
  complex(wp), parameter :: z0(*) = [(cmplx(j,-j,wp),j=1,n)]

  type :: my_type
     complex(wp) :: z(n) = z0
     type(cx)    :: c(n) = c0
  end type my_type
  type(my_type), parameter :: y = my_type()

contains

  ! Check simplification for inquiries of host-associated variables
  subroutine test1 ()
    ! Inquiries and full arrays
    real(wp), parameter :: r0(*) = real  (z0)
    real(wp), parameter :: i0(*) = aimag (z0)
    real(wp), parameter :: r1(*) = c0 % re
    real(wp), parameter :: i1(*) = c0 % im
    real(wp), parameter :: r2(*) = z0 % re
    real(wp), parameter :: i2(*) = z0 % im
    real(wp), parameter :: r3(*) = y % c % re
    real(wp), parameter :: i3(*) = y % c % im
    real(wp), parameter :: r4(*) = y % z % re
    real(wp), parameter :: i4(*) = y % z % im

    logical, parameter :: l1 = all (r1 == r0)
    logical, parameter :: l2 = all (i1 == i0)
    logical, parameter :: l3 = all (r1 == r2)
    logical, parameter :: l4 = all (i1 == i2)
    logical, parameter :: l5 = all (r3 == r4)
    logical, parameter :: l6 = all (i3 == i4)
    logical, parameter :: l7 = all (r1 == r3)
    logical, parameter :: l8 = all (i1 == i3)

    ! Inquiries and array sections
    real(wp), parameter :: p0(*) = real (z0(::2))
    real(wp), parameter :: q0(*) = aimag (z0(::2))
    real(wp), parameter :: p1(*) = c0(::2) % re
    real(wp), parameter :: q1(*) = c0(::2) % im
    real(wp), parameter :: p2(*) = z0(::2) % re
    real(wp), parameter :: q2(*) = z0(::2) % im
    real(wp), parameter :: p3(*) = y % c(::2) % re
    real(wp), parameter :: q3(*) = y % c(::2) % im
    real(wp), parameter :: p4(*) = y % z(::2) % re
    real(wp), parameter :: q4(*) = y % z(::2) % im

    logical, parameter :: m1 = all (p1 == p0)
    logical, parameter :: m2 = all (q1 == q0)
    logical, parameter :: m3 = all (p1 == p2)
    logical, parameter :: m4 = all (q1 == q2)
    logical, parameter :: m5 = all (p3 == p4)
    logical, parameter :: m6 = all (q3 == q4)
    logical, parameter :: m7 = all (p1 == p3)
    logical, parameter :: m8 = all (q1 == q3)

    ! Inquiries and vector subscripts
    real(wp), parameter :: v0(*) = real (z0([3,2]))
    real(wp), parameter :: w0(*) = aimag (z0([3,2]))
    real(wp), parameter :: v1(*) = c0([3,2]) % re
    real(wp), parameter :: w1(*) = c0([3,2]) % im
    real(wp), parameter :: v2(*) = z0([3,2]) % re
    real(wp), parameter :: w2(*) = z0([3,2]) % im
    real(wp), parameter :: v3(*) = y % c([3,2]) % re
    real(wp), parameter :: w3(*) = y % c([3,2]) % im
    real(wp), parameter :: v4(*) = y % z([3,2]) % re
    real(wp), parameter :: w4(*) = y % z([3,2]) % im

    logical, parameter :: o1 = all (v1 == v0)
    logical, parameter :: o2 = all (w1 == w0)
    logical, parameter :: o3 = all (v1 == v2)
    logical, parameter :: o4 = all (w1 == w2)
    logical, parameter :: o5 = all (v3 == v4)
    logical, parameter :: o6 = all (w3 == w4)
    logical, parameter :: o7 = all (v1 == v3)
    logical, parameter :: o8 = all (w1 == w3)

    ! Miscellaneous
    complex(wp),     parameter :: x(-1:*) = cmplx (r1,i1,kind=wp)
    real(x%re%kind), parameter :: r(*) = x % re
    real(x%im%kind), parameter :: i(*) = x % im
    real(x%re%kind), parameter :: s(*) = [ x(:) % re ]
    real(x%im%kind), parameter :: t(*) = [ x(:) % im ]

    integer, parameter :: kr = x % re % kind
    integer, parameter :: ki = x % im % kind
    integer, parameter :: kx = x %      kind

    if (kr /= wp .or. ki /= wp .or. kx /= wp) stop 1
    if (any (r /= r1)) stop 2
    if (any (i /= i1)) stop 3
    if (any (s /= r1)) stop 4
    if (any (t /= i1)) stop 5

    if (.not. all ([l1,l2,l3,l4,l5,l6,l7,l8])) stop 6
    if (.not. all ([m1,m2,m3,m4,m5,m6,m7,m8])) stop 7
    if (.not. all ([o1,o2,o3,o4,o5,o6,o7,o8])) stop 8
  end subroutine test1
end

program p
  use mod, only: wp, c0, z0, y, test1
  implicit none
  call test1 ()
  call test2 ()
contains
  ! Check simplification for inquiries of use-associated variables
  subroutine test2 ()
    ! Inquiries and full arrays
    real(wp), parameter :: r0(*) = real (z0)
    real(wp), parameter :: i0(*) = aimag (z0)
    real(wp), parameter :: r1(*) = c0 % re
    real(wp), parameter :: i1(*) = c0 % im
    real(wp), parameter :: r2(*) = z0 % re
    real(wp), parameter :: i2(*) = z0 % im
    real(wp), parameter :: r3(*) = y % c % re
    real(wp), parameter :: i3(*) = y % c % im
    real(wp), parameter :: r4(*) = y % z % re
    real(wp), parameter :: i4(*) = y % z % im

    logical, parameter :: l1 = all (r1 == r0)
    logical, parameter :: l2 = all (i1 == i0)
    logical, parameter :: l3 = all (r1 == r2)
    logical, parameter :: l4 = all (i1 == i2)
    logical, parameter :: l5 = all (r3 == r4)
    logical, parameter :: l6 = all (i3 == i4)
    logical, parameter :: l7 = all (r1 == r3)
    logical, parameter :: l8 = all (i1 == i3)

    ! Inquiries and array sections
    real(wp), parameter :: p0(*) = real (z0(::2))
    real(wp), parameter :: q0(*) = aimag (z0(::2))
    real(wp), parameter :: p1(*) = c0(::2) % re
    real(wp), parameter :: q1(*) = c0(::2) % im
    real(wp), parameter :: p2(*) = z0(::2) % re
    real(wp), parameter :: q2(*) = z0(::2) % im
    real(wp), parameter :: p3(*) = y % c(::2) % re
    real(wp), parameter :: q3(*) = y % c(::2) % im
    real(wp), parameter :: p4(*) = y % z(::2) % re
    real(wp), parameter :: q4(*) = y % z(::2) % im

    logical, parameter :: m1 = all (p1 == p0)
    logical, parameter :: m2 = all (q1 == q0)
    logical, parameter :: m3 = all (p1 == p2)
    logical, parameter :: m4 = all (q1 == q2)
    logical, parameter :: m5 = all (p3 == p4)
    logical, parameter :: m6 = all (q3 == q4)
    logical, parameter :: m7 = all (p1 == p3)
    logical, parameter :: m8 = all (q1 == q3)

    ! Inquiries and vector subscripts
    real(wp), parameter :: v0(*) = real (z0([3,2]))
    real(wp), parameter :: w0(*) = aimag (z0([3,2]))
    real(wp), parameter :: v1(*) = c0([3,2]) % re
    real(wp), parameter :: w1(*) = c0([3,2]) % im
    real(wp), parameter :: v2(*) = z0([3,2]) % re
    real(wp), parameter :: w2(*) = z0([3,2]) % im
    real(wp), parameter :: v3(*) = y % c([3,2]) % re
    real(wp), parameter :: w3(*) = y % c([3,2]) % im
    real(wp), parameter :: v4(*) = y % z([3,2]) % re
    real(wp), parameter :: w4(*) = y % z([3,2]) % im

    logical, parameter :: o1 = all (v1 == v0)
    logical, parameter :: o2 = all (w1 == w0)
    logical, parameter :: o3 = all (v1 == v2)
    logical, parameter :: o4 = all (w1 == w2)
    logical, parameter :: o5 = all (v3 == v4)
    logical, parameter :: o6 = all (w3 == w4)
    logical, parameter :: o7 = all (v1 == v3)
    logical, parameter :: o8 = all (w1 == w3)

    ! Miscellaneous
    complex(wp),     parameter :: x(-1:*) = cmplx (r1,i1,kind=wp)
    real(x%re%kind), parameter :: r(*) = x % re
    real(x%im%kind), parameter :: i(*) = x % im
    real(x%re%kind), parameter :: s(*) = [ x(:) % re ]
    real(x%im%kind), parameter :: t(*) = [ x(:) % im ]

    integer, parameter :: kr = x % re % kind
    integer, parameter :: ki = x % im % kind
    integer, parameter :: kx = x %      kind

    if (kr /= wp .or. ki /= wp .or. kx /= wp) stop 11
    if (any (r /= r1)) stop 12
    if (any (i /= i1)) stop 13
    if (any (s /= r1)) stop 14
    if (any (t /= i1)) stop 15

    if (.not. all ([l1,l2,l3,l4,l5,l6,l7,l8])) stop 16
    if (.not. all ([m1,m2,m3,m4,m5,m6,m7,m8])) stop 17
    if (.not. all ([o1,o2,o3,o4,o5,o6,o7,o8])) stop 18
  end subroutine test2
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
