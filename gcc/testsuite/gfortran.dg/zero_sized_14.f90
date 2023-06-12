! { dg-do run }
! PR fortran/86277
!
! Check proper detection of presence of optional array dummy arguments
! for zero-sized actual array arguments or array constructors:
! tests for REAL (as non-character intrinsic type) and empty derived type

program test
  implicit none
  real, parameter   :: m(0) = 42.
  real, parameter   :: n(1) = 23.
  real              :: x(0) = 1.
  real              :: z(1) = 2.
  real              :: w(0)
  real, pointer     :: p(:)
  real, allocatable :: y(:)
  integer           :: k = 0, l = 0     ! Test/failure counter
  type dt
     ! Empty type
  end type dt
  type(dt), parameter   :: t0(0) = dt()
  type(dt), parameter   :: t1(1) = dt()
  type(dt)              :: t2(0) = dt()
  type(dt)              :: t3(1) = dt()
  type(dt)              :: t4(0)
  type(dt), allocatable :: tt(:)
  !
  allocate (p(0))
  allocate (y(0))
  allocate (tt(0))
  call a0 ()
  call a1 ()
  call a2 ()
  call a3 ()
  call all_missing ()
  print *, "Total tests:", k, " failed:", l
contains
  subroutine a0 ()
    print *, "Variables as actual argument"
    call i  (m)
    call i  (n)
    call i  (x)
    call i  (w)
    call i  (y)
    call i  (p)
    call j  (t0)
    call j  (t1)
    call j  (t2)
    call j  (t3)
    call j  (t4)
    call j  (tt)
    print *, "Array section as actual argument"
    call i  (m(1:0))
    call i  (n(1:0))
    call i  (x(1:0))
    call i  (w(1:0))
    call i  (z(1:0))
    call i  (p(1:0))
    call j  (t0(1:0))
    call j  (t1(1:0))
    call j  (t2(1:0))
    call j  (t3(1:0))
    call j  (t4(1:0))
    call j  (tt(1:0))
  end subroutine a0
  !
  subroutine a1 ()
    print *, "Explicit temporary as actual argument"
    call i ((m))
    call i ((n))
    call i ((n(1:0)))
    call i ((x))
    call i ((w))
    call i ((z(1:0)))
    call i ((y))
    call i ((p))
    call i ((p(1:0)))
    call j ((t0))
    call j ((t1))
    call j ((tt))
    call j ((t1(1:0)))
    call j ((tt(1:0)))
  end subroutine a1
  !
  subroutine a2 ()
    print *, "Array constructor as actual argument"
    call i ([m])
    call i ([n])
    call i ([x])
    call i ([w])
    call i ([z])
    call i ([m(1:0)])
    call i ([n(1:0)])
    call i ([m,n(1:0)])
    call i ([x(1:0)])
    call i ([w(1:0)])
    call i ([z(1:0)])
    call i ([y])
    call i ([p])
    call i ([y,y])
    call i ([p,p])
    call i ([y(1:0)])
    call i ([p(1:0)])
    call j ([t0])
    call j ([t0,t0])
    call j ([t1])
    call j ([tt])
    call j ([tt,tt])
    call j ([t1(1:0)])
    call j ([tt(1:0)])
  end subroutine a2
  !
  subroutine a3 ()
    print *, "Array constructor with type-spec as actual argument"
    call i ([real::  ])
    call i ([real:: 7])
    call i ([real:: m])
    call i ([real:: n])
    call i ([real:: x])
    call i ([real:: w])
    call i ([real:: m(1:0)])
    call i ([real:: n(1:0)])
    call i ([real:: m,n(1:0)])
    call i ([real:: x(1:0)])
    call i ([real:: w(1:0)])
    call i ([real:: z(1:0)])
    call i ([real:: y])
    call i ([real:: p])
    call i ([real:: y,y])
    call i ([real:: p,p])
    call i ([real:: y(1:0)])
    call i ([real:: p(1:0)])
    call j ([ dt ::   ])
    call j ([ dt :: t0])
    call j ([ dt :: t0,t0])
    call j ([ dt :: t1])
    call j ([ dt :: tt])
    call j ([ dt :: tt,tt])
    call j ([ dt :: t1(1:0)])
    call j ([ dt :: tt(1:0)])
  end subroutine a3
  !
  subroutine i (arg)
    real, optional, intent(in) :: arg(:)
    logical :: t
    t = present (arg)
    k = k + 1
    print *, 'test', k, merge ("  ok", "FAIL", t)
    if (.not. t) l = l + 1
    if (.not. t) stop k
  end subroutine i
  !
  subroutine j (arg)
    type(dt), optional, intent(in) :: arg(:)
    logical :: t
    t = present (arg)
    k = k + 1
    print *, 'test', k, merge ("  ok", "FAIL", t)
    if (.not. t) l = l + 1
    if (.not. t) stop k
  end subroutine j
  !
  subroutine all_missing (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
    real,         optional, intent(in)  :: arg1(:)
    real,         optional, allocatable :: arg2(:)
    real,         optional, pointer     :: arg3(:)
    character(*), optional, intent(in)  :: arg4(:)
    character(*), optional, allocatable :: arg5(:)
    character(*), optional, pointer     :: arg6(:)
    character(:), optional, pointer     :: arg7(:)
    character(:), optional, allocatable :: arg8(:)
    if (present (arg1)) stop 101
    if (present (arg2)) stop 102
    if (present (arg3)) stop 103
    if (present (arg4)) stop 104
    if (present (arg5)) stop 105
    if (present (arg6)) stop 106
    if (present (arg7)) stop 107
    if (present (arg8)) stop 108
  end subroutine all_missing
end program
