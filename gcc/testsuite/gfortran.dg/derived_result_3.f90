! { dg-do run }
! PR fortran/98454 - default-initialization of derived-type function results

program test
  implicit none
  type t
     integer :: unit = -1
  end type t
  type u
     integer, allocatable :: unit(:)
  end type u
  type(t) :: x, x3(3)
  type(u) :: y, y4(4)

  ! Scalar function result, DT with default initializer
  x = t(42)
  if (x% unit /= 42) stop 1
  x = g()
  if (x% unit /= -1) stop 2
  x = t(42)
  x = f()
  if (x% unit /= -1) stop 3
  x = t(42)
  x = h()
  if (x% unit /= -1) stop 4
  x = t(42)
  x = k()
  if (x% unit /= -1) stop 5

  ! Array function result, DT with default initializer
  x3 = t(13)
  if (any (x3% unit /= 13)) stop 11
  x3 = f3()
  if (any (x3% unit /= -1)) stop 12
  x3 = t(13)
  x3 = g3()
  if (any (x3% unit /= -1)) stop 13
  x3 = t(13)
  x3 = h3()
  if (any (x3% unit /= -1)) stop 14
  x3 = t(13)
  x3 = k3()
  if (any (x3% unit /= -1)) stop 15

  ! Scalar function result, DT with allocatable component
  y = u()
  if (allocated (y% unit)) stop 21
  allocate (y% unit(42))
  y = m()
  if (allocated (y% unit)) stop 22
  allocate (y% unit(42))
  y = n()
  if (allocated (y% unit)) stop 23
  allocate (y% unit(42))
  y = o()
  if (allocated (y% unit)) stop 24
  allocate (y% unit(42))
  y = p()
  if (allocated (y% unit)) stop 25

  ! Array function result, DT with allocatable component
  y4 = u()
  if (allocated (y4(1)% unit)) stop 31
  allocate (y4(1)% unit(42))
  y4 = m4()
  if (allocated (y4(1)% unit)) stop 32
  y4 = u()
  allocate (y4(1)% unit(42))
  y4 = n4()
  if (allocated (y4(1)% unit)) stop 33

  y4 = u()
  allocate (y4(1)% unit(42))
  y4 = o4()
  if (allocated (y4(1)% unit)) stop 34
  y4 = u()
  allocate (y4(1)% unit(42))
  y4 = p4()
  if (allocated (y4(1)% unit)) stop 35

contains

  ! Function result not referenced within function body
  function f()
    type(t) :: f
  end function f

  function k() result (f)
    type(t) :: f
  end function k

  ! Function result referenced within function body
  function g()
    type(t) :: g
    if (g% unit /= -1) stop 41
  end function g

  function h() result (g)
    type(t) :: g
    if (g% unit /= -1) stop 42
  end function h

  ! Function result not referenced within function body
  function f3 ()
    type(t) :: f3(3)
  end function f3

  function k3() result (f3)
    type(t) :: f3(3)
  end function k3

  ! Function result referenced within function body
  function g3()
    type(t) :: g3(3)
    if (any (g3% unit /= -1)) stop 43
  end function g3

  function h3() result (g3)
    type(t) :: g3(3)
    if (any (g3% unit /= -1)) stop 44
  end function h3

  function m()
    type(u) :: m
  end function m

  function n() result (f)
    type(u) :: f
  end function n

  function o()
    type(u) :: o
    if (allocated (o% unit)) stop 71
  end function o

  function p() result (f)
    type(u) :: f
    if (allocated (f% unit)) stop 72
  end function p

  function m4()
    type(u) :: m4(4)
  end function m4

  function n4() result (f)
    type(u) :: f(4)
  end function n4

  function o4()
    type(u) :: o4(4)
    if (allocated (o4(1)% unit)) stop 73
  end function o4

  function p4() result (f)
    type(u) :: f(4)
    if (allocated (f(1)% unit)) stop 74
  end function p4
end
