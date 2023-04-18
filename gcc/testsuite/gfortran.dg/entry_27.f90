! { dg-do run }
! { dg-additional-options "-ff2c" }
!
! PR fortran/104312 - ICE in fold_convert_loc with entry, -ff2c: test
! Contributed by G.Steinmetz

module m
  implicit none
contains
  function f()
    real, pointer :: f, e
    real, target  :: a(2) = [1,2]
    f => a(1)
    return
    entry e()
    e => a(2)
  end
  function g()
    complex, pointer :: g,h
    complex, target  :: a(2) = [3,4]
    g => a(1)
    return
    entry h()
    h => a(2)
  end
  function f3()
    real, allocatable :: f3, e3
    allocate (f3, source=1.0)
    return
    entry e3()
    allocate (e3, source=2.0)
  end
  function g3()
    complex, allocatable :: g3, h3
    allocate (g3, source=(3.0,0.0))
    return
    entry h3()
    allocate (h3, source=(4.0,0.0))
  end
end

program p
  use m
  real,    pointer :: x
  complex, pointer :: c
  real    :: y
  complex :: d
  x => f()
  if (x /= 1.0) stop 1
  x => e()
  if (x /= 2.0) stop 2
  c => g()
  if (c /= (3.0,0.0)) stop 3
  c => h()
  if (c /= (4.0,0.0)) stop 4
  y = f3()
  if (y /= 1.0) stop 5
  y = e3()
  if (y /= 2.0) stop 6
  d = g3()
  if (d /= (3.0,0.0)) stop 7
  d = h3()
  if (d /= (4.0,0.0)) stop 8
end
