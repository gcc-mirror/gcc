!{ dg-do run }

! Check that copying of memory for allocated scalar is assigned
! to coarray object.

! Contributed by G. Steinmetz  <gscfq@t-online.de>

program p
  type t
    integer, allocatable :: a
  end type
  type t2
    type(t), allocatable :: b
  end type
  type(t2) :: x, y[*]

  x%b = t(1)
  y = x
  y%b%a = 2

  if (x%b%a /= 1) stop 1
  if (y%b%a /= 2) stop 2
end

