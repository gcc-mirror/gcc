! { dg-do run }


! PR fortran/99585

module m2
  type t
     class(*), pointer :: bar(:)
  end type
  type t2
     class(t), allocatable :: my(:)
  end type t2
contains
  function f (x, y) result(z)
    class(t) :: x(:)
    class(t) :: y(size(x(1)%bar))
    type(t)  :: z(size(x(1)%bar))
  end
  function g (x) result(z)
    class(t) :: x(:)
    type(t)  :: z(size(x(1)%bar))
  end
  subroutine s ()
    class(t2), allocatable :: a(:), b(:), c(:), d(:)
    class(t2), pointer     :: p(:)
    c(1)%my = f (a(1)%my, b(1)%my)
    d(1)%my = g (p(1)%my)
  end
end

! Contributed by  G. Steinmetz:
! PR fortran/104430

module m
   type t
      integer :: a
   end type
contains
   function f(x) result(z)
      class(t) :: x(:)
      type(t) :: z(size(x%a))
      z%a = 42
   end
end
program p
   use m
   class(t), allocatable :: y(:), z(:)
   allocate (y(32))
   z = f(y)
   if (size(z) /= 32) stop 1
   if (any (z%a /= 42)) stop 2
end
