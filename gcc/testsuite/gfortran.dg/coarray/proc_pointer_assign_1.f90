!{ dg-do run }

! Check that PR101100 is fixed.

! Contributed by G. Steinmetz  <gscfq@t-online.de>

program p
  type t
    procedure(), pointer, nopass :: f
  end type

  integer :: i = 0
  type(t) :: x[*]

  x%f => null()
  if ( associated(x%f) ) stop 1

  x%f => g
  if (.not. associated(x%f) ) stop 2

  call x%f()
  if ( i /= 1 ) stop 3

contains
  subroutine g()
    i = 1
  end subroutine
end

