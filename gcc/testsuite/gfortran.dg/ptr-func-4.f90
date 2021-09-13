! { dg-do run }
! { dg-options "-O2 -std=f2008" }
! PR fortran/100218 - target of pointer from evaluation of function-reference

program p
  implicit none
  integer, target :: z = 0
  call g (f ())
  if (z /= 1) stop 1
contains
  function f () result (r)
    integer, pointer :: r
    r => z
  end function f
  subroutine g (x)
    integer, intent(out) :: x
    x = 1
  end subroutine g
end program p
