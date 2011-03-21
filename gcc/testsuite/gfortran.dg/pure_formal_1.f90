! { dg-do compile }
!
! PR fortran/47507
!
! PURE procedures: Allow arguments w/o INTENT if they are VALUE
!

pure function f(x)
  real, VALUE :: x
  real :: f
  f = sin(x)
end function f

pure subroutine sub(x)
  real, VALUE :: x
end subroutine sub
