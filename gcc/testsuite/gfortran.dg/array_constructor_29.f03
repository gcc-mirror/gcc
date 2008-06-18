! { dg-do compile }

! PR fortran/36492
! Similar to the ICE-test, but now test it works for real constants.

implicit none

integer, parameter :: a = 42
type t
  character (a) :: arr (1) = [ "a" ]
end type t

end
