! { dg-do compile }
! Tests the fix for PR30319, in which the use of the parameter 'aa' in
! the array constructor that initialises bb would cause an internal
! error in resolution.
!
! Contributed by Vivek Rao <vivekrao4@yahoo.com>
!
module foomod
  character (len=1), parameter :: aa = "z", bb(1) = (/aa/)
end module foomod
  use foomod
  print *, aa, bb
end
! { dg-final { cleanup-modules "foomod" } }
