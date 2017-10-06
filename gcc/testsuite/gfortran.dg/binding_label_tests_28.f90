! { dg-do compile }
!
! PR fortran/61450
! Contributed by Francois-Xavier Coudert  <fxcoudert@gmail.com>
!
module p
  integer i1 ! { dg-error "Global binding name 'foo' at .1. is already being used at .2." }
  bind(c,name="foo") :: i1
end module

subroutine truc() bind(c,name="foo") ! { dg-error "Global binding name 'foo' at .1. is already being used at .2." }
end
