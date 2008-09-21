! { dg-do compile }

! PR fortran/35846
! This used to ICE because the charlength of the trim-expression was
! NULL, but it is switched around to test for the right operand of // being
! not a constant, too.

implicit none
character(len=2) :: c(2)

c = 'a'
c = (/ (/ trim(c(1)), 'a' /) // (/ trim(c(1)), 'a' /) /)

print *, c

end
