! { dg-do compile }
! This test is run with result-checking and -fbounds-check as
! nested_array_constructor_2.f90

! PR fortran/35846
! This used to ICE because the charlength of the trim-expression was
! NULL.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

implicit none
character(len=2) :: c(3)

c = 'a'
c = (/ (/ trim(c(1)), 'a' /)//'c', 'cd' /)

print *, c

end
