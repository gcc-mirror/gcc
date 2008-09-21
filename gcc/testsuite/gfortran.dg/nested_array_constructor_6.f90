! { dg-do compile }

! PR fortran/35846
! Nested three levels deep.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

implicit none
character(len=3) :: c(3)
c = 'a'
c = (/ (/ 'A'//(/ trim(c(1)), 'a' /)/)//'c', 'dcd' /)
print *, c(1)
print *, c(2)
print *, c(3)
end
