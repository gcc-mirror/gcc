! { dg-do run }
! { dg-options "-fbounds-check" }

! PR fortran/35846
! This used to ICE because the charlength of the trim-expression was
! NULL.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

implicit none
character(len=2) :: c(3)

c = 'a'
c = (/ (/ trim(c(1)), 'a' /)//'c', 'cd' /)

print *, c

if (c(1) /= 'ac' .or. c(2) /= 'ac' .or. c(3) /= 'cd') then
  call abort ()
end if

end
