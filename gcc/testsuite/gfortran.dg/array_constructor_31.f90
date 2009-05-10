! { dg-do compile }
! Test the fix for pr40018 in which the elements in the array
! constructor would be of default type and this would cause an
! ICE in the backend because of the type mistmatch with 'i'.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
  integer(kind=8) :: i
  write(*,*) [(i, i = 1, 10)]
  end
