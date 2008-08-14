! { dg-do compile }
!
! PR fortran/36705
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

save :: p
procedure() :: p
pointer :: p

contains

subroutine bar(x)
  procedure(), intent(in) :: x
  pointer :: x
end subroutine bar 

end
