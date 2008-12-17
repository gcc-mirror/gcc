! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Unequal character lengths" }

! PR fortran/38137
! Test that -fbounds-check detects unequal character lengths to MERGE
! at runtime.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

subroutine foo(a)
implicit none
character(len=*) :: a
character(len=3) :: b
print *, merge(a,b,.true.)  ! Unequal character lengths
end subroutine foo

call foo("ab")
end
