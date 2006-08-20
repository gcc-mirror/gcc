! { dg-do compile }
! Test the fix for PR28601 in which line 55 would produce an ICE
! because the rhs and lhs derived times were not identically
! associated and so could not be cast.
!
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
!
module modA
implicit none
save
private

type, public :: typA
integer :: i
end type typA

type, public :: atom
type(typA), pointer :: ofTypA(:,:)
end type atom
end module modA

!!! re-name and re-export typA as typB:
module modB
use modA, only: typB => typA
implicit none
save
private

public typB
end module modB

!!! mixed used of typA and typeB:
module modC
use modB
implicit none
save
private
contains

subroutine buggy(a)
use modA, only: atom
! use modB, only: typB
! use modA, only: typA
implicit none
type(atom),intent(inout) :: a
target :: a
! *** end of interface ***

type(typB), pointer :: ofTypB(:,:)
! type(typA), pointer :: ofTypB(:,:)
integer :: i,j,k

ofTypB => a%ofTypA

a%ofTypA(i,j) = ofTypB(k,j)
end subroutine buggy
end module modC
