! { dg-do compile }
! { dg-options "-fcoarray=single" }
! Test the fix for PR55172.
!
! Contributed by Arjen Markus  <arjen.markus@deltares.nl>
!
module gn
  type :: ncb
  end type ncb
  type, public :: tn
     class(ncb), allocatable, dimension(:) :: cb
  end type tn
contains
  integer function name(self)
    implicit none
    class (tn), intent(in) :: self
    select type (component => self%cb(i)) ! { dg-error "has no IMPLICIT type" }
    end select
  end function name
end module gn

! Further issues, raised by Tobias Burnus in the course of fixing the PR

module gn1
  type :: ncb1
  end type ncb1
  type, public :: tn1
     class(ncb1), allocatable, dimension(:) :: cb
  end type tn1
contains
  integer function name(self)
    implicit none
    class (tn1), intent(in) :: self
    select type (component => self%cb([4,7+1])) ! { dg-error "needs a temporary" }
    end select
  end function name
end module gn1

module gn2
  type :: ncb2
  end type ncb2
  type, public :: tn2
     class(ncb2), allocatable :: cb[:]
  end type tn2
contains
  integer function name(self)
    implicit none
    class (tn2), intent(in) :: self
    select type (component => self%cb[4]) ! { dg-error "must not be coindexed" }
    end select
  end function name
end module gn2
