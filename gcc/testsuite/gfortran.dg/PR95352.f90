! { dg-do compile }
!
! Test the fix for PR95352
! 
  
module ice6_m

  implicit none

contains

  function ice6_s(a) result(ierr)
    integer, intent(in) :: a(..)

    integer :: ierr

    integer :: lb

    select rank(a)
    rank(*)
      lb = lbound(a, dim=1)
      if(lbound(a, dim=1)/=lb) ierr = -1
    end select
    return
  end function ice6_s
  
end module ice6_m
