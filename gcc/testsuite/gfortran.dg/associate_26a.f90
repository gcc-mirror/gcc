! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! Test the fix for PR78152 and the followup in PR82868
!
! Contributed by <physiker@toast2.net>
!
program co_assoc
  implicit none
  integer, parameter :: p = 5
  real, allocatable :: a(:,:)[:,:]
  allocate (a(p,p)[2,*])
  associate (i => a(1:p, 1:p))
  end associate
end program co_assoc
