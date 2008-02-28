! { dg-do compile }
! { dg-options "-Wreturn-type" }
! Tests the fix of PR34545, in which the 'numclusters' that determines the size
! of fnres was not properly associated.
!
! Reported by Jon D. Richards <jon_d_r@msn.com>
!
module m1
  integer :: numclusters = 2
end module m1

module m2
  contains
    function get_nfirst( ) result(fnres)  ! { dg-warning "not set" }
      use m1, only: numclusters
      real :: fnres(numclusters)   ! change to REAL and it works!!  
    end function get_nfirst
end module m2

program kmeans_driver
   use m1
   use m2
   integer :: nfirst(3)
   nfirst(1:numclusters) = get_nfirst( )
end program kmeans_driver
! { dg-final { cleanup-modules "m1 m2" } }
