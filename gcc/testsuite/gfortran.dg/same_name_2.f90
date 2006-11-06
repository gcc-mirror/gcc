! ( dg-do compile }
! Tests the fix for PR27701, in which two same name procedures
! were not diagnosed if they had no arguments.
!
! Contributed by Arjen Markus  <arjen.markus@wldelft.nl>
!
module aha
contains
subroutine aa ! { dg-error "Procedure" }
   write(*,*) 'AA'
end subroutine aa
subroutine aa ! { dg-error "is already defined" }
   write(*,*) 'BB'
end subroutine aa
end module
! { dg-final { cleanup-modules "aha" } }
