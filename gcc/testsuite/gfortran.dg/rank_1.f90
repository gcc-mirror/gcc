! { dg-do compile }
! { dg-options "-std=f2008" }
!
! Fortran < 2008 allows 7  dimensions
! Fortran   2008 allows 15 dimensions (including co-array ranks)
!
! FIXME: Rank patch was reverted because of PR 36825.
integer :: a(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) ! { dg-error "has more than 7 dimensions" }
integer :: b(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16) ! { dg-error "has more than 7 dimensions" }
end
