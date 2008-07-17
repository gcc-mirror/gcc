! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Fortran < 2008 allows 7  dimensions
! Fortran   2008 allows 15 dimensions (including co-array ranks)
!
integer :: a(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) ! { dg-error "more than 7 dimensions" }

! PR fortran/36825:
integer,parameter                        :: N=10
complex,dimension(-N:N,-N:N,0:1,0:1,-N:N,-N:N,0:1,0:1)   :: P ! { dg-error "more than 7 dimensions" }
end
