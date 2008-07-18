! { dg-do compile }
! PR fortran/36824
!
! Dimension of tgclist was not recognized as having constant bounds
!
program test
implicit none
integer, dimension( 3 ), parameter :: tgc = (/5, 6, 7 /)
type tgccomp
   integer, dimension( tgc( 1 ) : tgc( 2 ) ) :: tgclist
end type tgccomp
end program
