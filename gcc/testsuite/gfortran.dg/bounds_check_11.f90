! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Array bound checking" }
! PR fortran/33745
!
! Don't check upper bound of assumed-size array
!

program test
 implicit none
 integer, parameter :: maxss=7,maxc=8
 integer :: jp(2,maxc)
 call findphase(jp)
contains
  subroutine findphase(jp)
    integer, intent(out) :: jp(2,*)
    jp(2,2:4)=0
    jp(2,0:4)=0 ! { dg-warning "out of bounds" }
    jp(3,1:4)=0 ! { dg-warning "out of bounds" }
  end subroutine
end program test

! { dg-output "At line 18 of file .*" }
! { dg-output "Array reference out of bounds, lower bound of dimension 2 of array 'jp' exceeded .0 < 1." }

