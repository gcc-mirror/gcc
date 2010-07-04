! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Coarray support -- allocatable array coarrays
! PR fortran/18918
! PR fortran/43931
!
program test
  implicit none
  call one()
contains
  subroutine one()
    integer, allocatable :: a(:)[:,:,:]
    allocate(a(1)[-4:9,8,4:*])
  end subroutine one
  subroutine four(C)
    integer, allocatable :: C(:)[:]
 end subroutine four
end program test
