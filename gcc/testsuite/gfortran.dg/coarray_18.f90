! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Prevent ICE when exceeding the maximal number of allowed
! dimensions (normal + codimensions).
!
! Fortran 2008 allows (co)arrays with 15 ranks
! Currently, gfortran only supports 7, cf. PR 37577
! Thus, the program is valid Fortran 2008 ...
!
! See also general coarray PR 18918
!
! Test case taken from Leibniz-Rechenzentrum (LRZ)'s
! fortran_tests with thanks to Reinhold Bader.
!

program ar
  implicit none
  integer :: ic(2)[*]
  integer :: id(2,2)[2,*]
  integer :: ie(2,2,2)[2,2,*]
  integer :: ig(2,2,2,2)[2,2,2,*] ! { dg-error "has more than 7 dimensions" }
  integer :: ih(2,2,2,2,2)[2,2,2,2,*] ! { dg-error "has more than 7 dimensions" }
  integer :: ij(2,2,2,2,2,2)[2,2,2,2,2,*] ! { dg-error "has more than 7 dimensions" }
  integer :: ik(2,2,2,2,2,2,2)[2,2,2,2,2,2,*] ! { dg-error "has more than 7 dimensions" }
  integer :: il[2,2,2,2,2,2,2,*] ! { dg-error "has more than 7 dimensions" }
  integer :: im[2,2,2,2,2,2,2,2,*] ! { dg-error "has more than 7 dimensions" }
  integer :: in[2,2,2,2,2,2,2,2,2,*] ! { dg-error "has more than 7 dimensions" }
  integer :: io[2,2,2,2,2,2,2,2,2,2,*] ! { dg-error "has more than 7 dimensions" }
   real :: x2(2,2,4)[2,*]
   complex :: c2(4,2)[2,*]
   double precision :: d2(1,5,9)[2,*]
   character(len=1) :: ch2(2)[2,*]
   character(len=2) :: ch22(-5:4)[2,*]
   logical :: l2(17)[2,*]
   if (this_image() == 1) then
      write(*,*) 'OK'
   end if
end program
