! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Prevent ICE when exceeding the maximal number of allowed
! dimensions (normal + codimensions).
!
! Fortran 2008 allows (co)arrays with 15 ranks
! Previously gfortran only supported 7, cf. PR 37577
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
! Previously, these would give errors.
  integer :: ig(2,2,2,2)[2,2,2,*]
  integer :: ih(2,2,2,2,2)[2,2,2,2,*]
  integer :: ij(2,2,2,2,2,2)[2,2,2,2,2,*]
  integer :: ik(2,2,2,2,2,2,2)[2,2,2,2,2,2,*]
  integer :: il[2,2,2,2,2,2,2,*] 
  integer :: im[2,2,2,2,2,2,2,2,*]
  integer :: in[2,2,2,2,2,2,2,2,2,*]
  integer :: io[2,2,2,2,2,2,2,2,2,2,*]
! Now with max dimensions 15.....
  integer :: ip(2,2,2,2,2,2,2,2)[2,2,2,2,2,2,2,*] ! { dg-error "has more than 15 dimensions" }
  integer :: iq[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,*] ! { dg-error "has more than 15 dimensions" }
! Check a non-coarray
  integer :: ir(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2) ! { dg-error "has more than 15 dimensions" }
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
