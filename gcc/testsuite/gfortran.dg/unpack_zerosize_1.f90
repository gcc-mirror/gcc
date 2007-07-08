! { dg-do run }
! PR 32217 - unpack used to crash at runtime with a zero-sized
!            array.  Test case submitted by Jaroslav Hajek.
program bug_report
  implicit none
  integer,parameter:: rp = kind(1.d0),na = 6
  real(rp),allocatable:: hhe(:,:,:),hhc(:,:,:),dv(:)
  integer:: nhh,ndv
  nhh = 0
  allocate(hhe(nhh,2,2))
  ndv = 2*na + count(hhe /= 0)
  allocate(hhc(nhh,2,2),dv(ndv))
  hhc = unpack(dv(2*na+1:),hhe /= 0._rp,0._rp)
end program bug_report
