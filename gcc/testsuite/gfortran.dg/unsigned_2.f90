! { dg-do run }
! { dg-options "-funsigned" }
! Test some list-directed I/O
program main
  implicit none
  unsigned :: uw, ur, vr
  unsigned(kind=8) :: u8
  uw = 10u
  open (10, status="scratch")
  write (10,*) uw,-1
  rewind 10
  read (10,*) ur,vr
  if (ur /= 10u .or. vr /= 4294967295u) error stop 1
  rewind 10
  write (10,*) 17179869184u_8
  rewind 10
  read (10,*) u8
  if (u8 /= 17179869184u_8) error stop 2
end program main
  
