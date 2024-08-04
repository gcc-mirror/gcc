! { dg-do run }
! { dg-options "-funsigned" }
! Test conversions from unsigned to different data types by
! doing some I/O.
program main
  implicit none
  integer :: vi,i
  integer, parameter :: n_int = 16, n_real = 8
  unsigned(kind=1) :: u1
  unsigned(kind=2) :: u2
  unsigned(kind=4) :: u4
  unsigned(kind=8) :: u8
  unsigned :: u
  integer, dimension(n_int) :: ires
  real(kind=8), dimension(n_real) :: rres
  real(kind=8) :: vr
  complex (kind=8) :: vc
  data ires /11,12,14,18,21,22,24,28,41,42,44,48,81,82,84,88/
  data rres /14., 18., 24., 28., 44., 48., 84., 88./
  open (10,status="scratch")

  write (10,*) int(11u_1,1)
  write (10,*) int(12u_1,2)
  write (10,*) int(14u_1,4)
  write (10,*) int(18u_1,8)

  write (10,*) int(21u_2,1)
  write (10,*) int(22u_2,2)
  write (10,*) int(24u_2,4)
  write (10,*) int(28u_2,8)

  write (10,*) int(41u_4,1)
  write (10,*) int(42u_4,2)
  write (10,*) int(44u_4,4)
  write (10,*) int(48u_4,8)

  write (10,*) int(81u_8,1)
  write (10,*) int(82u_8,2)
  write (10,*) int(84u_8,4)
  write (10,*) int(88u_8,8)

  rewind 10
  do i=1,n_int
     read (10,*) vi
     if (vi /= ires(i)) error stop 1
  end do

  rewind 10
  u1 = 11u; write (10,*) int(u1,1)
  u1 = 12u; write (10,*) int(u1,2)
  u1 = 14u; write (10,*) int(u1,4)
  u1 = 18u; write (10,*) int(u1,8)

  u2 = 21u; write (10,*) int(u2,1)
  u2 = 22u; write (10,*) int(u2,2)
  u2 = 24u; write (10,*) int(u2,4)
  u2 = 28u; write (10,*) int(u2,8)

  u4 = 41u; write (10,*) int(u4,1)
  u4 = 42u; write (10,*) int(u4,2)
  u4 = 44u; write (10,*) int(u4,4)
  u4 = 48u; write (10,*) int(u4,8)

  u8 = 81u; write (10,*) int(u8,1)
  u8 = 82u; write (10,*) int(u8,2)
  u8 = 84u; write (10,*) int(u8,4)
  u8 = 88u; write (10,*) int(u8,8)

  rewind 10
  do i=1,n_int
     read (10,*) vi
     if (vi /= ires(i)) error stop 2
  end do

  rewind 10
  write (10,*) real(14u_1,4)
  write (10,*) real(18u_1,8)
  write (10,*) real(24u_2,4)
  write (10,*) real(28u_2,8)
  write (10,*) real(44u_4,4)
  write (10,*) real(48u_4,8)
  write (10,*) real(84u_8,4)
  write (10,*) real(88u_8,8)

  rewind 10
  do i=1, n_real
     read (10, *) vr
     if (vr /= rres(i)) error stop 3
  end do

  rewind 10
  u1 = 14u_1; write (10,*) real(u1,4)
  u1 = 18u_1; write (10,*) real(u1,8)
  u2 = 24u_2; write (10,*) real(u2,4)
  u2 = 28u_2; write (10,*) real(u2,8)
  u4 = 44u_4; write (10,*) real(u4,4)
  u4 = 48u_4; write (10,*) real(u4,8)
  u8 = 84u_4; write (10,*) real(u8,4)
  u8 = 88u_4; write (10,*) real(u8,8)

  rewind 10
  do i=1, n_real
     read (10, *) vr
     if (vr /= rres(i)) error stop 4
  end do

  rewind 10
  u1 = 14u_1; write (10,*) cmplx(14u_1,u1,kind=4)
  u1 = 18u_1; write (10,*) cmplx(18u_1,u1,kind=8)
  u2 = 24u_2; write (10,*) cmplx(24u_2,u2,kind=4)
  u2 = 28u_2; write (10,*) cmplx(28u_2,u2,kind=8)
  u4 = 44u_4; write (10,*) cmplx(44u_4,u4,kind=4)
  u4 = 48u_8; write (10,*) cmplx(48u_4,u4,kind=8)
  u8 = 84u_8; write (10,*) cmplx(84u_8,u8,kind=4)
  u8 = 88u_8; write (10,*) cmplx(88u_8,u8,kind=8)

  rewind 10
  do i=1,n_real
     read (10, *) vc
     if (real(vc) /= rres(i)) error stop 5
     if (aimag(vc) /= rres(i)) error stop 6
  end do
end program main
