! { dg-do run }
! Test that the internal pack and unpack routines work OK
! for different data types

program main
  integer(kind=1), dimension(3) :: i1
  integer(kind=2), dimension(3) :: i2
  integer(kind=4), dimension(3) :: i4
  integer(kind=8), dimension(3) :: i8
  real(kind=4), dimension(3) :: r4
  real(kind=8), dimension(3) :: r8
  complex(kind=4), dimension(3) :: c4
  complex(kind=8), dimension(3) :: c8
  type i8_t
     sequence
     integer(kind=8) :: v
  end type i8_t
  type(i8_t), dimension(3) :: d_i8

  i1 = (/ -1, 1, -3 /)
  call sub_i1(i1(1:3:2))
  if (any(i1 /= (/ 3, 1, 2 /))) call abort

  i2 = (/ -1, 1, -3 /)
  call sub_i2(i2(1:3:2))
  if (any(i2 /= (/ 3, 1, 2 /))) call abort

  i4 = (/ -1, 1, -3 /)
  call sub_i4(i4(1:3:2))
  if (any(i4 /= (/ 3, 1, 2 /))) call abort

  i8 = (/ -1, 1, -3 /)
  call sub_i8(i8(1:3:2))
  if (any(i8 /= (/ 3, 1, 2 /))) call abort

  r4 = (/ -1.0, 1.0, -3.0 /)
  call sub_r4(r4(1:3:2))
  if (any(r4 /= (/ 3.0, 1.0, 2.0/))) call abort

  r8 = (/ -1.0_8, 1.0_8, -3.0_8 /)
  call sub_r8(r8(1:3:2))
  if (any(r8 /= (/ 3.0_8, 1.0_8, 2.0_8/))) call abort

  c4 = (/ (-1.0_4, 0._4), (1.0_4, 0._4), (-3.0_4, 0._4) /)
  call sub_c4(c4(1:3:2))
  if (any(real(c4) /= (/ 3.0_4, 1.0_4, 2.0_4/))) call abort
  if (any(aimag(c4) /= 0._4)) call abort

  c8 = (/ (-1.0_4, 0._4), (1.0_4, 0._4), (-3.0_4, 0._4) /)
  call sub_c8(c8(1:3:2))
  if (any(real(c8) /= (/ 3.0_4, 1.0_4, 2.0_4/))) call abort
  if (any(aimag(c8) /= 0._4)) call abort

  d_i8%v = (/ -1, 1, -3 /)
  call sub_d_i8(d_i8(1:3:2))
  if (any(d_i8%v /= (/ 3, 1, 2 /))) call abort

end program main

subroutine sub_i1(i)
  integer(kind=1), dimension(2) :: i
  if (i(1) /= -1) call abort
  if (i(2) /= -3) call abort
  i(1) = 3
  i(2) = 2
end subroutine sub_i1

subroutine sub_i2(i)
  integer(kind=2), dimension(2) :: i
  if (i(1) /= -1) call abort
  if (i(2) /= -3) call abort
  i(1) = 3
  i(2) = 2
end subroutine sub_i2

subroutine sub_i4(i)
  integer(kind=4), dimension(2) :: i
  if (i(1) /= -1) call abort
  if (i(2) /= -3) call abort
  i(1) = 3
  i(2) = 2
end subroutine sub_i4

subroutine sub_i8(i)
  integer(kind=8), dimension(2) :: i
  if (i(1) /= -1) call abort
  if (i(2) /= -3) call abort
  i(1) = 3
  i(2) = 2
end subroutine sub_i8

subroutine sub_r4(r)
  real(kind=4), dimension(2) :: r
  if (r(1) /= -1.) call abort
  if (r(2) /= -3.) call abort
  r(1) = 3.
  r(2) = 2.
end subroutine sub_r4

subroutine sub_r8(r)
  real(kind=8), dimension(2) :: r
  if (r(1) /= -1._8) call abort
  if (r(2) /= -3._8) call abort
  r(1) = 3._8
  r(2) = 2._8
end subroutine sub_r8

subroutine sub_c8(r)
  implicit none
  complex(kind=8), dimension(2) :: r
  if (r(1) /= (-1._8,0._8)) call abort
  if (r(2) /= (-3._8,0._8)) call abort
  r(1) = 3._8
  r(2) = 2._8
end subroutine sub_c8

subroutine sub_c4(r)
  implicit none
  complex(kind=4), dimension(2) :: r
  if (r(1) /= (-1._4,0._4)) call abort
  if (r(2) /= (-3._4,0._4)) call abort
  r(1) = 3._4
  r(2) = 2._4
end subroutine sub_c4

subroutine sub_d_i8(i)
  type i8_t
     sequence
     integer(kind=8) :: v
  end type i8_t
  type(i8_t), dimension(2) :: i
  if (i(1)%v /= -1) call abort
  if (i(2)%v /= -3) call abort
  i(1)%v = 3
  i(2)%v = 2
end subroutine sub_d_i8
