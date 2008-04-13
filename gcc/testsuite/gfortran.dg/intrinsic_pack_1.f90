! { dg-do run }
! Take the pack intrinsic through its paces, with all types that are
! normally accessible.
program main
  implicit none
  integer :: i
  real(kind=4), dimension(3,3) :: r4
  real(kind=4), dimension(9) :: vr4
  real(kind=4), dimension(9) :: rr4
  real(kind=8), dimension(3,3) :: r8
  real(kind=8), dimension(9) :: vr8
  real(kind=8), dimension(9) :: rr8
  complex(kind=4), dimension(3,3) :: c4
  complex(kind=4), dimension(9) :: vc4
  complex(kind=4), dimension(9) :: rc4
  complex(kind=8), dimension(3,3) :: c8
  complex(kind=8), dimension(9) :: vc8
  complex(kind=8), dimension(9) :: rc8
  integer(kind=1), dimension(3,3) :: i1
  integer(kind=1), dimension(9) :: vi1
  integer(kind=1), dimension(9) :: ri1
  integer(kind=2), dimension(3,3) :: i2
  integer(kind=2), dimension(9) :: vi2
  integer(kind=2), dimension(9) :: ri2
  integer(kind=4), dimension(3,3) :: i4
  integer(kind=4), dimension(9) :: vi4
  integer(kind=4), dimension(9) :: ri4
  integer(kind=8), dimension(3,3) :: i8
  integer(kind=8), dimension(9) :: vi8
  integer(kind=8), dimension(9) :: ri8

  type i1_t
    integer(kind=1) :: v
  end type i1_t
  type(i1_t), dimension(3,3) :: d_i1
  type(i1_t), dimension(9) :: d_vi1
  type(i1_t), dimension(9) :: d_ri1

  type i4_t
    integer(kind=4) :: v
  end type i4_t
  type(i4_t), dimension(3,3) :: d_i4
  type(i4_t), dimension(9) :: d_vi4
  type(i4_t), dimension(9) :: d_ri4

  d_vi1%v = (/(i+10,i=1,9)/)
  d_i1%v = reshape((/1_1, -1_1, 2_1, -2_1, 3_1, -3_1, 4_1, &
                    & -4_1, 5_1/), shape(i1))
  d_ri1 = pack(d_i1,d_i1%v>0,d_vi1)
  if (any(d_ri1%v /= (/1_1, 2_1, 3_1, 4_1, 5_1, 16_1, 17_1, 18_1, 19_1/))) &
       & call abort

  d_vi4%v = (/(i+10,i=1,9)/)
  d_i4%v = reshape((/1_4, -1_4, 2_4, -2_4, 3_4, -3_4, 4_4, &
                    & -4_4, 5_4/), shape(d_i4))
  d_ri4 = pack(d_i4,d_i4%v>0,d_vi4)
  if (any(d_ri4%v /= (/1_4, 2_4, 3_4, 4_4, 5_4, 16_4, 17_4, 18_4, 19_4/))) &
       & call abort

  vr4 = (/(i+10,i=1,9)/)
  r4 = reshape((/1.0_4, -3.0_4, 2.1_4, -4.21_4, 1.2_4, 0.98_4, -1.2_4, &
  &              -7.1_4, -9.9_4, 0.3_4 /), shape(r4))
  rr4 = pack(r4,r4>0,vr4)
  if (any(rr4 /= (/ 1.0_4, 2.1_4, 1.2_4, 0.98_4,  15._4, 16._4, 17._4, &
  &                  18._4, 19._4 /))) call abort

  vr8 = (/(i+10,i=1,9)/)
  r8 = reshape((/1.0_8, -3.0_8, 2.1_8, -4.21_8, 1.2_8, 0.98_8, -1.2_8, &
  &              -7.1_8, -9.9_8, 0.3_8 /), shape(r8))
  rr8 = pack(r8,r8>0,vr8)
  if (any(rr8 /= (/ 1.0_8, 2.1_8, 1.2_8, 0.98_8,  15._8, 16._8, 17._8, &
  &                  18._8, 19._8 /))) call abort

  vc4 = (/(i+10,i=1,9)/)
  c4 = reshape((/1.0_4, -3.0_4, 2.1_4, -4.21_4, 1.2_4, 0.98_4, -1.2_4, &
  &              -7.1_4, -9.9_4, 0.3_4 /), shape(c4))
  rc4 = pack(c4,real(c4)>0,vc4)
  if (any(real(rc4) /= (/ 1.0_4, 2.1_4, 1.2_4, 0.98_4,  15._4, 16._4, 17._4, &
  &                  18._4, 19._4 /))) call abort
  if (any(aimag(rc4) /= 0)) call abort

  vc8 = (/(i+10,i=1,9)/)
  c8 = reshape((/1.0_4, -3.0_4, 2.1_4, -4.21_4, 1.2_4, 0.98_4, -1.2_4, &
  &              -7.1_4, -9.9_4, 0.3_4 /), shape(c8))
  rc8 = pack(c8,real(c8)>0,vc8)
  if (any(real(rc8) /= (/ 1.0_4, 2.1_4, 1.2_4, 0.98_4,  15._4, 16._4, 17._4, &
  &                  18._4, 19._4 /))) call abort
  if (any(aimag(rc8) /= 0)) call abort

  vi1 = (/(i+10,i=1,9)/)
  i1 = reshape((/1_1, -1_1, 2_1, -2_1, 3_1, -3_1, 4_1, -4_1, 5_1/), shape(i1))
  ri1 = pack(i1,i1>0,vi1)
  if (any(ri1 /= (/1_1, 2_1, 3_1, 4_1, 5_1, 16_1, 17_1, 18_1, 19_1/))) &
       & call abort

  vi2 = (/(i+10,i=1,9)/)
  i2 = reshape((/1_2, -1_2, 2_2, -2_2, 3_2, -3_2, 4_2, -4_2, 5_2/), shape(i2))
  ri2 = pack(i2,i2>0,vi2)
  if (any(ri2 /= (/1_2, 2_2, 3_2, 4_2, 5_2, 16_2, 17_2, 18_2, 19_2/))) &
       & call abort

  vi4 = (/(i+10,i=1,9)/)
  i4 = reshape((/1_4, -1_4, 2_4, -2_4, 3_4, -3_4, 4_4, -4_4, 5_4/), shape(i4))
  ri4 = pack(i4,i4>0,vi4)
  if (any(ri4 /= (/1_4, 2_4, 3_4, 4_4, 5_4, 16_4, 17_4, 18_4, 19_4/))) &
       & call abort

  vi8 = (/(i+10,i=1,9)/)
  i8 = reshape((/1_8, -1_8, 2_8, -2_8, 3_8, -3_8, 4_8, -4_8, 5_8/), shape(i8))
  ri8 = pack(i8,i8>0,vi8)
  if (any(ri8 /= (/1_8, 2_8, 3_8, 4_8, 5_8, 16_8, 17_8, 18_8, 19_8/))) &
       & call abort


end program main
