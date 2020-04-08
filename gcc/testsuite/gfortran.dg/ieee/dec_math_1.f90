! { dg-do run }
! { dg-additional-options "-cpp -std=gnu" }
!
! Test values for degree-valued trigonometric intrinsics.
!
! Run under ieee/ as
!   use ieee_arithmetic
! (used for result checking) is not available on all platforms)

module dec_math_5


  ! Use the highest precision available.
  ! Note however that if both __GFC_REAL_10__ and __GFC_REAL_16__ are defined,
  ! the size of real(16) is actually that of REAL(10) (80 bits) in which case
  ! we should not over-estimate the precision available, or the test will fail.
#if defined(__GFC_REAL_10__)
  integer, parameter :: real_kind = 10
  real(real_kind), parameter :: eps = 5e-11_10

  real(real_kind), parameter :: pi_2 = 1.57079632679489656_10
  real(real_kind), parameter :: pi = 3.14159265358979312_10
  real(real_kind), parameter :: tau = 6.28318530717958623_10

#elif defined(__GFC_REAL_16__)
  integer, parameter :: real_kind = 16
  real(real_kind), parameter :: eps = 5e-16_16

  real(real_kind), parameter :: pi_2 = 1.5707963267948966192313216916397514_16
  real(real_kind), parameter :: pi = 3.1415926535897932384626433832795_16
  real(real_kind), parameter :: tau = 6.28318530717958647692528676655900559_16

#else
  integer, parameter :: real_kind = 8
  real(real_kind), parameter :: eps = 5e-10_8

  real(real_kind), parameter :: pi_2 = 1.57079632679490_8
  real(real_kind), parameter :: pi = 3.14159265358979_8
  real(real_kind), parameter :: tau = 6.28318530717959_8

#endif

  ! Important angles in canonical form.

  integer, parameter :: nangle = 16

  real(real_kind), dimension(nangle), parameter :: degrees = (/ &
      0, & !  180 * 0
     30, & ! 180 * 1/6
     45, & ! 180 * 1/4
     60, & ! 180 * 1/3
     90, & ! 180 * 1/2
    120, & ! 180 * 2/3
    135, & ! 180 * 3/4
    150, & ! 180 * 5/6
    180, & ! 180
    210, & ! 180 * 7/6
    225, & ! 180 * 5/4
    240, & ! 180 * 4/3
    270, & ! 180 * 3/2
    300, & ! 180 * 5/3
    315, & ! 180 * 7/4
    330  & ! 180 * 11/6
  /)

  real(real_kind), dimension(nangle), parameter :: radians = (/ &
#ifdef __GFC_REAL_10__
    0.000000000000000000_10, & ! pi * 0
    0.523598775598298873_10, & ! pi * 1/6
    0.785398163397448310_10, & ! pi * 1/4
    1.047197551196597750_10, & ! pi * 1/3
    1.570796326794896620_10, & ! pi * 1/2
    2.094395102393195490_10, & ! pi * 2/3
    2.356194490192344930_10, & ! pi * 3/4
    2.617993877991494370_10, & ! pi * 5/6
    3.141592653589793240_10, & ! pi
    3.665191429188092110_10, & ! pi * 7/6
    3.926990816987241550_10, & ! pi * 5/4
    4.188790204786390980_10, & ! pi * 4/3
    4.712388980384689860_10, & ! pi * 3/2
    5.235987755982988730_10, & ! pi * 5/3
    5.497787143782138170_10, & ! pi * 7/4
    5.759586531581287600_10  & ! pi * 11/6

#elif defined(__GFC_REAL_16__)
    0.000000000000000000000000000000000_16, & ! pi * 0
    0.523598775598298873077107230546584_16, & ! pi * 1/6
    0.785398163397448309615660845819876_16, & ! pi * 1/4
    1.047197551196597746154214461093170_16, & ! pi * 1/3
    1.570796326794896619231321691639750_16, & ! pi * 1/2
    2.094395102393195492308428922186330_16, & ! pi * 2/3
    2.356194490192344928846982537459630_16, & ! pi * 3/4
    2.617993877991494365385536152732920_16, & ! pi * 5/6
    3.141592653589793238462643383279500_16, & ! pi
    3.665191429188092111539750613826090_16, & ! pi * 7/6
    3.926990816987241548078304229099380_16, & ! pi * 5/4
    4.188790204786390984616857844372670_16, & ! pi * 4/3
    4.712388980384689857693965074919250_16, & ! pi * 3/2
    5.235987755982988730771072305465840_16, & ! pi * 5/3
    5.497787143782138167309625920739130_16, & ! pi * 7/4
    5.759586531581287603848179536012420_16  & ! pi * 11/6

#else
    0.000000000000000_8, & ! pi * 0
    0.523598775598299_8, & ! pi * 1/6
    0.785398163397448_8, & ! pi * 1/4
    1.047197551196600_8, & ! pi * 1/3
    1.570796326794900_8, & ! pi * 1/2
    2.094395102393200_8, & ! pi * 2/3
    2.356194490192340_8, & ! pi * 3/4
    2.617993877991490_8, & ! pi * 5/6
    3.141592653589790_8, & ! pi
    3.665191429188090_8, & ! pi * 7/6
    3.926990816987240_8, & ! pi * 5/4
    4.188790204786390_8, & ! pi * 4/3
    4.712388980384690_8, & ! pi * 3/2
    5.235987755982990_8, & ! pi * 5/3
    5.497787143782140_8, & ! pi * 7/4
    5.759586531581290_8  & ! pi * 11/6
#endif
  /)

  ! sind, cosd, tand, cotand

  ! Ensure precision degrades minimally for large values.
  integer, parameter :: nphase = 5

  integer, dimension(nphase), parameter :: phases = (/ &
    0, 1, 5, 100, 10000  &
  /)

contains

  subroutine compare(strl, xl_in, xl_out, strr, xr_in, xr_out, eps)
    use ieee_arithmetic
    implicit none
    character(*), intent(in) :: strl, strr
    real(real_kind), intent(in) :: xl_in, xl_out, xr_in, xr_out, eps

    if ((ieee_is_nan(xl_out) .neqv. ieee_is_nan(xr_out)) &
        .or. (ieee_is_finite(xl_out) .neqv. ieee_is_finite(xr_out)) &
        .or. (abs(xl_out - xr_out) .gt. eps)) then
      write (*, 100) strl, "(", xl_in, "): ", xl_out
      write (*, 100) strr, "(", xr_in, "): ", xr_out

      if ((ieee_is_nan(xl_out) .eqv. ieee_is_nan(xr_out)) &
          .and. ieee_is_finite(xl_out) .and. ieee_is_finite(xr_out)) then
        write (*, 300) "|xl - xr| = ", abs(xl_out - xr_out)
        write (*, 300) "    > eps = ", eps
      endif

      call abort()
    endif

#ifdef __GFC_REAL_16__
    100  format((A8,A,F34.30,A,F34.30,F34.30))
    200  format((A12,F34.30))
    !500  format((A8,A,G34.29,A,G34.29,G34.29))
#elif defined(__GFC_REAL_10__)
    100  format((A8,A,F21.17,A,F21.17,F21.17))
    200  format((A12,F21.17))
    !500  format((A8,A,G21.16,A,G21.16,G21.16))
#else
    100  format((A8,A,F18.14,A,F18.14,F18.14))
    200  format((A12,F18.14))
    !500  format((A8,A,G18.13,A,G18.13,G18.13))
#endif
    300  format((A12,G8.2))
  endsubroutine

endmodule

use dec_math_5
use ieee_arithmetic
implicit none

integer :: phase_index, angle_index, phase
real(real_kind) :: deg_in, deg_out, deg_out2, rad_in, rad_out

! Try every value in degrees, and make sure they are correct compared to the
! corresponding radian function.

do phase_index = 1, size(phases)
  phase = phases(phase_index)

  do angle_index = 1, size(degrees)
    ! eqv to degrees(angle_index) modulo 360
    deg_in = degrees(angle_index) + phase * 360
    rad_in = radians(angle_index) + phase * tau

    ! sind vs. sin
    deg_out = sind(deg_in)
    rad_out = sin(rad_in)
    call compare("sind", deg_in, deg_out, "sin", rad_in, rad_out, eps)

    ! cosd vs. cos
    deg_out = cosd(deg_in)
    rad_out = cos(rad_in)
    call compare("cosd", deg_in, deg_out, "cos", rad_in, rad_out, eps)

    ! tand vs. tan
    deg_out = tand(deg_in)
    rad_out = tan(rad_in)
    if ( ieee_is_finite(deg_out) ) then
      call compare("tand", deg_in, deg_out, "tan", rad_in, rad_out, eps)
    endif

    ! cotand vs. cotan
    deg_out = cotand(deg_in)
    rad_out = cotan(rad_in)

    ! Skip comparing infinity, because cotan does not return infinity
    if ( ieee_is_finite(deg_out) ) then
      call compare("cotand", deg_in, deg_out, "cotan", rad_in, rad_out, eps)

      ! cotand vs. tand
      deg_out = cotand(deg_in)
      deg_out2 = -tand(deg_in + 90)

      call compare("cotand", deg_in, deg_out, "-tand+90", deg_in, deg_out2, eps)
      deg_out2 = 1 / tand(deg_in)
      call compare("cotand", deg_in, deg_out, "1/tand", deg_in, deg_out2, eps)
    endif

  enddo


enddo


end
