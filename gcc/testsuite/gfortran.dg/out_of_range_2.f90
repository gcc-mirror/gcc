! { dg-do run }
! { dg-additional-options "-funsigned" }
!
! PR fortran/115788 - OUT_OF_RANGE

program p
  use iso_fortran_env, only: int8, int64, uint8, uint64, real32, real64
  implicit none
  integer          :: i
  integer(int8)    :: i1
  integer(int64)   :: i8
  unsigned         :: u
  unsigned(uint8)  :: u1
  unsigned(uint64) :: u8
  real(real32)     :: r
  real(real64)     :: d
  logical          :: t = .true., f = .false.

  real,    parameter :: a(*)       = [-0.5, 0.5, 254.5, 255.5]
  logical, parameter :: l1(*)      = OUT_OF_RANGE (a, 0U_uint8)
  logical, parameter :: l2(*)      = OUT_OF_RANGE (a, 0U_uint8, .true.)
  logical, parameter :: expect1(*) = [.false.,.false.,.false.,.false.]
  logical, parameter :: expect2(*) = [.true. ,.false.,.false.,.true. ]
  real               :: b(size(a)) = a

  ! Check for correct truncation or rounding, compile-time
  if (any (l1 .neqv. expect1)) stop 1
  if (any (l2 .neqv. expect2)) stop 2

  ! Check for correct truncation or rounding, run-time
  if (any (OUT_OF_RANGE (a, 0U_uint8, f) .neqv. expect1)) stop 3
  if (any (OUT_OF_RANGE (a, 0U_uint8, t) .neqv. expect2)) stop 4

  if (any (OUT_OF_RANGE (b, 0U_uint8)          .neqv. expect1)) stop 5
  if (any (OUT_OF_RANGE (b, 0U_uint8, .false.) .neqv. expect1)) stop 6
  if (any (OUT_OF_RANGE (b, 0U_uint8, .true.)  .neqv. expect2)) stop 7
  if (any (OUT_OF_RANGE (b, 0U_uint8, f)       .neqv. expect1)) stop 8
  if (any (OUT_OF_RANGE (b, 0U_uint8, t)       .neqv. expect2)) stop 9

  ! Miscellaneous "obvious" special cases
  u1 = huge (0U_uint8)
  u  = huge (0U)
  u8 = huge (0U_uint64)
  r  = huge (0._real32)
  d  = real (r, real64)
  if (OUT_OF_RANGE (huge (0U_uint8), r)) stop 10
  if (OUT_OF_RANGE (huge (0U_uint8), d)) stop 11
  if (OUT_OF_RANGE (huge (0U_uint8), u)) stop 12
  if (OUT_OF_RANGE (u1,            u)) stop 13
  if (OUT_OF_RANGE (r,             d)) stop 14
  if (OUT_OF_RANGE (d,             r)) stop 15
  if (OUT_OF_RANGE (u,             r)) stop 16
  if (OUT_OF_RANGE (u8,            r)) stop 17
  if (OUT_OF_RANGE (u,            u8)) stop 18

  if (OUT_OF_RANGE (real (u1),      u1,f)) stop 19
  if (OUT_OF_RANGE (real (u,real64), u,f)) stop 20

  if (.not. OUT_OF_RANGE (u,      u1)) stop 21
  if (.not. OUT_OF_RANGE (u8,      u)) stop 22
  if (.not. OUT_OF_RANGE (r,      u8)) stop 23
  if (.not. OUT_OF_RANGE (d,      u8)) stop 24

  ! Check passing of optional argument
  if (any (out_of_range_1 (b, f) .neqv. OUT_OF_RANGE (b, 0U_uint8, f))) stop 25
  if (any (out_of_range_1 (b, t) .neqv. OUT_OF_RANGE (b, 0U_uint8, t))) stop 26
  if (any (out_of_range_1 (b)    .neqv. OUT_OF_RANGE (b, 0U_uint8)   )) stop 27

  if (any (out_of_range_2 (b,u1,f) .neqv. OUT_OF_RANGE (b,0U_uint8,f))) stop 28
  if (any (out_of_range_2 (b,u1,t) .neqv. OUT_OF_RANGE (b,0U_uint8,t))) stop 29
  if (any (out_of_range_2 (b,u1)   .neqv. OUT_OF_RANGE (b,0U_uint8)  )) stop 30

  ! Conversions between integer and unsigned
  i1 = huge (0_int8)
  i  = huge (0)
  i8 = huge (0_int64)

  if (OUT_OF_RANGE (i1, u1)) stop 31
  if (OUT_OF_RANGE (i,   u)) stop 32
  if (OUT_OF_RANGE (i8, u8)) stop 33
  if (OUT_OF_RANGE (u1,  i)) stop 34

  if (.not. OUT_OF_RANGE (-i1, u1)) stop 35
  if (.not. OUT_OF_RANGE (-i,   u)) stop 36
  if (.not. OUT_OF_RANGE (-i8, u8)) stop 37

  if (.not. OUT_OF_RANGE (u1, i1)) stop 38
  if (.not. OUT_OF_RANGE (u,   i)) stop 39
  if (.not. OUT_OF_RANGE (u8, i8)) stop 40

contains

  elemental logical function out_of_range_1 (x, round)
    real,    intent(in)           :: x
    logical, intent(in), optional :: round

    out_of_range_1 = out_of_range (x, 0U_uint8, round)
  end function out_of_range_1

  elemental logical function out_of_range_2 (x, mold, round) result (res)
    real,     intent(in)           :: x
    class(*), intent(in)           :: mold
    logical,  intent(in), optional :: round

    select type (mold)
    type is (integer(int8))
       res = out_of_range (x, 0_int8, round)
    type is (unsigned(uint8))
       res = out_of_range (x, 0U_uint8, round)
    class default
       error stop 99
    end select
  end function out_of_range_2

end
