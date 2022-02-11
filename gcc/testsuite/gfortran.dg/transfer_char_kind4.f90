! { dg-do run }
! PR fortran/83079 - ICE and wrong code with TRANSFER and character(kind=4)
! Exercise TRANSFER intrinsic to check character result length and shape

program p
  implicit none
  character(len=*,kind=4), parameter :: a = 4_'ABCDEF'
  character(len=6,kind=4)            :: b = 4_'abcdef'
  character(len=*,kind=4), parameter :: c = 4_'XY'
  character(len=2,kind=4)            :: d = 4_'xy'
  integer :: k, l
  k = len (a)
  l = len (c)

! print *, transfer(4_'xy', [4_'a'])

  ! TRANSFER with rank-0 result
  call chk0 (transfer (4_'ABCD', 4_'XY'), 2, 1)
  call chk0 (transfer (4_'ABCD', c     ), l, 2)
  call chk0 (transfer (4_'ABCD', d     ), l, 3)
  call chk0 (transfer (a       , 4_'XY'), 2, 4)
  call chk0 (transfer (a       , c     ), l, 5)
  call chk0 (transfer (a       , d     ), l, 6)
  call chk0 (transfer (b       , 4_'XY'), 2, 7)
  call chk0 (transfer (b       , c     ), l, 8)
  call chk0 (transfer (b       , d     ), l, 9)

  call chk0 (transfer ([4_'ABCD'], 4_'XY'), 2, 11)
  call chk0 (transfer ([4_'ABCD'], c     ), l, 12)
  call chk0 (transfer ([4_'ABCD'], d     ), l, 13)
  call chk0 (transfer ([a       ], 4_'XY'), 2, 14)
  call chk0 (transfer ([a       ], c     ), l, 15)
  call chk0 (transfer ([a       ], d     ), l, 16)
  call chk0 (transfer ([b       ], 4_'XY'), 2, 17)
  call chk0 (transfer ([b       ], c     ), l, 18)
  call chk0 (transfer ([b       ], d     ), l, 19)

  ! TRANSFER with rank-1 result
  call chk1 (transfer (4_'ABCD', [4_'XY']), 2,   2, 21)
  call chk1 (transfer (4_'ABCD', [c]     ), 2,   2, 22)
  call chk1 (transfer (4_'ABCD', [d]     ), 2,   2, 23)
  call chk1 (transfer (a       , [4_'XY']), 2, k/2, 24)
  call chk1 (transfer (a       , [c]     ), l, k/l, 25)
  call chk1 (transfer (a       , [d]     ), l, k/l, 26)
  call chk1 (transfer (b       , [4_'XY']), 2, k/2, 27)
  call chk1 (transfer (b       , [c]     ), l, k/l, 28)
  call chk1 (transfer (b       , [d]     ), l, k/l, 29)

  call chk1 (transfer (4_'ABCD', 4_'XY',size=2), 2, 2, 31)
  call chk1 (transfer (4_'ABCD', c     ,size=2), 2, 2, 32)
  call chk1 (transfer (4_'ABCD', d     ,size=2), 2, 2, 33)
  call chk1 (transfer (a       , 4_'XY',size=3), 2, 3, 34)
  call chk1 (transfer (a       , c     ,size=3), l, 3, 35)
  call chk1 (transfer (a       , d     ,size=3), l, 3, 36)
  call chk1 (transfer (b       , 4_'XY',size=3), 2, 3, 37)
  call chk1 (transfer (b       , c     ,size=3), l, 3, 38)
  call chk1 (transfer (b       , d     ,size=3), l, 3, 39)

  call chk1 (transfer (4_'ABCD', [4_'XY'],size=2), 2, 2, 41)
  call chk1 (transfer (4_'ABCD', [c]     ,size=2), 2, 2, 42)
  call chk1 (transfer (4_'ABCD', [d]     ,size=2), 2, 2, 43)
  call chk1 (transfer (a       , [4_'XY'],size=3), 2, 3, 44)
  call chk1 (transfer (a       , [c]     ,size=3), l, 3, 45)
  call chk1 (transfer (a       , [d]     ,size=3), l, 3, 46)
  call chk1 (transfer (b       , [4_'XY'],size=3), 2, 3, 47)
  call chk1 (transfer (b       , [c]     ,size=3), l, 3, 48)
  call chk1 (transfer (b       , [d]     ,size=3), l, 3, 49)

  call chk1 (transfer ([4_'ABCD'], [4_'XY']), 2,   2, 51)
  call chk1 (transfer ([4_'ABCD'], [c]     ), 2,   2, 52)
  call chk1 (transfer ([4_'ABCD'], [d]     ), 2,   2, 53)
  call chk1 (transfer ([a       ], [4_'XY']), 2, k/2, 54)
  call chk1 (transfer ([a       ], [c]     ), l, k/l, 55)
  call chk1 (transfer ([a       ], [d]     ), l, k/l, 56)
  call chk1 (transfer ([b       ], [4_'XY']), 2, k/2, 57)
  call chk1 (transfer ([b       ], [c]     ), l, k/l, 58)
  call chk1 (transfer ([b       ], [d]     ), l, k/l, 59)

  call chk1 (transfer (4_'ABCD', c     ,size=4/l), l, 4/l, 62)
  call chk1 (transfer (4_'ABCD', d     ,size=4/l), l, 4/l, 63)
  call chk1 (transfer (a       , 4_'XY',size=k/2), 2, k/2, 64)
  call chk1 (transfer (a       , c     ,size=k/l), l, k/l, 65)
  call chk1 (transfer (a       , d     ,size=k/l), l, k/l, 66)
  call chk1 (transfer (b       , 4_'XY',size=k/2), 2, k/2, 67)
  call chk1 (transfer (b       , c     ,size=k/l), l, k/l, 68)
  call chk1 (transfer (b       , d     ,size=k/l), l, k/l, 69)

contains
  ! Validate rank-0 result
  subroutine chk0 (str, l, stopcode)
    character(kind=4,len=*), intent(in) :: str
    integer,                 intent(in) :: l, stopcode
    integer :: i, p
    i = len  (str)
    p = verify (str, a // b) ! Check for junk characters
    if (i /= l .or. p > 0) then
       print *, stopcode, "len=", i, i == l, ">", str, "<"
       stop stopcode
    end if
  end subroutine chk0

  ! Validate rank-1 result
  subroutine chk1 (str, l, m, stopcode)
    character(kind=4,len=*), intent(in) :: str(:)
    integer,                 intent(in) :: l, m, stopcode
    integer :: i, j, p
    i = len  (str)
    j = size (str)
    p = maxval (verify (str, a // b)) ! Check for junk characters
    if (i /= l .or. j /= m .or. p > 0) then
       print *, stopcode, "len=", i, i == l, "size=", j, j == m, ">", str, "<"
       stop stopcode
    end if
  end subroutine chk1
end
