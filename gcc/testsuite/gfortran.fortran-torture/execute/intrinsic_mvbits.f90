! Test the MVBITS intrinsic subroutine
INTEGER*4 :: from, to, result
integer*8 :: to8

DATA from / z'0003FFFC' /
DATA to / z'77760000' /
DATA result / z'7777FFFE' /

CALL mvbits(from, 2, 16, to, 1)
if (to /= result) CALL abort()

to8 = 0
call mvbits (b'1011'_8*2_8**32, 33, 3, to8, 2) ! { dg-warning "" "" }
if (to8 /= b'10100'_8) call abort ! { dg-warning "" "" }
end
