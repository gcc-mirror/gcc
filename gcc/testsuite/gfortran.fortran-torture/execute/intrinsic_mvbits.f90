! Test the MVBITS intrinsic subroutine
INTEGER*4 :: from, to, result
integer*8 :: from8, to8

DATA from / z'0003FFFC' /
DATA to / z'77760000' /
DATA result / z'7777FFFE' /

CALL mvbits(from, 2, 16, to, 1)
if (to /= result) STOP 1

to8 = 0_8
from8 = b'1011'*2_8**32
call mvbits (from8, 33, 3, to8, 2)
if (to8 /= b'10100') STOP 1
end
