! Test the MVBITS intrinsic subroutine
INTEGER*4 :: from, to, result

DATA from / z'0003FFFC' /
DATA to / z'77760000' /
DATA result / z'7777FFFE' /

CALL mvbits(from, 2, 16, to, 1)
if (to /= result) CALL abort()
end
