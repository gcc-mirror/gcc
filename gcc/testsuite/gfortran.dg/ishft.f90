! { dg-do run }
! verifies basic functioning of the ishft and ishftc intrinsics
if (ishft (1_1, 0) /= 1) call abort
if (ishft (1_1, 1) /= 2) call abort
if (ishft (3_1, 1) /= 6) call abort
if (ishft (-1_1, 1) /= -2) call abort
if (ishft (-1_1, -1) /= 127) call abort
if (ishft (96_1, 2) /= -128) call abort

if (ishft (1_2, 0) /= 1) call abort
if (ishft (1_2, 1) /= 2) call abort
if (ishft (3_2, 1) /= 6) call abort
if (ishft (-1_2, 1) /= -2) call abort
if (ishft (-1_2, -1) /= 32767) call abort
if (ishft (16384_2 + 8192_2, 2) /= -32768_4) call abort

if (ishft (1_4, 0) /= 1) call abort
if (ishft (1_4, 1) /= 2) call abort
if (ishft (3_4, 1) /= 6) call abort
if (ishft (-1_4, 1) /= -2) call abort
if (ishft (-1_4, -1) /= 2147483647) call abort
if (ishft (1073741824_4 + 536870912_4, 2) /= -2147483648_8) call abort

if (ishft (1_8, 0) /= 1) call abort
if (ishft (1_8, 1) /= 2) call abort
if (ishft (3_8, 1) /= 6) call abort
if (ishft (-1_8, 1) /= -2) call abort
if (ishft (-1_8, -60) /= z'F') call abort

if (ishftc (1_1, 0) /= 1) call abort
if (ishftc (1_1, 1) /= 2) call abort
if (ishftc (3_1, 1) /= 6) call abort
if (ishftc (-1_1, 1) /= -1) call abort
if (ishftc (-1_1, -1) /= -1) call abort
if (ishftc (ishftc (96_1, 2), -2) /= 96) call abort

if (ishftc (1_2, 0) /= 1) call abort
if (ishftc (1_2, 1) /= 2) call abort
if (ishftc (3_2, 1) /= 6) call abort
if (ishftc (-1_2, 1) /= -1) call abort
if (ishftc (-1_2, -1) /= -1) call abort
if (ishftc (ishftc (25000_2, 2), -2) /= 25000) call abort

if (ishftc (1_4, 0) /= 1) call abort
if (ishftc (1_4, 1) /= 2) call abort
if (ishftc (3_4, 1) /= 6) call abort
if (ishftc (-1_4, 1) /= -1) call abort
if (ishftc (-1_4, -1) /= -1) call abort
if (ishftc (ishftc (1325876_4, 2), -2) /= 1325876) call abort

if (ishftc (1_8, 0) /= 1) call abort
if (ishftc (1_8, 1) /= 2) call abort
if (ishftc (3_8, 1) /= 6) call abort
if (ishftc (-1_8, 1) /= -1) call abort
if (ishftc (-1_8, -1) /= -1) call abort
if (ishftc (ishftc (1325876_8, 2), -2) /= 1325876) call abort
end


