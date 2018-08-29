! { dg-do run }
! verifies basic functioning of the ishft and ishftc intrinsics
if (ishft (1_1, 0) /= 1) STOP 1
if (ishft (1_1, 1) /= 2) STOP 2
if (ishft (3_1, 1) /= 6) STOP 3
if (ishft (-1_1, 1) /= -2) STOP 4
if (ishft (-1_1, -1) /= 127) STOP 5
if (ishft (96_1, 2) /= -128) STOP 6

if (ishft (1_2, 0) /= 1) STOP 7
if (ishft (1_2, 1) /= 2) STOP 8
if (ishft (3_2, 1) /= 6) STOP 9
if (ishft (-1_2, 1) /= -2) STOP 10
if (ishft (-1_2, -1) /= 32767) STOP 11
if (ishft (16384_2 + 8192_2, 2) /= -32768_4) STOP 12

if (ishft (1_4, 0) /= 1) STOP 13
if (ishft (1_4, 1) /= 2) STOP 14
if (ishft (3_4, 1) /= 6) STOP 15
if (ishft (-1_4, 1) /= -2) STOP 16
if (ishft (-1_4, -1) /= 2147483647) STOP 17
if (ishft (1073741824_4 + 536870912_4, 2) /= -2147483648_8) STOP 18

if (ishft (1_8, 0) /= 1) STOP 19
if (ishft (1_8, 1) /= 2) STOP 20
if (ishft (3_8, 1) /= 6) STOP 21
if (ishft (-1_8, 1) /= -2) STOP 22
if (ishft (-1_8, -60) /= z'F') STOP 23

if (ishftc (1_1, 0) /= 1) STOP 24
if (ishftc (1_1, 1) /= 2) STOP 25
if (ishftc (3_1, 1) /= 6) STOP 26
if (ishftc (-1_1, 1) /= -1) STOP 27
if (ishftc (-1_1, -1) /= -1) STOP 28
if (ishftc (ishftc (96_1, 2), -2) /= 96) STOP 29

if (ishftc (1_2, 0) /= 1) STOP 30
if (ishftc (1_2, 1) /= 2) STOP 31
if (ishftc (3_2, 1) /= 6) STOP 32
if (ishftc (-1_2, 1) /= -1) STOP 33
if (ishftc (-1_2, -1) /= -1) STOP 34
if (ishftc (ishftc (25000_2, 2), -2) /= 25000) STOP 35

if (ishftc (1_4, 0) /= 1) STOP 36
if (ishftc (1_4, 1) /= 2) STOP 37
if (ishftc (3_4, 1) /= 6) STOP 38
if (ishftc (-1_4, 1) /= -1) STOP 39
if (ishftc (-1_4, -1) /= -1) STOP 40
if (ishftc (ishftc (1325876_4, 2), -2) /= 1325876) STOP 41

if (ishftc (1_8, 0) /= 1) STOP 42
if (ishftc (1_8, 1) /= 2) STOP 43
if (ishftc (3_8, 1) /= 6) STOP 44
if (ishftc (-1_8, 1) /= -1) STOP 45
if (ishftc (-1_8, -1) /= -1) STOP 46
if (ishftc (ishftc (1325876_8, 2), -2) /= 1325876) STOP 47
end


