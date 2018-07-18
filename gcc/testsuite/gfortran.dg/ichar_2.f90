! { dg-do run }
! Test char and ichar intrinsic functions
Program test
integer i

if (ichar (char (0)) .ne. 0) STOP 1
if (ichar (char (255)) .ne. 255) STOP 2
if (ichar (char (127)) .ne. 127) STOP 3

i = 0
if (ichar (char (i)) .ne. i) STOP 4
i = 255
if (ichar (char (i)) .ne. i) STOP 5
i = 127
if (ichar (char (i)) .ne. i) STOP 6
end
