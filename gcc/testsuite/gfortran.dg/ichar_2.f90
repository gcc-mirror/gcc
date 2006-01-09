! { dg-do run }
! Test char and ichar intrinsic functions
Program test
integer i

if (ichar (char (0)) .ne. 0) call abort ()
if (ichar (char (255)) .ne. 255) call abort ()
if (ichar (char (127)) .ne. 127) call abort ()

i = 0
if (ichar (char (i)) .ne. i) call abort ()
i = 255
if (ichar (char (i)) .ne. i) call abort ()
i = 127
if (ichar (char (i)) .ne. i) call abort ()
end
