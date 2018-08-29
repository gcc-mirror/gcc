! { dg-do run }
! PR libfortran/20101
! With format "PE", 0.0 must still have "+00" as exponent
character(len=10) :: c1, c2
write(c1,"(1pe9.2)") 0.0
write(c2,"(1pe9.2)") 1.0
if (trim(adjustl(c1)) .ne. "0.00E+00") STOP 1
if (trim(adjustl(c2)) .ne. "1.00E+00") STOP 2
end
