! { dg-do run }
! { dg-options "-std=f2018 -pedantic" }

character(20) :: fmt
character(9) :: buffer
fmt = "(1a1,f0.2,1a1)"
write(buffer,fmt) ">", 3.0, "<"
if (buffer.ne.">3.00<") stop 1
fmt = "(1a1,g0.2,1a1)"
write(buffer,fmt) ">", 0.3, "<"
if (buffer.ne.">0.30<") stop 2
fmt = "(1a1,d0.2,1a1)"
write(buffer,fmt) ">", 3.0, "<"
if (buffer.ne.">0.30D+1<") stop 3
fmt = "(1a1,e0.2,1a1)"
write(buffer,fmt) ">", 3.0, "<"
if (buffer.ne.">0.30E+1<") stop 4
fmt = "(1a1,en0.2,1a1)"
write(buffer,fmt) ">", 3.0, "<"
if (buffer.ne.">3.00E+0<") stop 5
fmt = "(1a1,es0.2,1a1)"
write(buffer,fmt) ">", 3.0, "<"
if (buffer.ne.">3.00E+0<") stop 6
end

