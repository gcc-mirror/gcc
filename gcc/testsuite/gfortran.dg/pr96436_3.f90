! { dg-do run }
! { dg-options "-std=f2008 -pedantic" }

character(20) :: fmt
character(9) :: buffer
fmt = "(1a1,f0.2,1a1)"
write(buffer,fmt) ">", 3.0, "<"
if (buffer.ne.">3.00<") stop 1
fmt = "(1a1,g0.2,1a1)"
write(buffer,fmt) ">", 0.3, "<"
if (buffer.ne.">0.30<") stop 2
end

