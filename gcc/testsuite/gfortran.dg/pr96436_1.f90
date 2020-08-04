! { dg-do run }
! { dg-options "-std=f95 -pedantic" }

character(20) :: fmt
character(9) :: buffer
fmt = "(1a1,f0.2,1a1)"
write(buffer,fmt) ">", 3.0, "<"
if (buffer.ne.">3.00<") stop 1
end

