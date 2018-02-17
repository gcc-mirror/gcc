subroutine foo(i,c)
character c
integer i
character(1),parameter :: hex_chars(0:15)=&
  (/'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'/)

c = hex_chars(i)
end

program strarray_2
  character c
  call foo(3,c)
  if (c.ne.'3') STOP 1
end
