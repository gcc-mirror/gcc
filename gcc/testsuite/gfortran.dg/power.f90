! { dg-do run }
integer i
i = 0
if ( a (i) ** 5 .ne. 1) STOP 1
contains
function a (i)
integer a, i
i = i + 1
a = i
end function
end
