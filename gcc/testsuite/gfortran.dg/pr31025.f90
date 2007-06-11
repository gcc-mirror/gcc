! { dg-options "-O2" }
real*8 function f(x)
t1 = g(0)
if(x .eq. 0) then
  f = 0
else if(x .eq. 1) then
  f = t1 *log( t1 )
end if
end
