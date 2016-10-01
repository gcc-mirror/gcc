subroutine foo(self,value)
integer(kind=kind(1)), dimension(:) :: self
integer(kind=kind(1)), intent(in) :: value
integer(kind=kind(1)) :: x,y,sign
intent(inout) :: self
integer(kind=kind(1)) :: len,i

len = size(self)
do i = 1,len
  x = self(i)
  if (x==0.0d0) cycle
  y = abs(x)
  sign = x/y
  self(i) = sign*min(value,y)
end do

end subroutine
