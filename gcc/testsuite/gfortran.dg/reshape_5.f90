! { dg-do compile }
!
! PR fortran/56849
!
integer :: x(2,2),y(4)
y = reshape([1,2,3,4],[4])
x(:,1:1) = reshape(y(::2), [1,2], order=[1,2]) ! { dg-error "Different shape for array assignment at .1. on dimension 1 .2 and 1." }
print *, y
print *, x(:,1)
end
