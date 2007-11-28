! { dg-do compile }
! PR32928 DATA statement with array element as initializer is rejected
integer, parameter,dimension(4) :: myint = [ 4,3,2,1 ]
integer :: a(5),b
data a(1:2) / myint(b), myint(2) / ! { dg-error "Invalid initializer" }
end
