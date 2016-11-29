! { dg-do compile }
program p
   integer :: n = 1
   character(3), parameter :: x(2) = ['abc', 'xyz']
   character(2), parameter :: y(2) = [x(2)(2:3), x(n)(1:2)] ! { dg-error "CHARACTER length must be a constant" }
end
