! { dg-do compile }
subroutine f(n, x, y)

   implicit none

   integer, parameter :: knd = kind(1.e0)

   integer, intent(in) :: n
   complex(knd), intent(in) :: x(1:n)

   integer i
   real(knd) y(2*n)
   
   y = [real(x), aimag(x)]
   y = [real(x(1:n)), aimag(x(1:n))]
   y = [real(knd) :: 1] 
   y = [real(kind=42) :: 1] ! { dg-error "Invalid type-spec" }
   y = [real(kind=knd) :: 1]
   y = [real(kind=knd, a=1.)]
   y = [real(a=1.)]
   y = [real(a=1, kind=knd)]

end subroutine f
