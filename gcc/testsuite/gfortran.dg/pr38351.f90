! { dg-do compile }
module m1
   type t1
      integer :: i
   end type t1
   interface operator(+)
      module procedure add
   end interface
   contains
      type(t1) function add(a,b)
         type(t1), intent(in) :: a,b
      end function
end module m1

program foo
   use m1
   type(t1), dimension(2,2) :: a = t1(1), b = t1(2)
   type(t1) :: c=t1(1), d=t1(2)
   c = c + d
   a = a + b   ! { dg-error "Unexpected derived-type entities" }
end program foo
