! { dg-do compile }
module foo
   implicit none
   interface operator(.x.)
      module procedure product
   end interface operator(.x.)
   contains
      function product(x, y)
         real, intent(in) :: x, y
         real :: product
         product = x * y
      end function product
end module foo

module gfcbug155
   implicit none
   contains
      subroutine print_prod (x, y)
         use foo, only : operator(.x.)
         implicit none
         real :: x, y
         print *, x .x. y
      end subroutine print_prod
end module gfcbug155
