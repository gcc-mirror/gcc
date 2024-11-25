! { dg-do compile }
!
program foo

   implicit none

   integer i
   integer :: j = 0
   real y(4)

   do concurrent(i=1:4)
      y(i) = bar(i)        ! { dg-error "Reference to impure function" }
   end do

   do concurrent(i=1:4)
      block
         y(i) = bar(i)     ! { dg-error "Reference to impure function" }
      end block
   end do

   contains

      impure function bar(i)
         real bar
         integer, intent(in) :: i
         j = j + i
         bar = j
      end function bar

end program foo
