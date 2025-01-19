! { dg-do compile }
!
program foo

   implicit none

   integer i
   integer :: j = 0
   real y(4)

   do concurrent(i=1:4)
      y(i) = bar(i)        ! { dg-error "Reference to impure function" }
      call bla(i)          ! { dg-error "Subroutine call to" }
   end do

   do concurrent(i=1:4)
      block
         y(i) = bar(i)     ! { dg-error "Reference to impure function" }
         call bla(i)       ! { dg-error "Subroutine call at" }
       end block
   end do

   contains

      impure function bar(i)
         real bar
         integer, intent(in) :: i
         j = j + i
         bar = j
      end function bar

      impure subroutine bla (i)
         integer, intent(in) :: i
         j = j + i
      end subroutine bla

end program foo
