!
! { dg-do compile }
!
! PR fortran/119836
!
program p
   implicit none
   integer, parameter :: n = 4
   integer :: i
   integer :: y(n), x(n)
   x = [(i,i=1,n)]
   do concurrent (i=1:n)
      call bar(x, y)       ! { dg-error "Subroutine call" }
   end do
   if (any(x /= y)) stop 1
   x = 2 * x
   do concurrent (i=1:n)
      block
         call bar(x, y)    ! { dg-error "Subroutine call" }
      end block
   end do
   if (any(x /= y)) stop 1

   contains
      subroutine bar(x, y)
         integer, intent(in) :: x(:)
         integer, intent(out) :: y(:)
         y = x
      end subroutine
end program p
