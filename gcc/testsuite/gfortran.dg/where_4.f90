! { dg-do compile }
! PR 60522 - this used to ICE.
! Original test case Roger Ferrer Ibanez
subroutine foo(a, b)
   implicit none
   integer, dimension(:), intent(inout) :: a
   integer, dimension(:), intent(in) :: b

   where (b(:) > 0)
      where (b(:) > 100)
         a(lbound(a, 1):ubound(a, 1)) = b(lbound(b, 1):ubound(b, 1)) * b(lbound(b, 1):ubound(b, 1)) - 100
      elsewhere
         a(lbound(a, 1):ubound(a, 1)) = b(lbound(b, 1):ubound(b, 1)) * b(lbound(b, 1):ubound(b, 1))
      end where
   elsewhere
      a(lbound(a, 1):ubound(a, 1)) = - b(lbound(b, 1):ubound(b, 1)) * b(lbound(b, 1):ubound(b, 1))
   end where
end subroutine foo
