! { dg-do compile }
!
! Contributed by Steve Kargl  <kargl@gcc.gnu.org>
!
module mod

   type :: objects(k1,l1)
      integer, kind :: k1 = selected_int_kind(4)
      integer, len :: l1
      integer(k1) :: p(l1+1)
   end type

   contains
      subroutine foo(n)
         integer n
         type(objects(l1=n)) :: x
         ! Any of these lines caused an ICE in compilation.
         if (x%k1 /= selected_int_kind(4)) stop 1
         if (x%l1 /= n) stop 2
         if (size(x%p) /= x%l1+1) stop 3
      end subroutine

end module

program p
   use mod
   type(objects(1,30)) :: x
   call foo(3)
end program p
