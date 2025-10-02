! { dg-do compile }
!
! Check PR105380 has gone away. Used to ICE with, "internal compiler error:
! tree check: expected array_type, have record_type in ....."
!
! Contributed by Martin Liska  <marxin@gcc.gnu.org>
!
program p
   type t(n)
      integer, len :: n
   end type
   type t2(m)
      integer, len :: m
      type(t(1)) :: a(m)
   end type
   type(t2(3)) :: x

   print *, x%m, size (x%a), x%a%n ! Outputs 3 3 1 as expected.
end
