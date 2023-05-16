! { dg-do compile }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   use iso_c_binding
   type, bind(c) :: t
      integer(c_int) :: a
   end type
   interface
      function f(x) bind(c) result(z)
         import :: c_int, t
         type(t) :: x(:)
         integer(c_int) :: z
      end
   end interface
   class(*), allocatable :: y(:)
   n = f(y) ! { dg-error "either an unlimited polymorphic or assumed type" }
end
