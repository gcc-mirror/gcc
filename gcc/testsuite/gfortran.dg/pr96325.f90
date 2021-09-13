! { dg-do compile }
!
! Test the fix for PR96325 in which the typebound procedure reference
! 'foo' was applied to an intrinsic type component without generating
! an error. The result of the expression was the value of the arg..
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
   implicit none

   type t2
      integer r1
   end type

   type(t2) :: t
   integer :: a

   a = t%r1%foo(1) ! { dg-error "is not an inquiry reference" }
   if (a == 42) stop

   end
