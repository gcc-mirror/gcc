! { dg-do run }
!
! PR fortran/99111
!
program p
   implicit none
   type t
      integer :: a(1)
   end type
   type(t), parameter :: x(3) = [t(transfer('("he', 1)), &
                                 t(transfer('llo ', 1)), &
                                 t(transfer('W1")', 1))]

   integer, parameter :: y(3) = transfer('("hello W2")', 1, size=3)
   real, parameter :: z(3) = transfer('("hello W3")', 1.0, size=3)

   print y      ! { dg-warning "Legacy Extension: Non-character in FORMAT" }
   print z      ! { dg-warning "Legacy Extension: Non-character in FORMAT" }
   print x%a(1) ! { dg-warning "Legacy Extension: Non-character in FORMAT" }
end

! { dg-output "hello W2(\r*\n+)hello W3(\r*\n+)hello W1" }
