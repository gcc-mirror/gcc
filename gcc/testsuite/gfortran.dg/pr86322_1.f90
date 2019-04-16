! { dg-do compile }
program foo
   implicit none
   type a
      integer i
   end type a
   type(a), target, save :: b
   type(a), pointer :: c
   data b%i /42/
   data c%i /b%i/          ! { dg-error "is not rightmost part-ref" }
   if (c%i == 42) c%i = 1  ! Unreachable
end program foo
