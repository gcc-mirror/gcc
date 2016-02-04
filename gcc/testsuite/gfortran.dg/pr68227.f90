! { dg-do compile }
! PR fortran/68227
! Original code contributed by Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de >
!
program p

   type t
   end type

   type t2
      type(t), pointer :: a
   end type

   type(t), target :: x
   type(t2), pointer :: y(:)
   integer :: i
   integer :: n = 2

   allocate (y(n))
   forall (i=1:n) y(i)%a = x

end program p
