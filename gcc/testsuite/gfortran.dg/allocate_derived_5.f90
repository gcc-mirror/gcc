! { dg-do run }
!
! PR 45828: [4.6 Regression] No default initialization of derived type members?
!
! Contributed by Juha <jpr@csc.fi>

program fail1
   type a
      integer :: i
   end type a

   type b
     type(a) :: acomp = a(5)
   end type b

   type(b), allocatable :: c(:)

   allocate(c(1))
   if (c(1) % acomp % i /= 5) call abort()
end program fail1
