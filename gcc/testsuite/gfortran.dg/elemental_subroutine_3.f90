! { dg-do run }
! Test the fix for PR25746, in which dependency checking was not being
! done for elemental subroutines and therefore for interface assignments.
!
! This test is based on
! http://home.comcast.net/~kmbtib/Fortran_stuff/elem_assign.f90
! as reported by Harald Anlauf <anlauf@gmx.de> in the PR.
! 
module elem_assign
   implicit none
   type mytype
      integer x
   end type mytype
   interface assignment(=)
      module procedure myassign
   end interface assignment(=)
   contains
      elemental subroutine myassign(x,y)
         type(mytype), intent(out) :: x
         type(mytype), intent(in) :: y
! Multiply the components by 2 to verify that this is being called.
         x%x = y%x*2
      end subroutine myassign
end module elem_assign

program test
   use elem_assign
   implicit none
   type(mytype) :: y(6), x(6) = (/mytype(1),mytype(20),mytype(300),&
                                  mytype(4000),mytype(50000),&
                                  mytype(1000000)/)
   type(mytype) :: z(2, 3)
! The original case - dependency between lhs and rhs. 
   x = x((/2,3,1,4,5,6/))
   if (any(x%x .ne. (/40, 600, 2, 8000, 100000, 2000000/))) call abort ()
! Slightly more elborate case with non-trivial array ref on lhs.
   x(4:1:-1) = x((/1,3,2,4/))
   if (any(x%x .ne. (/16000, 1200, 4, 80, 100000, 2000000/))) call abort ()
! Check that no-dependence case works....
   y = x
   if (any(y%x .ne. (/32000, 2400, 8, 160, 200000, 4000000/))) call abort ()
! ...and now a case that caused headaches during the preparation of the patch
   x(2:5) = x(1:4)
   if (any(x%x .ne. (/16000, 32000, 2400, 8, 160, 2000000/))) call abort ()
! Check offsets are done correctly in multi-dimensional cases
   z = reshape (x, (/2,3/))
   z(:, 3:2:-1) = z(:, 1:2)
   y = reshape (z, (/6/))
   if (any(y%x .ne. (/ 64000, 128000, 19200, 64, 128000, 256000/))) call abort ()
end program test
