! { dg-do run }
! Modified program from http://groups.google.com/group/\
! comp.lang.fortran/browse_frm/thread/423e4392dc965ab7#
!
module myoperator
   contains
      function dadd(arg1,arg2)
         integer ::dadd(2)
         integer, intent(in) :: arg1(2), arg2(2)
         dadd(1)=arg1(1)+arg2(1)
         dadd(2)=arg1(2)+arg2(2)
      end function dadd
end module myoperator

program test_interface

   use myoperator

   implicit none

   interface operator (.myadd.)
      module procedure dadd
   end interface

   integer input1(2), input2(2), mysum(2)

   input1 = (/0,1/)
   input2 = (/3,3/)
   mysum = input1 .myadd. input2
   if (mysum(1) /= 3 .and. mysum(2) /= 4) call abort 

   call test_sub(input1, input2)

end program test_interface 

subroutine test_sub(input1, input2)

   use myoperator

   implicit none

   interface operator (.myadd.)
      module procedure dadd
   end interface

   integer, intent(in) :: input1(2), input2(2)
   integer mysum(2)

   mysum = input1 .myadd. input2
   if (mysum(1) /= 3 .and. mysum(2) /= 4) call abort 

end subroutine test_sub 
! { dg-final { cleanup-modules "myoperator" } }
