! { dg-do compile }
!
! PR 40541: Assignment checking for proc-pointer => proc-ptr-returning-function()
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program test
  procedure(real), pointer :: p
  p => f()  ! { dg-error "Type mismatch in function result" }
contains
 function f()
   pointer :: f
   interface
     logical(1) function f()
     end function
   end interface
   f = .true._1
 end function f
end program test
