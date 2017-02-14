! { dg-do compile }
! { dg-options "-Wall" }
module foo

   implicit none

   private
   public f1,f2

   contains

      integer function f1()
         integer f2
         integer f3           ! { dg-warning "Unused variable" }
         f1=5
      entry f2
         f2=8
      end function
end module

program test
   use foo
   implicit none
   print *,f2()
end program
! { dg-final { cleanup-modules "foo" } }
