! { dg-do run }
! { dg-additional-sources deferred_character_33a.f90 }
! PR fortran/90744 - this used to pass a wrong length
! to an external function without a prototype.
! Original test case by Tomáš Trnka.
module StringModule
   implicit none

contains
   function getstr()
      character(:), allocatable :: getstr

      getstr = 'OK'
   end function
end module
module TestModule
   use StringModule
   implicit none

contains
   subroutine DoTest()
      if (.false.) then
         call convrs('A',getstr())
      else
         call convrs('B',getstr())
      end if
   end subroutine
end module
program external_char_length
   use TestModule

   implicit none

   call DoTest()
end program
