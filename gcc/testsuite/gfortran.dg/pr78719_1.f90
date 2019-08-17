! { dg-do run }
! PR fortran/78719
! Code contributed by Gerhard Steinmetz 
program p

   type t
      integer :: n
   end type

   abstract interface
      subroutine h
      end
   end interface

   procedure(h), pointer :: s

   s => f
   call s
   s => g
   call s

   contains

      subroutine f
      end

      subroutine g
      end
end program p
