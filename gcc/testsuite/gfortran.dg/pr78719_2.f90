! { dg-do compile }
! PR fortran/78719
! Code contributed by Gerhard Steinmetz 
program p

   type t
      integer :: n
   end type

   real :: g

   abstract interface
      subroutine h
      end
   end interface

   procedure(h), pointer :: s

   s => f
   call s
   s => g            ! { dg-error "Invalid procedure pointer" }
   call s

   contains

      subroutine f
      end

      subroutine g   ! { dg-error "has an explicit interface" }
      end

end program p        ! { dg-error "Syntax error" }
