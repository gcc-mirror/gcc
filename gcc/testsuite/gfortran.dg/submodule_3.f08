! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Check enforcement of F2008 standard for MODULE PROCEDURES and SUBMODULES
! This is rather bare-bones to reduce the number of error messages too the
! essential minimum.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
 module foo_interface
   implicit none

   interface
     module function array1(this) result (that) ! { dg-error "MODULE prefix" }
     end function ! { dg-error "Expecting END INTERFACE" }
     character(16) module function array2(this, that) ! { dg-error "MODULE prefix" }
     end function ! { dg-error "Expecting END INTERFACE" }
   end interface
 end module

!
  SUBMODULE (foo_interface) foo_interface_son ! { dg-error "SUBMODULE declaration" }
!
  contains ! { dg-error "CONTAINS statement without FUNCTION or SUBROUTINE" }

     module function array1 (this) result(that) ! { dg-error "MODULE prefix" }
     end function ! { dg-error "Expecting END PROGRAM" }

! Test array characteristics for dummy and result are OK for
! abbreviated module procedure declaration.
     module procedure array2 ! { dg-error "must be in a generic module interface" }
     end PROCEDURE ! { dg-error "Expecting END PROGRAM" }

  end SUBMODULE foo_interface_son ! { dg-error "Expecting END PROGRAM" }

end

