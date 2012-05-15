! { dg-do compile }
!
! PR 46330: [4.6 Regression] [OOP] ICE after revision 166368
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
! Taken from http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/76f99e7fd4f3e772

module type2_type 
 implicit none 
 type, abstract :: Type2 
 end type Type2 
end module type2_type 

module extended2A_type 
 use type2_type 
 implicit none 
 type, extends(Type2) :: Extended2A 
    real(kind(1.0D0)) :: coeff1 = 1. 
 contains 
    procedure :: setCoeff1 => Extended2A_setCoeff1 
 end type Extended2A 
 contains 
    function Extended2A_new(c1, c2) result(typePtr_) 
       real(kind(1.0D0)), optional, intent(in) :: c1 
       real(kind(1.0D0)), optional, intent(in) :: c2 
       type(Extended2A), pointer  :: typePtr_ 
       type(Extended2A), save, allocatable, target  :: type_ 
       allocate(type_) 
       typePtr_ => null() 
       if (present(c1)) call type_%setCoeff1(c1) 
       typePtr_ => type_ 
       if ( .not.(associated (typePtr_))) then 
          stop 'Error initializing Extended2A Pointer.' 
       endif 
    end function Extended2A_new 
    subroutine Extended2A_setCoeff1(this,c1) 
       class(Extended2A) :: this 
       real(kind(1.0D0)), intent(in) :: c1 
       this% coeff1 = c1 
    end subroutine Extended2A_setCoeff1 
end module extended2A_type 

module type1_type 
 use type2_type 
 implicit none 
 type Type1 
    class(type2), pointer :: type2Ptr => null() 
 contains 
    procedure :: initProc => Type1_initProc 
 end type Type1 
 contains 
    function Type1_initProc(this) result(iError) 
       use extended2A_type 
       implicit none 
       class(Type1) :: this 
       integer :: iError 
          this% type2Ptr => extended2A_new() 
          if ( .not.( associated(this% type2Ptr))) then 
             iError = 1 
             write(*,'(A)') "Something Wrong." 
          else 
             iError = 0 
          endif 
    end function Type1_initProc 
end module type1_type
