! { dg-do compile }
!
! PR 44558: [OOP] ICE on invalid code: called TBP subroutine as TBP function
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module ice5
  type::a_type
   contains  
     procedure::a_subroutine_1
     procedure::a_subroutine_2
  end type a_type
contains
  real function a_subroutine_1(this)
    class(a_type)::this
    real::res
    res=this%a_subroutine_2()     ! { dg-error "should be a FUNCTION" }
  end function
  subroutine a_subroutine_2(this)
    class(a_type)::this
    call this%a_subroutine_1()    ! { dg-error "should be a SUBROUTINE" }
  end subroutine
end module ice5
 
! { dg-final { cleanup-modules "ice5" } }
