! { dg-do compile }
!
! PR 63674: [F03] procedure pointer and non/pure procedure
!
! Contributed by Valery Weber <valeryweber@hotmail.com>

program prog
  interface
    integer function nf()
    end function
    pure integer function pf()
    end function
    subroutine ns()
    end subroutine
    pure subroutine ps()
    end subroutine
  end interface
  type :: t
    procedure(nf), nopass, pointer :: nf => NULL()  ! non-pure function
    procedure(pf), nopass, pointer :: pf => NULL()  ! pure function
    procedure(ns), nopass, pointer :: ns => NULL()  ! non-pure subroutine
    procedure(ps), nopass, pointer :: ps => NULL()  ! pure subroutine
  end type
contains
  pure integer function eval(a)
    type(t), intent(in) :: a
    eval = a%pf()
    eval = a%nf()   ! { dg-error "Reference to impure function" }
    call a%ps()
    call a%ns()     ! { dg-error "is not PURE" }
  end function
end
