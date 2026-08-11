! { dg-do compile }
!
! PR fortran/126798
!
! F2023:C1105 - a selector shall be neither a designator of a procedure
! pointer nor a function reference that returns a procedure pointer.
! The function-reference case used to ICE in verify_gimple, or be accepted
! silently when the result had an integer interface.
!
! Calling a procedure pointer is of course still permitted; only a result
! that is itself a procedure pointer is rejected.

module m
  abstract interface
    function ifc_ptr () result (r)
      procedure(real), pointer :: r
    end function
    real function ifc_real ()
    end function
  end interface
contains
  function getp () result (r)
    procedure(real), pointer :: r
    r => null ()
  end function
  function getp_int () result (r)
    procedure(integer), pointer :: r
    r => null ()
  end function
  function getp_sub () result (r)
    procedure(), pointer :: r
    r => null ()
  end function
end module

program associate_85
  use m
  implicit none
  type t
    procedure(real), pointer, nopass :: ppc => null ()
  end type
  type(t) :: x
  procedure(real), pointer :: pp => null ()
  procedure(ifc_ptr), pointer :: qp => null ()
  procedure(ifc_real), pointer :: qr => null ()

  ! Procedure pointer designators.
  associate (a => pp)        ! { dg-error "is a procedure pointer" }
  end associate

  associate (b => x%ppc)     ! { dg-error "is a procedure pointer" }
  end associate

  ! Function references returning a procedure pointer.
  associate (c => getp ())      ! { dg-error "returns a procedure pointer" }
  end associate

  associate (d => getp_int ())  ! { dg-error "returns a procedure pointer" }
  end associate

  associate (e => getp_sub ())  ! { dg-error "returns a procedure pointer" }
  end associate

  associate (f => qp ())        ! { dg-error "returns a procedure pointer" }
  end associate

  ! Legal: calling a procedure pointer yields its result, not a pointer.
  associate (g => pp ())
    print *, g
  end associate

  associate (h => qr ())
    print *, h
  end associate

  associate (i => x%ppc ())
    print *, i
  end associate
end program
