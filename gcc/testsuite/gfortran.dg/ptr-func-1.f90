! { dg-do compile }
! { dg-options "-std=f2008 " }
!
! PR fortran/46100
!
! Pointer function as definable actual argument
! - a Fortran 2008 feature
!
integer, target :: tgt
call one (two ())
if (tgt /= 774) STOP 1
contains
  subroutine one (x)
    integer, intent(inout) :: x
    if (x /= 34) STOP 2
    x = 774
  end subroutine one
  function two ()
    integer, pointer :: two
    two => tgt 
    two = 34
  end function two
end

