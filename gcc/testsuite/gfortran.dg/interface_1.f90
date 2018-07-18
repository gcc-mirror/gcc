! { dg-do compile }
! This program would segfault without the patch for PR fortran/24005.
module y
  !
  ! If private statement is removed, then we get a bunch of errors
  !
  private f
  !
  ! If we rename 'f' in module y to say 'g', then gfortran correctly
  ! identifies ambiguous as being ambiguous.
  !
  interface ambiguous
    module procedure f
  end interface

  contains

    real function f(a)
      real a
      f = a
    end function

end module y

module z

  use y    ! { dg-warning "in generic interface" }

  interface ambiguous
    module procedure f 
  end interface

  contains

    real function f(a)   ! { dg-warning "in generic interface" }
      real a
      f = a
    end function

end module z
