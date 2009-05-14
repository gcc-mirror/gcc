! { dg-do compile }
!
! PR 39996: Double typing of function results not detected
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  interface
    real function A ()
    end function
  end interface
  real :: A  ! { dg-error "already has basic type of" }

  real :: B
  interface
    real function B ()  ! { dg-error "already has basic type of" }
    end function  ! { dg-error "Expecting END INTERFACE statement" }
  end interface

  interface
    function C ()
      real :: C
    end function
  end interface
  real :: C  ! { dg-error "already has basic type of" }

  real :: D
  interface
    function D ()
      real :: D  ! { dg-error "already has basic type of" }
    end function
  end interface

  interface
    function E () result (s)
      real ::s
    end function
  end interface
  real :: E  ! { dg-error "already has basic type of" }

  real :: F
  interface
    function F () result (s)
      real ::s  ! { dg-error "already has basic type of" }
    end function F
  end interface

end

