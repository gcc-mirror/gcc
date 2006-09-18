! ' dg-do compile }
! This tests the fix for PR28526, in which a public interface named
! 'end' would be treated as a variable because the matcher tried
! 'END INTERFACE' as an assignment and left the symbol modified in
! failing. The various pitfalls that were encountered in developing
! the fix are checked here.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module blahblah
  public function, end

! The original PR from Yusuke IGUCHI <iguchi@coral.t.u-tokyo.ac.jp>
  interface end
    module procedure foo1
  end interface

! A contribution to the PR from Tobias Schlueter  <tobi@gcc.gnu.org>
  interface function
     module procedure foo2 ! { dg-error "is neither function nor" }
  end interface

  interface function
     module procedure foo3
  end interface

  interface
    function foo4 ()
      real foo4
      x = 1.0          ! { dg-error "in INTERFACE" }
    end function foo4
  end interface

  interface
    x = 2.0            ! { dg-error "in INTERFACE block" }
    function foo5 ()
      real foo5
    end function foo5
  end interface

  x = 3.0              ! { dg-error "in MODULE" }

contains

  subroutine foo1
  end subroutine foo1

  function foo2        ! { dg-error "Expected formal argument list" }
    foo2 = 0           ! { dg-error "already been host associated" }
  end function foo2    ! { dg-error "Expecting END MODULE" }

  function foo3 ()
    real foo3
  end function foo3

  x = 4.0              ! { dg-error "in CONTAINS section" }
end module blahblah
