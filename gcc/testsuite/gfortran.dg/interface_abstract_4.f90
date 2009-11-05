! { dg-do compile }
!
! PR 41873: Bogus Error: ABSTRACT INTERFACE must not be referenced...
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

  implicit none

  type, abstract :: abstype
  contains
    procedure(f), nopass, deferred :: f_bound
    procedure(s), nopass, deferred :: s_bound
  end type

  abstract interface
    real function f ()
    end function
  end interface

  abstract interface
    subroutine s
    end subroutine
  end interface

contains

  subroutine cg (c)
    class(abstype) :: c
    print *, f()             ! { dg-error "must not be referenced" }
    call s                   ! { dg-error "must not be referenced" }
    print *, c%f_bound ()
    call c%s_bound ()
  end subroutine

end
