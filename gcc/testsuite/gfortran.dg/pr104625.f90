! { dg-do compile }
!
! Check the fix for PR104625 in which the selectors in parentheses used
! to cause ICEs. The "Unclassifiable statement" errors were uncovered once
! the ICEs were fixed.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
  implicit none
  type t
     integer :: a
  end type
contains
  subroutine s(x)
!   class(t) :: x          ! Was OK
    class(t) :: x(:)       ! Used to ICE in combination with below
    class(t), allocatable :: r(:)

    select type (y =>  x)  ! OK
      type is (t)
        y%a = 99
    end select
    select type (z => (x))  ! Used to ICE
      type is (t)
        r = z(1)            ! Used to give "Unclassifiable statement" error
        z%a = 99            ! { dg-error "cannot be used in a variable definition" }
    end select
    select type (u => ((x))) ! Used to ICE
      type is (t)
        r = u(1)            ! Used to give "Unclassifiable statement" error
        u%a = 99            ! { dg-error "cannot be used in a variable definition" }
    end select
  end
end
