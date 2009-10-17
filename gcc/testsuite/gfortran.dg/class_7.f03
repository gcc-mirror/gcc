! { dg-do compile }
! Test fixes for PR41587 and PR41608.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
! PR41587: used to accept the declaration of component 'foo'
  type t0
    integer :: j = 42
  end type t0
  type t
    integer :: i
    class(t0), allocatable :: foo(3)  ! { dg-error "deferred shape" }
  end type t

! PR41608: Would ICE on missing type decl
  class(t1), pointer :: c  ! { dg-error "before it is defined" }

  select type (c)          ! { dg-error "shall be polymorphic" }
    type is (t1)           ! { dg-error "Unexpected" }
  end select               ! { dg-error "Expecting END PROGRAM" }
end
