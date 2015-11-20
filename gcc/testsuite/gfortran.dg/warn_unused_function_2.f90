! { dg-do compile }
! { dg-options "-Wall" }
!
! [4.8 Regression] PR 54997: -Wunused-function gives false warnings
! PR 54224: missing warnings with -Wunused-function
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

  implicit none
  private :: s1,s2,s3

contains

  subroutine s1            ! { dg-warning "defined but not used" }
    call s2(s3)
    contains
      subroutine s4        ! { dg-warning "defined but not used" }
      end subroutine
  end subroutine

  subroutine s2(dummy)     ! { dg-warning "Unused dummy argument" }
    procedure() :: dummy
  end subroutine

  subroutine s3()
  end subroutine

end module


subroutine sub
entry en
end subroutine

program test
contains
  subroutine s5            ! { dg-warning "defined but not used" }
  end subroutine
end

! { dg-final { cleanup-modules "m" } }
