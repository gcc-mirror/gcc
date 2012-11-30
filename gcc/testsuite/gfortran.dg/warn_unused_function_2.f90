! { dg-do compile }
! { dg-options "-Wall" }
!
! [4.8 Regression] PR 54997: -Wunused-function gives false warnings
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

  implicit none
  private :: s1,s2,s3

contains

  subroutine s1            ! { dg-warning "defined but not used" }
    call s2(s3)
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


! { dg-final { cleanup-modules "m" } }
