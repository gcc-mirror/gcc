! { dg-do compile }
!
! PR 42418: PROCEDURE: Rejects interface which is both specific and generic procedure
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

  interface gen
    procedure gen
  end interface

  procedure(gen)  :: p1
  procedure(gen2) :: p2  ! { dg-error "may not be generic" }
  procedure(sf)   :: p3  ! { dg-error "may not be a statement function" }
  procedure(char) :: p4

  interface gen2
    procedure char
  end interface

  sf(x) = x**2  ! { dg-warning "Obsolescent feature" }

contains

  subroutine gen
  end subroutine

  subroutine char
  end subroutine

end
