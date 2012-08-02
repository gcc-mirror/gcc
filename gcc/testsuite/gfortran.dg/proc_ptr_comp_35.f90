! { dg-do compile }
!
! PR 54147: [F03] Interface checks for PPCs & deferred TBPs
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  interface gen
    procedure gen
  end interface

  type t1
    procedure(gen),pointer,nopass  :: p1
    procedure(gen2),pointer,nopass :: p2  ! { dg-error "may not be generic" }
  end type

  type t2
    procedure(sf),pointer,nopass   :: p3  ! { dg-error "may not be a statement function" }
  end type

  type t3
    procedure(char),pointer,nopass :: p4  ! { dg-error "Intrinsic procedure" }
  end type

  interface gen2
    procedure gen
  end interface

  sf(x) = x**2  ! { dg-warning "Obsolescent feature" }

contains

  subroutine gen
  end subroutine

end
