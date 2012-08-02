! { dg-do compile }
!
! PR 54147: [F03] Interface checks for PPCs & deferred TBPs
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  interface gen
    procedure gen
  end interface

  type, abstract :: t1
  contains
    procedure(gen),deferred,nopass  :: p1
    procedure(gen2),deferred,nopass :: p2  ! { dg-error "may not be generic" }
  end type

  type, abstract :: t2
  contains
    procedure(sf),deferred,nopass   :: p3  ! { dg-error "may not be a statement function" }
  end type

  type, abstract :: t3
  contains
    procedure(char),deferred,nopass :: p4  ! { dg-error "Intrinsic procedure" }
  end type

  interface gen2
    procedure gen
  end interface

  sf(x) = x**2  ! { dg-warning "Obsolescent feature" }

contains

  subroutine gen
  end subroutine

end
