! { dg-do compile }
!
! PR 77501: [F03] ICE in gfc_match_generic, at fortran/decl.c:9429
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

module m1
  type t
  contains
    generic :: f => g  ! { dg-error "must target a specific binding" }
    generic :: g => h  ! { dg-error "Undefined specific binding" }
  end type
end

module m2
  type t
  contains
    generic :: f => g  ! { dg-error "must target a specific binding" }
    generic :: g => f  ! { dg-error "Undefined specific binding" }
  end type
end
