! { dg-do compile }
! { dg-additional-options "-w" }
! PR fortran/104572 - ICE in gfc_resolve_finalizers
! Contributed by G. Steinmetz

module m
  type t
   contains
     final :: s
  end type
contains
  subroutine s(*) ! { dg-error "Argument of FINAL procedure" }
  end
end
