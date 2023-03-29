! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/107681 - ICE in gfc_type_is_extensible
! Contributed by G.Steinmetz

program p
  type t
     integer, allocatable :: a
  end type
  class(t) :: x[*]   ! { dg-error "must be dummy, allocatable or pointer" }
  associate (y => x) ! { dg-error "Invalid array reference" }
  end associate
end
