! { dg-do compile }
! PR fortran/103606 -  ICE in resolve_fl_procedure
! Contributed by G.Steinmetz

program p
  type t
  end type
contains
  elemental function f() result(z) ! { dg-error "CLASS variable" }
    class(t) :: z
  end
end
