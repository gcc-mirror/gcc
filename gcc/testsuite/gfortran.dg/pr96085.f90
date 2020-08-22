! { dg-do compile }
! { dg-options "-std=legacy" }
! PR fortran/96085 - ICE in gfc_finish_var_decl, at fortran/trans-decl.c:694

module m
  integer, parameter :: a = 1
contains
  subroutine s
    assign 2 to a   ! { dg-error "requires a scalar default INTEGER variable" }
2   print *, a
  end
end
