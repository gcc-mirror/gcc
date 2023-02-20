! { dg-do compile }
! PR fortran/96025 - ICE in expr_check_typed_help
! Contributed by G.Steinmetz

program p
  print *, f()
contains
  character(char(1)) function f() ! { dg-error "must be of INTEGER type" }
    f = 'f'
  end
end
