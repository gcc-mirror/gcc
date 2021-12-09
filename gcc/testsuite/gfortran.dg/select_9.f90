! { dg-do compile }
! PR fortran/103591 - ICE in gfc_compare_string
! Contributed by G.Steinmetz

program p
  integer :: n
  select case (n)
  case ('1':2.)   ! { dg-error "cannot be REAL" }
  end select
end
