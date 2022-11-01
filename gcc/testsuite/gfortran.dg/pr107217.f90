! { dg-do compile }
! PR fortran/107217 - ICE in gfc_arith_times
! Contributed by G.Steinmetz

program p
  print *, [real :: (['1'])] * 2 ! { dg-error "Cannot convert" }
  print *, 2 * [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] + 2 ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] - 2 ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] / 2 ! { dg-error "Cannot convert" }
  print *, 1 / [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] ** 2 ! { dg-error "Cannot convert" }
  print *, 2 ** [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, 2.0 ** [real :: (.true.)] ! { dg-error "Cannot convert" }
  print *, [real :: (.true.)] ** 2.0 ! { dg-error "Cannot convert" }
  print *, [complex :: (['1'])] ** (1.0,2.0) ! { dg-error "Cannot convert" }
  print *, (1.0,2.0) ** [complex :: (['1'])] ! { dg-error "Cannot convert" }
end
