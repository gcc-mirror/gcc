! { dg-do compile }
! PR fortran/107272 - followup of PR/107217 for non-numeric types

program p
  print *, 2 <= [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, 2 <  [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, 2 == [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, 2 /= [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, 2 >= [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, 2 >  [real :: (['1'])] ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] >= 2 ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] >  2 ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] == 2 ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] /= 2 ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] <= 2 ! { dg-error "Cannot convert" }
  print *, [real :: (['1'])] <  2 ! { dg-error "Cannot convert" }
  print *, [logical :: (['1'])] .and.  .true. ! { dg-error "Cannot convert" }
  print *, [logical :: (['1'])] .or.   .true. ! { dg-error "Cannot convert" }
  print *, [logical :: (['1'])] .eqv.  .true. ! { dg-error "Cannot convert" }
  print *, [logical :: (['1'])] .neqv. .true. ! { dg-error "Cannot convert" }
end
