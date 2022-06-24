! { dg-do compile }
! PR fortran/105813
! Fix checking of VECTOR argument to UNPACK when MASK is a variable.
! Contributed by G.Steinmetz

program p
  logical, parameter :: mask(2,2) = reshape ([.true.,  .true.,  &
                                              .false., .true.], &
                                              shape (mask))
  print *, unpack ([1,2,3], mask, 0) ! OK
  print *, unpack ([1,2],   mask, 0) ! { dg-error "must provide at least" }
end
