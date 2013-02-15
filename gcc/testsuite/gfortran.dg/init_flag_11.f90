! { dg-do compile }
! { dg-flags "-finit-local-zero -fno-automatic"
!
! PR fortran/53818
!
! Contributed by John Moyard
!
logical function testing(date1, date2) result(test)
  integer  date1, date2
  test = ( (date1 < date2) .or. ( date1==date2 ))
end function testing
