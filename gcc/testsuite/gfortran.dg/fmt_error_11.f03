! { dg-do run }
! PR45143  Endless loop with unlimited edit descriptor
    print 20, "1234", "abcd", "14rfa5"
 20 format ( *('start',('ahdh',('dhdhhow',a),'ndlownd  ')))
    print 30, "1234", "abcd", "14rfa5"
 30 format ( *('start',('ahdh',('dhdhhow'),'ndlownd  ')))
end
! { dg-shouldfail "Fortran runtime error: '*' requires at least one associated data descriptor" }
