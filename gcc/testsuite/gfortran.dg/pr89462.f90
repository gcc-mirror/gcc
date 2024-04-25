! { dg-do compile }
! { dg-options "-pedantic-errors" }
! Test the fix for PR89462 in which the shared 'cl' field of the typespec
! shared between 'test', 'TR' and 'aTP' caused the compiler to go into an
! infinite loop.
! Contributed by Sergei Trofimovich  <slyich@gmail.com>
  CHARACTER*1 FUNCTION test(H) ! { dg-warning "Old-style character length" }
     CHARACTER*1 test2,TR,aTP  ! { dg-warning "Old-style character length" }
     ENTRY test2(L)
     CALL ttest3(aTP)
     test = TR
     RETURN
  END
