! { dg-do compile }
! { dg-options "-O -Wall" }
! PR 94091 - this used to give a bogus warning.
! Test case by "MikeS".
program tester
  character(50) cname,fred
  fred='1234567890123456789012345678901234567890' ! 40 characters
  kk=len_trim(fred)
  cname=fred(5:kk)
  print *,kk,cname
end program tester
