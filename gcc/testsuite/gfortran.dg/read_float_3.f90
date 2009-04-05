! { dg-do run }
! Contributed by Dominique Dhumieres <dominiq@lps.ens.fr>

character(100) :: str1 = &
  "123.00456.88 0.123E+01  +0.987+1 -0.2345+02 -0.6879E+2+0.7E+03 0.4E+03" 
character(100), parameter :: should_be = &
  "123.00456.88 0.123E+01 0.987E+01-0.2345E+02-0.6879E+02 0.7E+03 0.4E+03"
character(100) :: output
complex :: c1, c2, c3, c4

100 format ( 2F6.2, 2E10.3, 2E11.4, 2E8.1)
read (str1,100) c1, c2, c3, c4
write (output, 100) c1, c2, c3, c4

print *, output
if (output /= should_be) then
  print *, should_be
  call abort ()
end if

end
