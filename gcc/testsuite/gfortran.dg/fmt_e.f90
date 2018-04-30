! { dg-do run }
! PR83811 fortran 'e' format broken for single digit exponents
program test
  character(25) :: s
  write(s, '(1pe5.0e1)') 1.e-4
  if (s.ne."1.E-4") STOP 1
  write(s, '(e5.1e1)') 1.e12
  if (s.ne."*****") STOP 2
end
      
