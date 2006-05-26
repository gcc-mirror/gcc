! { dg-do compile }
program main
  character*80 line
  print (line,'(A)'), 'hello' ! { dg-error "Syntax error" }
end program main
