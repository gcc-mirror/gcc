! { dg-do compile }
! { dg-options "-std=legacy" }
!
program main
  character*80 line
  print (line,'(A)'), 'hello' ! { dg-error "Syntax error" }
end program main
