! { dg-do compile }
! PR 66310
! Make sure there is a limit to how large arrays we try to handle at
! compile time.
program p
  character, parameter :: z = 'z'
  print *, repeat(z, huge(1_4)) ! { dg-warning "Evaluation of string" }
end program p
