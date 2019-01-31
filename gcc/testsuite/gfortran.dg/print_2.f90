! { dg-do compile }
! PR52564 Accepts invalid: Missing I/O list after comma 
program printbug
  print *, 'hello world'
! the following line should not compile:
  print *,  ! { dg-error "not allowed" }
end program
