! { dg-do run }
! { dg-shouldfail "Compile-time specifier checking" }
! Check keyword checking for specifiers
! PR fortran/29452
program test
  implicit none
  character(len=5) :: str
  str = 'yes'
  write(*,'(a)',advance=str) ''
  str = 'no'
  write(*,'(a)',advance=str) ''
  str = 'NOT'
  write(*,'(a)',advance=str) ''
end program test
! { dg-output "At line 13 of file.*" }
! { dg-output "Bad ADVANCE parameter in data transfer statement" }
