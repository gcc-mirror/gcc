# 1 "<test>"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "<test>"
! PR fortran/92613
! { dg-do compile }
! { dg-options "-cpp -fpreprocessed" }
program test
  implicit none
  write(6,*) 'hello'
! Comment with apostrophe: it's good!
! Comment with double quote: "quoted"
end program
