! { dg-do compile }
! { dg-additional-options "-dH" }
! PR 65996.f90. before patch compiler aborted on this program.
program foo
  implicit none
  character(len=16) :: a,b,c
  a="XXX"
  b="&
    &XXX"
  c="XXX &
    & XXX"
  write(0,*) 'a=',a,' b=',b,' c=',c 
endprogram foo
