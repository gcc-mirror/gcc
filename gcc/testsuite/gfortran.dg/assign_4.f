! { dg-do compile }
! Option passed to avoid excess errors from obsolete warning
! { dg-options "-w" }
! PR17423
      program testit
c
      assign 12 to i
      write(*, i) 
 0012 format (" **** ASSIGN FORMAT NUMBER TO INTEGER VARIABLE ****" )
      end

