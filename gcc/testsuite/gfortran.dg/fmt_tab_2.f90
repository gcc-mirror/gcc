! { dg-do compile }
! { dg-options "-std=f2003" }
! PR fortran/32987
      program TestFormat
        write (*, 10) ! { dg-error "FORMAT label 10 at .1. not defined" }
 10     format ('Hello ',	'bug!') ! { dg-error "Extension: Tab character in format" }
      end
