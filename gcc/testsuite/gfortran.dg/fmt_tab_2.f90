! { dg-do compile }
! { dg-options "-std=f2003" }
! PR fortran/32987
! PR fortran/58001
      program TestFormat
        write (*, 10)
 10     format ('Hello ',	'bug!') ! { dg-warning "tab character in format" }
      end
! { dg-excess-errors "tab character in FORMAT" }
