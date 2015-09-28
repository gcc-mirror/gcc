! { dg-do compile }
! { dg-options -Wno-error=tabs }
! PR fortran/32987
      program TestFormat
        write (*, 10)
 10     format ('Hello ',	'bug!') ! { dg-warning "Extension: Tab character in format" }
      end
