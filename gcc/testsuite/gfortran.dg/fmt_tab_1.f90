! { dg-do compile }
! { dg-options -Wtabs }
! PR fortran/32987
! PR fortran/58001
      program TestFormat
        write (*, 10)
        ! There is a tab character before 'bug!'.  This is accepted without
        ! the -Wno-tabs option or a -std= option.
 10     format ('Hello ',	'bug!') ! { dg-warning "tab character in format" }

      end
! { dg-excess-errors "tab character in format" }
