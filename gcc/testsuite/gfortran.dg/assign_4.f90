! { dg-do compile }
! PR fortran/20883
      write (*, a) b ! { dg-error "must be of type CHARACTER or INTEGER" }
      end
