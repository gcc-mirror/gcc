! { dg-do compile }
! PR fortran/20883
      write (*, a) b ! { dg-error "must be of type default-kind CHARACTER or of INTEGER" }
      end
