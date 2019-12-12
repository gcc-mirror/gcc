! { dg-do compile }
! { dg-options "-w" }
      bytea          ! { dg-error "Unclassifiable statement" }
      byte b
      byte :: d
      a = 1
      b = 1
      d = 1
      print *, a, b * d
      end
