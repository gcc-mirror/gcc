! we didn't give a warning if the RHS of an assignment was NULL
! { dg-do compile }
INTEGER, POINTER :: P
I = NULL() ! { dg-error "NULL appears" "Assignment non-pointer = NULL" }
P = NULL() ! { dg-error "NULL appears" "Assignment pointer = NULL" }
P => NULL()
END
