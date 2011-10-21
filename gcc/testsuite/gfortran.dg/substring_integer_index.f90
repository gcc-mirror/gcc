! { dg-do compile }
! PR fortran/50524
!
program foo
   print *, 'abc'(2.e0:3)   ! { dg-error "must be of type INTEGER" }
   print *,'qwe'(1:1e0)     ! { dg-error "must be of type INTEGER" }
end program foo

