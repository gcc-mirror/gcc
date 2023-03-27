! { dg-do compile }
! PR fortran/78500
class(t) function f() ! { dg-error "is not accessible" }
   f = 1              ! { dg-error "variable must not be polymorphic" }
end

