! { dg-do compile }
! PR fortran/78500
class(t) function f() ! { dg-error "must be dummy, allocatable or pointer" }
   f = 1              ! { dg-error "variable must not be polymorphic" }
end

