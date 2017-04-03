! { dg-do compile }
! PR fortran/69498
! This test used to result in an internal compiler error
function f()
    interface
    external f ! { dg-error "Unexpected attribute declaration statement in INTERFACE" }
    end interface
end function
