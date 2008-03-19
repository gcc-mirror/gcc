! { dg-do compile }
! PR fortran/35152 - implicit procedure with keyword=argument

external bar

call bar(a=5)       ! { dg-error "requires explicit interface" }
call foo(a=5)       ! { dg-error "requires explicit interface" }
end

