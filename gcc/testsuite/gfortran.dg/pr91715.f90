! { dg-do compile }
! PR fortran/91715
! Code contributed Gerhard Steinmetz
character(1function f()  ! { dg-error "Syntax error in CHARACTER" }
end
