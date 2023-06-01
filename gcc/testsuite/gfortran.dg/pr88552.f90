! { dg-do compile }
! PR fortran/88552
! Contributed by G.Steinmetz

integer(len((c)) :: n   ! { dg-error "must be CHARACTER" }
end
