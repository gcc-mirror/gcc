! { dg-do compile }
! { dg-options "-std=f95" }! PR36420 Fortran 2008: g0 edit descriptor 
! Test case provided by Jerry DeLisle <jvdelisle@gcc.gnu.org>
    character(25) :: string = "(g0,g0,g0)" 
    character(33) :: buffer
    write(buffer, '(g0,g0,g0)') ':',12340,':' ! { dg-error "Fortran 2008:" }
end
