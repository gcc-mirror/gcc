! { dg-do run }
! { dg-options "-std=f95 -pedantic -fall-intrinsics" }
! { dg-shouldfail "Zero width in format descriptor" }
! PR36420 Fortran 2008: g0 edit descriptor 
! Test case provided by Jerry DeLisle <jvdelisle@gcc.gnu.org>
    character(25) :: string = "(g0,g0,g0)" 
    character(33) :: buffer
    write(buffer, string) ':',0,':'
    if (buffer.ne.":0:") call abort
end
! { dg-output "Fortran runtime error: Zero width in format descriptor(\n|\r\n|\r)" }
