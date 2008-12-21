! { dg-do run }
! PR36420 Fortran 2008: g0 edit descriptor 
! Test case provided by Jerry DeLisle <jvdelisle@gcc.gnu.org>
    character(25) :: string = "(g0,g0,g0)" 
    character(33) :: buffer
    write(buffer, '(g0,g0,g0)') ':',12340,':'
    if (buffer.ne.":12340:") call abort
    write(buffer, string) ':',0,':'
    if (buffer.ne.":0:") call abort
    write(buffer, string) ':',1.0/3.0,':'
    if (buffer.ne.":.33333334:") call abort
    write(buffer, '(1x,a,g0,a)') ':',1.0/3.0,':'
    if (buffer.ne." :.33333334:") call abort
    write(buffer, string) ':',"hello",':'
    if (buffer.ne.":hello:") call abort
    write(buffer, "(g0,g0,g0,g0)") ':',.true.,.false.,':'
    if (buffer.ne.":TF:") call abort
    write(buffer, "(g0,g0,',',g0,g0)") '(',( 1.2345, 2.4567 ),')'
    if (buffer.ne."(1.2345001,2.4567001)") call abort
end
