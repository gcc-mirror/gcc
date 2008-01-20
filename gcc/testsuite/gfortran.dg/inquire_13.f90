! { dg-do run }
! PR34795 inquire statement , direct= specifier incorrectly returns YES
! Test case from PR, modified by Jerry DeLisle  <jvdelisle@gcc.gnu.org
program testinquire
implicit none
character drct*7, acc*12, frmt*12, seqn*12, fname*15
logical opn

fname="inquire_13_test"
inquire(unit=6, direct=drct, opened=opn, access=acc)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL") call abort

inquire(unit=10, direct=drct, opened=opn, access=acc)
if (drct.ne."UNKNOWN" .and. opn .and. acc.ne."UNDEFINED") call abort

inquire(unit=10, direct=drct, opened=opn, access=acc, formatted=frmt)
if (drct.ne."UNKNOWN" .and. opn .and. acc.ne."UNDEFINED") call abort
if (frmt.ne."UNKNOWN") call abort

open(unit=19,file=fname,status='replace',err=170,form="formatted")
inquire(unit=19, direct=drct, opened=opn, access=acc,formatted=frmt)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL")  call abort
if (frmt.ne."YES")  call abort

! Inquire on filename, open file with DIRECT and FORMATTED
inquire(file=fname, direct=drct, opened=opn, access=acc, FORMATTED=frmt)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL")  call abort
if (frmt.ne."YES") call abort
close(19)

! Inquire on filename, closed file with DIRECT and FORMATTED
inquire(file=fname, direct=drct, opened=opn, access=acc, formatted=frmt)
if (drct.ne."UNKNOWN" .and. opn .and. acc.ne."UNDEFINED") call abort
if (frmt.ne."UNKNOWN") call abort

open(unit=19,file=fname,status='replace',err=170,form="unformatted")
inquire(unit=19, direct=drct, opened=opn, access=acc, formatted=frmt)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL") call abort
if (frmt.ne."NO")  call abort
close(19)
       
open(unit=19,file=fname,status='replace',err=170,form="formatted")

inquire(unit=19, direct=drct, opened=opn, access=acc, unformatted=frmt)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL") call abort

! Inquire on filename, open file with DIRECT and UNFORMATTED
inquire(file=fname, direct=drct, opened=opn, access=acc, UNFORMATTED=frmt)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL")  call abort
if (frmt.ne."NO") call abort
close(19)

! Inquire on filename, closed file with DIRECT and UNFORMATTED
inquire(file=fname, direct=drct, opened=opn, access=acc, unformatted=frmt)
if (drct.ne."UNKNOWN" .and. opn .and. acc.ne."UNDEFINED") call abort
if (frmt.ne."UNKNOWN") call abort

open(unit=19,file=fname,status='replace',err=170,form="unformatted")

inquire(unit=19, direct=drct, opened=opn, access=acc,unformatted=frmt)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL") call abort
if (frmt.ne."YES")  call abort
close(19)
      
open(unit=19,file=fname,status='replace',err=170)

inquire(unit=19, direct=drct, opened=opn, access=acc)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL") call abort
close(19)

open(unit=19,file=fname,status='replace',err=170,access='SEQUENTIAL')

inquire(unit=19, direct=drct, opened=opn, access=acc)
if (drct.ne."NO" .and. .not.opn .and. acc.ne."SEQUENTIAL") call abort

! Inquire on filename, open file with SEQUENTIAL
inquire(file=fname, SEQUENTIAL=seqn, opened=opn, access=acc)
if (seqn.ne."YES" .and. .not.opn .and. acc.ne."DIRECT") call abort
close(19)

! Inquire on filename, closed file with SEQUENTIAL
inquire(file=fname, SEQUENTIAL=seqn, opened=opn, access=acc)
if (seqn.ne."UNKNOWN" .and. opn .and. acc.ne."UNDEFINED") call abort

open(unit=19,file=fname,status='replace',err=170,form='UNFORMATTED',access='DIRECT',recl=72)

inquire(unit=19, direct=drct, opened=opn, access=acc)
if (drct.ne."YES" .and. .not.opn .and. acc.ne."DIRECT") call abort

! Inquire on filename, open file with DIRECT
inquire(file=fname, direct=drct, opened=opn, access=acc)
if (drct.ne."YES" .and. .not.opn .and. acc.ne."DIRECT") call abort
close(19, status="delete")

! Inquire on filename, closed file with DIRECT
inquire(file=fname, direct=drct, opened=opn, access=acc)
if (drct.ne."UNKNOWN" .and. opn .and. acc.ne."UNDEFINED") call abort
stop

170   write(*,*) "ERROR: unable to open testdirect.f"
end
