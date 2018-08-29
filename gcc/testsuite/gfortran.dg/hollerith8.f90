! { dg-do run }
! { dg-options "-std=gnu" }
! PR43217 Output of Hollerith constants which are not a multiple of 4 bytes
! Test case prepared from OP by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program hello2
  call wrtout (9hHELLO YOU, 9)
  stop
end

subroutine wrtout (iarray, nchrs)
  integer iarray(1)
  integer nchrs

  integer icpw
  data icpw/4/
  integer i, nwrds
  character(len=33) outstr

  nwrds = (nchrs + icpw - 1) /icpw
  write(outstr,'(4(z8," "))') (iarray(i), i=1,nwrds)
  if (outstr.ne."4C4C4548 4F59204F 20202055" .and. &
 &    outstr.ne."48454C4C 4F20594F 55202020") STOP 1
  return
end
! { dg-warning "Hollerith constant" "" { target *-*-* } 6 }
! { dg-warning "Rank mismatch" "" { target *-*-* } 6 }
