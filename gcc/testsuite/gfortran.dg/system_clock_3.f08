! { dg-do run }
! PR64432
program countem
  implicit none
  integer(1)    :: count1, irate1, mymax1
  integer(2)    :: count2, irate2, mymax2
  integer(4)    :: count4, irate4, mymax4
  real(4)       :: rrate4

  call system_clock(count=count1, count_rate=irate4, count_max=mymax4)
  if (count1.ne.-127.and.irate4.ne.0.and.mymax4.ne.0) call abort
  call system_clock(count=count1, count_rate=rrate4, count_max=mymax1)
  if (count1.ne.-127.and.rrate4.ne.0.0.and.mymax4.ne.0) call abort
  call system_clock(count=count2, count_rate=irate2, count_max=mymax2)
  if (count2.ne.-32767.and.irate2.ne.0.and.mymax2.ne.0) call abort
  call system_clock(count=count2, count_rate=rrate4, count_max=mymax2)
  if (count2.ne.-32767.and.rrate4.ne.0.0.and.mymax2.ne.0) call abort
  call system_clock(count=count4, count_rate=irate4, count_max=mymax4)
  if (rrate4.ne.1000.and.mymax4.ne.huge(0_4)) call abort
  call system_clock(count=count4, count_rate=rrate4, count_max=mymax4)
  if (rrate4.ne.1000.0.and.mymax4.ne.huge(0_4)) call abort
end program countem
