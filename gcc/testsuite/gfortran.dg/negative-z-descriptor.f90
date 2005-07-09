! { dg-do run }
! PR 22217:  Z edit descriptor with negative numbers used to give lots of *

program main
  character(len=70) line
  character(len=20) fmt
  write(unit=line,fmt='(Z4)') -1_1
  if (line(1:4) .ne. '  FF') call abort
  write(unit=line,fmt='(Z5)') -1_2
  if (line(1:5) .ne. ' FFFF') call abort
  write(unit=line,fmt='(Z9)') -1_4
  if (line(1:9) .ne. ' FFFFFFFF') call abort
  write(unit=line,fmt='(Z17)') -2_8
  if (line(1:17) .ne. ' FFFFFFFFFFFFFFFE') call abort
  write(unit=line,fmt='(Z2)') 10_8
  if (line(1:2) .ne. ' A') call abort

  write(unit=line,fmt='(Z8)') -43_8
  if (line(1:1) .ne. '*') call abort

  write(unit=line,fmt='(B65)') -1_8
  if (line(1:2) .ne. ' 1') call abort
  if (line(64:66) .ne. '11 ') call abort

  write(unit=line,fmt='(O4)') -2_1
  if (line(1:4) .ne. ' 376') call abort
end
