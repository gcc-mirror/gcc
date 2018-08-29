! { dg-do run }
! PR 22217:  Z edit descriptor with negative numbers used to give lots of *

program main
  character(len=70) line
  character(len=20) fmt
  write(unit=line,fmt='(Z4)') -1_1
  if (line(1:4) .ne. '  FF') STOP 1
  write(unit=line,fmt='(Z5)') -1_2
  if (line(1:5) .ne. ' FFFF') STOP 2
  write(unit=line,fmt='(Z9)') -1_4
  if (line(1:9) .ne. ' FFFFFFFF') STOP 3
  write(unit=line,fmt='(Z17)') -2_8
  if (line(1:17) .ne. ' FFFFFFFFFFFFFFFE') STOP 4
  write(unit=line,fmt='(Z2)') 10_8
  if (line(1:2) .ne. ' A') STOP 5

  write(unit=line,fmt='(Z8)') -43_8
  if (line(1:1) .ne. '*') STOP 6

  write(unit=line,fmt='(B65)') -1_8
  if (line(1:2) .ne. ' 1') STOP 7
  if (line(64:66) .ne. '11 ') STOP 8

  write(unit=line,fmt='(O4)') -2_1
  if (line(1:4) .ne. ' 376') STOP 9
end
