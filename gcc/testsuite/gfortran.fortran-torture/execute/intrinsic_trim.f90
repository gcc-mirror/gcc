! Program to test the TRIM and REPEAT intrinsics.
program intrinsic_trim
  character(len=8) a
  character(len=4) b,work
  a='1234    '
  b=work(8,a)
  if (llt(b,"1234")) STOP 1
  a='     '
  b=trim(a)
  if (b .gt. "") STOP 2
  b='12'
  a=repeat(b,0)
  if (a .gt. "") STOP 3
  a=repeat(b,2)
  if (a .ne. "12  12  ") STOP 4
end

function work(i,a)
  integer i
  character(len=i) a
  character(len=4) work
  work = trim(a)
end
