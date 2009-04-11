! Program to test the TRIM and REPEAT intrinsics.
program intrinsic_trim
  character(len=8) a
  character(len=4) b,work
  a='1234    '
  b=work(8,a)
  if (llt(b,"1234")) call abort()
  a='     '
  b=trim(a)
  if (b .gt. "") call abort()
  b='12'
  a=repeat(b,0)
  if (a .gt. "") call abort()
  a=repeat(b,2)
  if (a .ne. "12  12  ") call abort()
end

function work(i,a)
  integer i
  character(len=i) a
  character(len=4) work
  work = trim(a)
end
