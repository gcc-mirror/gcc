program prog 
  common /block/ i
  equivalence (a, b, c), (i, j, k ,l)
  a = 1.0
  b = 2.0
  c = 3.0
  i = 1
  j = 2
  k = 3
  l = 4

  if ((a .ne. 3.0) .or. (b .ne. 3.0) .or. (c .ne. 3.0)) STOP 1
  if ((i .ne. 4) .or. (j .ne. 4) .or. (k .ne. 4) .or. (l .ne. 4)) &
    STOP 2
end program
