! { dg-do run }
program verify_2
  character(len=3) s1, s2
  s1 = 'abc'
  s2 = ''
  if (verify('ab', '') /= 1) STOP 1
  if (verify(s1, s2)   /= 1) STOP 2
  if (verify('abc', '', .true.) /= 3) STOP 3
  if (verify(s1, s2, .true.) /= 3) STOP 4
end program verify_2

