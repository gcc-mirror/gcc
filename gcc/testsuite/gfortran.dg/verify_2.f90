! { dg-do run }
program verify_2
  character(len=3) s1, s2
  s1 = 'abc'
  s2 = ''
  if (verify('ab', '') /= 1) call abort
  if (verify(s1, s2)   /= 1) call abort
  if (verify('abc', '', .true.) /= 3) call abort
  if (verify(s1, s2, .true.) /= 3) call abort
end program verify_2

