! { dg-do run }
! { dg-options "-fbackslash" }
!
! PR fortran/57633
!
program teststuff
  implicit none
  integer::a
  character(len=10)::s1,s2

  open(11,file="testcase.txt",form='unformatted',access='stream',status='new')
  write(11) 'line1,1,\r\nline2'
  close(11)

  open(11,file="testcase.txt",form='formatted')
  s1 = repeat('x', len(s1))
  a = 99
  read(11,*)s1,a
  if (s1 /= "line1" .or. a /= 1) STOP 1

  s1 = repeat('x', len(s1))
  read(11,"(a)")s1
  close(11,status="delete")
  if (s1 /= "line2") STOP 2


  open(11,file="testcase.txt",form='unformatted',access='stream',status='new')
  write(11) 'word1\rword2,\n'
  close(11)

  open(11,file="testcase.txt",form='formatted')
  s1 = repeat('x', len(s1))
  s2 = repeat('x', len(s1))
  read(11,*)s1,s2
  close(11,status="delete")
  if (s1 /= "word1") STOP 3
  if (s2 /= "word2") STOP 4
end program teststuff
