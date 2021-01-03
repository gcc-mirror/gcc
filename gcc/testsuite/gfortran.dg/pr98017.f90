! { dg-do run }
! PR98017 - [8/9/10/11 Regression] Suspected regression using PACK

program p
  implicit none
  character(*), parameter :: s(1) = ['abc()']
  character(*), parameter :: t(*) = s(:)(:1)
  if (len (pack (s, s(:)(:1)  == 'a')) /= len (s)) stop 1
  if (any (pack (s, s(:)(:1)  == 'a')  /=      s)) stop 2
  if (len (pack (s,         t == 'a')) /= len (s)) stop 3
  if (any (pack (s,         t == 'a')  /=      s)) stop 4
  if (len (pack (s(:)(1:5), t == 'a')) /= len (s)) stop 5
  if (any (pack (s(:)(1:5), t == 'a')  /=      s)) stop 6
end
