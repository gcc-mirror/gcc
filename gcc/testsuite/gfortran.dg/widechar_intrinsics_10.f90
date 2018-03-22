! { dg-do run }
! { dg-options "-fbackslash" }

  implicit none
  character(kind=1,len=3) :: s1(3)
  character(kind=4,len=3) :: s4(3)

  s1 = [ "abc", "def", "ghi" ]
  s4 = s1
  s4 = [ "abc", "def", "ghi" ]

  if (any (cshift (s1, 0) /= s1)) STOP 1
  if (any (cshift (s4, 0) /= s4)) STOP 2
  if (any (cshift (s1, 3) /= s1)) STOP 3
  if (any (cshift (s4, 3) /= s4)) STOP 4
  if (any (cshift (s1, 6) /= s1)) STOP 5
  if (any (cshift (s4, 6) /= s4)) STOP 6
  if (any (cshift (s1, -3) /= s1)) STOP 7
  if (any (cshift (s4, -3) /= s4)) STOP 8
  if (any (cshift (s1, -6) /= s1)) STOP 9
  if (any (cshift (s4, -6) /= s4)) STOP 10

  if (any (cshift (s1, 1) /= [ s1(2:3), s1(1) ])) STOP 11
  if (any (cshift (s1, -1) /= [ s1(3), s1(1:2) ])) STOP 12
  if (any (cshift (s1, 4) /= [ s1(2:3), s1(1) ])) STOP 13
  if (any (cshift (s1, -4) /= [ s1(3), s1(1:2) ])) STOP 14

  if (any (cshift (s4, 1) /= [ s4(2:3), s4(1) ])) STOP 15
  if (any (cshift (s4, -1) /= [ s4(3), s4(1:2) ])) STOP 16
  if (any (cshift (s4, 4) /= [ s4(2:3), s4(1) ])) STOP 17
  if (any (cshift (s4, -4) /= [ s4(3), s4(1:2) ])) STOP 18

  if (any (cshift (s1, 2) /= [ s1(3), s1(1:2) ])) STOP 19
  if (any (cshift (s1, -2) /= [ s1(2:3), s1(1) ])) STOP 20
  if (any (cshift (s1, 5) /= [ s1(3), s1(1:2) ])) STOP 21
  if (any (cshift (s1, -5) /= [ s1(2:3), s1(1) ])) STOP 22

  if (any (cshift (s4, 2) /= [ s4(3), s4(1:2) ])) STOP 23
  if (any (cshift (s4, -2) /= [ s4(2:3), s4(1) ])) STOP 24
  if (any (cshift (s4, 5) /= [ s4(3), s4(1:2) ])) STOP 25
  if (any (cshift (s4, -5) /= [ s4(2:3), s4(1) ])) STOP 26


  if (any (eoshift (s1, 0) /= s1)) STOP 27
  if (any (eoshift (s4, 0) /= s4)) STOP 28
  if (any (eoshift (s1, 3) /= "")) STOP 29
  if (any (eoshift (s4, 3) /= 4_"")) STOP 30
  if (any (eoshift (s1, 3, "   ") /= "")) STOP 31
  if (any (eoshift (s4, 3, 4_"   ") /= 4_"")) STOP 32
  if (any (eoshift (s1, 3, " x ") /= " x")) STOP 33
  if (any (eoshift (s4, 3, 4_" x ") /= 4_" x")) STOP 34
  if (any (eoshift (s1, -3) /= "")) STOP 35
  if (any (eoshift (s4, -3) /= 4_"")) STOP 36
  if (any (eoshift (s1, -3, "   ") /= "")) STOP 37
  if (any (eoshift (s4, -3, 4_"   ") /= 4_"")) STOP 38
  if (any (eoshift (s1, -3, " x ") /= " x")) STOP 39
  if (any (eoshift (s4, -3, 4_" x ") /= 4_" x")) STOP 40
  if (any (eoshift (s1, 4) /= "")) STOP 41
  if (any (eoshift (s4, 4) /= 4_"")) STOP 42
  if (any (eoshift (s1, 4, "   ") /= "")) STOP 43
  if (any (eoshift (s4, 4, 4_"   ") /= 4_"")) STOP 44
  if (any (eoshift (s1, 4, " x ") /= " x")) STOP 45
  if (any (eoshift (s4, 4, 4_" x ") /= 4_" x")) STOP 46
  if (any (eoshift (s1, -4) /= "")) STOP 47
  if (any (eoshift (s4, -4) /= 4_"")) STOP 48
  if (any (eoshift (s1, -4, "   ") /= "")) STOP 49
  if (any (eoshift (s4, -4, 4_"   ") /= 4_"")) STOP 50
  if (any (eoshift (s1, -4, " x ") /= " x")) STOP 51
  if (any (eoshift (s4, -4, 4_" x ") /= 4_" x")) STOP 52

  if (any (eoshift (s1, 1) /= [ s1(2:3), "   " ])) STOP 53
  if (any (eoshift (s1, -1) /= [ "   ", s1(1:2) ])) STOP 54
  if (any (eoshift (s1, 1, " x ") /= [ s1(2:3), " x " ])) STOP 55
  if (any (eoshift (s1, -1, " x ") /= [ " x ", s1(1:2) ])) STOP 56
  if (any (eoshift (s4, 1) /= [ s4(2:3), 4_"   " ])) STOP 57
  if (any (eoshift (s4, -1) /= [ 4_"   ", s4(1:2) ])) STOP 58
  if (any (eoshift (s4, 1, 4_" x ") /= [ s4(2:3), 4_" x " ])) STOP 59
  if (any (eoshift (s4, -1, 4_" x ") /= [ 4_" x ", s4(1:2) ])) STOP 60

  if (any (eoshift (s1, 2) /= [ s1(3), "   ", "   " ])) STOP 61
  if (any (eoshift (s1, -2) /= [ "   ", "   ", s1(1) ])) STOP 62
  if (any (eoshift (s1, 2, " x ") /= [ s1(3), " x ", " x " ])) STOP 63
  if (any (eoshift (s1, -2, " x ") /= [ " x ", " x ", s1(1) ])) STOP 64
  if (any (eoshift (s4, 2) /= [ s4(3), 4_"   ", 4_"   " ])) STOP 65
  if (any (eoshift (s4, -2) /= [ 4_"   ", 4_"   ", s4(1) ])) STOP 66
  if (any (eoshift (s4, 2, 4_" x ") /= [ s4(3), 4_" x ", 4_" x " ])) STOP 67
  if (any (eoshift (s4, -2, 4_" x ") /= [ 4_" x ", 4_" x ", s4(1) ])) STOP 68

end
