! { dg-do run }

  integer(kind=1) :: i1
  integer(kind=2) :: i2
  integer(kind=4) :: i4
  integer(kind=8) :: i8

  i1 = -1
  i2 = -1
  i4 = -1
  i8 = -1

  if (leadz(i1) /= 0) STOP 1
  if (leadz(i2) /= 0) STOP 2
  if (leadz(i4) /= 0) STOP 3
  if (leadz(i8) /= 0) STOP 4

  if (trailz(i1) /= 0) STOP 5
  if (trailz(i2) /= 0) STOP 6
  if (trailz(i4) /= 0) STOP 7
  if (trailz(i8) /= 0) STOP 8

  if (leadz(-1_1) /= 0) STOP 9
  if (leadz(-1_2) /= 0) STOP 10
  if (leadz(-1_4) /= 0) STOP 11
  if (leadz(-1_8) /= 0) STOP 12

  if (trailz(-1_1) /= 0) STOP 13
  if (trailz(-1_2) /= 0) STOP 14
  if (trailz(-1_4) /= 0) STOP 15
  if (trailz(-1_8) /= 0) STOP 16

  i1 = -64
  i2 = -64
  i4 = -64
  i8 = -64

  if (leadz(i1) /= 0) STOP 17
  if (leadz(i2) /= 0) STOP 18
  if (leadz(i4) /= 0) STOP 19
  if (leadz(i8) /= 0) STOP 20

  if (trailz(i1) /= 6) STOP 21
  if (trailz(i2) /= 6) STOP 22
  if (trailz(i4) /= 6) STOP 23
  if (trailz(i8) /= 6) STOP 24

  if (leadz(-64_1) /= 0) STOP 25
  if (leadz(-64_2) /= 0) STOP 26
  if (leadz(-64_4) /= 0) STOP 27
  if (leadz(-64_8) /= 0) STOP 28

  if (trailz(-64_1) /= 6) STOP 29
  if (trailz(-64_2) /= 6) STOP 30
  if (trailz(-64_4) /= 6) STOP 31
  if (trailz(-64_8) /= 6) STOP 32

  i1 = -108
  i2 = -108
  i4 = -108
  i8 = -108

  if (leadz(i1) /= 0) STOP 33
  if (leadz(i2) /= 0) STOP 34
  if (leadz(i4) /= 0) STOP 35
  if (leadz(i8) /= 0) STOP 36

  if (trailz(i1) /= 2) STOP 37
  if (trailz(i2) /= 2) STOP 38
  if (trailz(i4) /= 2) STOP 39
  if (trailz(i8) /= 2) STOP 40

  if (leadz(-108_1) /= 0) STOP 41
  if (leadz(-108_2) /= 0) STOP 42
  if (leadz(-108_4) /= 0) STOP 43
  if (leadz(-108_8) /= 0) STOP 44

  if (trailz(-108_1) /= 2) STOP 45
  if (trailz(-108_2) /= 2) STOP 46
  if (trailz(-108_4) /= 2) STOP 47
  if (trailz(-108_8) /= 2) STOP 48

  i1 = 1
  i2 = 1
  i4 = 1
  i8 = 1

  if (leadz(i1) /= bit_size(i1) - 1) STOP 49
  if (leadz(i2) /= bit_size(i2) - 1) STOP 50
  if (leadz(i4) /= bit_size(i4) - 1) STOP 51
  if (leadz(i8) /= bit_size(i8) - 1) STOP 52

  if (trailz(i1) /= 0) STOP 53
  if (trailz(i2) /= 0) STOP 54
  if (trailz(i4) /= 0) STOP 55
  if (trailz(i8) /= 0) STOP 56

  if (leadz(1_1) /= bit_size(1_1) - 1) STOP 57
  if (leadz(1_2) /= bit_size(1_2) - 1) STOP 58
  if (leadz(1_4) /= bit_size(1_4) - 1) STOP 59
  if (leadz(1_8) /= bit_size(1_8) - 1) STOP 60

  if (trailz(1_1) /= 0) STOP 61
  if (trailz(1_2) /= 0) STOP 62
  if (trailz(1_4) /= 0) STOP 63
  if (trailz(1_8) /= 0) STOP 64

  i1 = 64
  i2 = 64
  i4 = 64
  i8 = 64

  if (leadz(i1) /= 1) STOP 65
  if (leadz(i2) /= 9) STOP 66
  if (leadz(i4) /= 25) STOP 67
  if (leadz(i8) /= 57) STOP 68

  if (trailz(i1) /= 6) STOP 69
  if (trailz(i2) /= 6) STOP 70
  if (trailz(i4) /= 6) STOP 71
  if (trailz(i8) /= 6) STOP 72

  if (leadz(64_1) /= 1) STOP 73
  if (leadz(64_2) /= 9) STOP 74
  if (leadz(64_4) /= 25) STOP 75
  if (leadz(64_8) /= 57) STOP 76

  if (trailz(64_1) /= 6) STOP 77
  if (trailz(64_2) /= 6) STOP 78
  if (trailz(64_4) /= 6) STOP 79
  if (trailz(64_8) /= 6) STOP 80

end
