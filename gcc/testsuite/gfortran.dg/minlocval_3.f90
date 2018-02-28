! { dg-do run }
  real :: a(30), b(10, 10), m
  real, allocatable :: c(:), d(:, :)
  integer :: e(30), f(10, 10), n
  integer, allocatable :: g(:), h(:,:)
  logical :: l(30), l2(10, 10)
  allocate (c (30))
  allocate (d (10, 10))
  allocate (g (30))
  allocate (h (10, 10))
  a = 7.0
  b = 7.0
  c = 7.0
  d = 7.0
  e = 7
  f = 7
  g = 7
  h = 7
  m = huge(m)
  n = huge(n)
  a(7) = 6.0
  b(5, 5) = 6.0
  b(5, 6) = 5.0
  b(6, 7) = 4.0
  c(7) = 6.0
  d(5, 5) = 6.0
  d(5, 6) = 5.0
  d(6, 7) = 4.0
  e(7) = 6
  f(5, 5) = 6
  f(5, 6) = 5
  f(6, 7) = 4
  g(7) = 6
  h(5, 5) = 6
  h(5, 6) = 5
  h(6, 7) = 4
  if (minloc (a, dim = 1).ne.7) STOP 1
  if (minval (a, dim = 1).ne.6.0) STOP 2
  if (minloc (a(::2), dim = 1).ne.4) STOP 3
  if (minval (a(::2), dim = 1).ne.6.0) STOP 4
  if (any (minloc (a).ne.(/ 7 /))) STOP 5
  if (minval (a).ne.6.0) STOP 6
  if (any (minloc (a(::2)).ne.(/ 4 /))) STOP 7
  if (minval (a(::2)).ne.6.0) STOP 8
  if (any (minloc (b, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 9
  if (any (minval (b, dim = 1).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) STOP 10
  if (any (minloc (b(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) STOP 11
  if (any (minval (b(::2,::2), dim = 1).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 12
  if (any (minloc (b, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 13
  if (any (minval (b, dim = 2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) STOP 14
  if (any (minloc (b(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) STOP 15
  if (any (minval (b(::2,::2), dim = 2).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 16
  if (any (minloc (b).ne.(/ 6, 7 /))) STOP 17
  if (minval (b).ne.4.0) STOP 18
  if (any (minloc (b(::2,::2)).ne.(/ 3, 3 /))) STOP 19
  if (minval (b(::2,::2)).ne.6.0) STOP 20
  if (minloc (c, dim = 1).ne.7) STOP 21
  if (minval (c, dim = 1).ne.6.0) STOP 22
  if (minloc (c(::2), dim = 1).ne.4) STOP 23
  if (minval (c(::2), dim = 1).ne.6.0) STOP 24
  if (any (minloc (c).ne.(/ 7 /))) STOP 25
  if (minval (c).ne.6.0) STOP 26
  if (any (minloc (c(::2)).ne.(/ 4 /))) STOP 27
  if (minval (c(::2)).ne.6.0) STOP 28
  if (any (minloc (d, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 29
  if (any (minval (d, dim = 1).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) STOP 30
  if (any (minloc (d(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) STOP 31
  if (any (minval (d(::2,::2), dim = 1).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 32
  if (any (minloc (d, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 33
  if (any (minval (d, dim = 2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) STOP 34
  if (any (minloc (d(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) STOP 35
  if (any (minval (d(::2,::2), dim = 2).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 36
  if (any (minloc (d).ne.(/ 6, 7 /))) STOP 37
  if (minval (d).ne.4.0) STOP 38
  if (any (minloc (d(::2,::2)).ne.(/ 3, 3 /))) STOP 39
  if (minval (d(::2,::2)).ne.6.0) STOP 40
  if (minloc (e, dim = 1).ne.7) STOP 41
  if (minval (e, dim = 1).ne.6) STOP 42
  if (minloc (e(::2), dim = 1).ne.4) STOP 43
  if (minval (e(::2), dim = 1).ne.6) STOP 44
  if (any (minloc (e).ne.(/ 7 /))) STOP 45
  if (minval (e).ne.6) STOP 46
  if (any (minloc (e(::2)).ne.(/ 4 /))) STOP 47
  if (minval (e(::2)).ne.6) STOP 48
  if (any (minloc (f, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 49
  if (any (minval (f, dim = 1).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) STOP 50
  if (any (minloc (f(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) STOP 51
  if (any (minval (f(::2,::2), dim = 1).ne.(/ 7, 7, 6, 7, 7 /))) STOP 52
  if (any (minloc (f, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 53
  if (any (minval (f, dim = 2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) STOP 54
  if (any (minloc (f(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) STOP 55
  if (any (minval (f(::2,::2), dim = 2).ne.(/ 7, 7, 6, 7, 7 /))) STOP 56
  if (any (minloc (f).ne.(/ 6, 7 /))) STOP 57
  if (minval (f).ne.4) STOP 58
  if (any (minloc (f(::2,::2)).ne.(/ 3, 3 /))) STOP 59
  if (minval (f(::2,::2)).ne.6) STOP 60
  if (minloc (g, dim = 1).ne.7) STOP 61
  if (minval (g, dim = 1).ne.6) STOP 62
  if (minloc (g(::2), dim = 1).ne.4) STOP 63
  if (minval (g(::2), dim = 1).ne.6) STOP 64
  if (any (minloc (g).ne.(/ 7 /))) STOP 65
  if (minval (g).ne.6) STOP 66
  if (any (minloc (g(::2)).ne.(/ 4 /))) STOP 67
  if (minval (g(::2)).ne.6) STOP 68
  if (any (minloc (h, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 69
  if (any (minval (h, dim = 1).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) STOP 70
  if (any (minloc (h(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) STOP 71
  if (any (minval (h(::2,::2), dim = 1).ne.(/ 7, 7, 6, 7, 7 /))) STOP 72
  if (any (minloc (h, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 73
  if (any (minval (h, dim = 2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) STOP 74
  if (any (minloc (h(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) STOP 75
  if (any (minval (h(::2,::2), dim = 2).ne.(/ 7, 7, 6, 7, 7 /))) STOP 76
  if (any (minloc (h).ne.(/ 6, 7 /))) STOP 77
  if (minval (h).ne.4) STOP 78
  if (any (minloc (h(::2,::2)).ne.(/ 3, 3 /))) STOP 79
  if (minval (h(::2,::2)).ne.6) STOP 80
  l = .true.
  l2 = .true.
  if (minloc (a, dim = 1, mask = l).ne.7) STOP 81
  if (minval (a, dim = 1, mask = l).ne.6.0) STOP 82
  if (minloc (a(::2), dim = 1, mask = l(::2)).ne.4) STOP 83
  if (minval (a(::2), dim = 1, mask = l(::2)).ne.6.0) STOP 84
  if (any (minloc (a, mask = l).ne.(/ 7 /))) STOP 85
  if (minval (a, mask = l).ne.6.0) STOP 86
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 4 /))) STOP 87
  if (minval (a(::2), mask = l(::2)).ne.6.0) STOP 88
  if (any (minloc (b, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 89
  if (any (minval (b, dim = 1, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) STOP 90
  if (any (minloc (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 91
  if (any (minval (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 92
  if (any (minloc (b, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 93
  if (any (minval (b, dim = 2, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) STOP 94
  if (any (minloc (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 95
  if (any (minval (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 96
  if (any (minloc (b, mask = l2).ne.(/ 6, 7 /))) STOP 97
  if (minval (b, mask = l2).ne.4.0) STOP 98
  if (any (minloc (b(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) STOP 99
  if (minval (b(::2,::2), mask = l2(::2,::2)).ne.6.0) STOP 100
  if (minloc (c, dim = 1, mask = l).ne.7) STOP 101
  if (minval (c, dim = 1, mask = l).ne.6.0) STOP 102
  if (minloc (c(::2), dim = 1, mask = l(::2)).ne.4) STOP 103
  if (minval (c(::2), dim = 1, mask = l(::2)).ne.6.0) STOP 104
  if (any (minloc (c, mask = l).ne.(/ 7 /))) STOP 105
  if (minval (c, mask = l).ne.6.0) STOP 106
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 4 /))) STOP 107
  if (minval (c(::2), mask = l(::2)).ne.6.0) STOP 108
  if (any (minloc (d, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 109
  if (any (minval (d, dim = 1, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) STOP 110
  if (any (minloc (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 111
  if (any (minval (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 112
  if (any (minloc (d, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 113
  if (any (minval (d, dim = 2, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) STOP 114
  if (any (minloc (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 115
  if (any (minval (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) STOP 116
  if (any (minloc (d, mask = l2).ne.(/ 6, 7 /))) STOP 117
  if (minval (d, mask = l2).ne.4.0) STOP 118
  if (any (minloc (d(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) STOP 119
  if (minval (d(::2,::2), mask = l2(::2,::2)).ne.6.0) STOP 120
  if (minloc (e, dim = 1, mask = l).ne.7) STOP 121
  if (minval (e, dim = 1, mask = l).ne.6) STOP 122
  if (minloc (e(::2), dim = 1, mask = l(::2)).ne.4) STOP 123
  if (minval (e(::2), dim = 1, mask = l(::2)).ne.6) STOP 124
  if (any (minloc (e, mask = l).ne.(/ 7 /))) STOP 125
  if (minval (e, mask = l).ne.6) STOP 126
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 4 /))) STOP 127
  if (minval (e(::2), mask = l(::2)).ne.6) STOP 128
  if (any (minloc (f, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 129
  if (any (minval (f, dim = 1, mask = l2).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) STOP 130
  if (any (minloc (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 131
  if (any (minval (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) STOP 132
  if (any (minloc (f, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 133
  if (any (minval (f, dim = 2, mask = l2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) STOP 134
  if (any (minloc (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 135
  if (any (minval (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) STOP 136
  if (any (minloc (f, mask = l2).ne.(/ 6, 7 /))) STOP 137
  if (minval (f, mask = l2).ne.4) STOP 138
  if (any (minloc (f(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) STOP 139
  if (minval (f(::2,::2), mask = l2(::2,::2)).ne.6) STOP 140
  if (minloc (g, dim = 1, mask = l).ne.7) STOP 141
  if (minval (g, dim = 1, mask = l).ne.6) STOP 142
  if (minloc (g(::2), dim = 1, mask = l(::2)).ne.4) STOP 143
  if (minval (g(::2), dim = 1, mask = l(::2)).ne.6) STOP 144
  if (any (minloc (g, mask = l).ne.(/ 7 /))) STOP 145
  if (minval (g, mask = l).ne.6) STOP 146
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 4 /))) STOP 147
  if (minval (g(::2), mask = l(::2)).ne.6) STOP 148
  if (any (minloc (h, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) STOP 149
  if (any (minval (h, dim = 1, mask = l2).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) STOP 150
  if (any (minloc (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 151
  if (any (minval (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) STOP 152
  if (any (minloc (h, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) STOP 153
  if (any (minval (h, dim = 2, mask = l2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) STOP 154
  if (any (minloc (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) STOP 155
  if (any (minval (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) STOP 156
  if (any (minloc (h, mask = l2).ne.(/ 6, 7 /))) STOP 157
  if (minval (h, mask = l2).ne.4) STOP 158
  if (any (minloc (h(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) STOP 159
  if (minval (h(::2,::2), mask = l2(::2,::2)).ne.6) STOP 160
  l = .false.
  l2 = .false.
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 161
  if (minval (a, dim = 1, mask = l).ne.m) STOP 162
  if (minloc (a(::2), dim = 1, mask = l(::2)).ne.0) STOP 163
  if (minval (a(::2), dim = 1, mask = l(::2)).ne.m) STOP 164
  if (any (minloc (a, mask = l).ne.(/ 0 /))) STOP 165
  if (minval (a, mask = l).ne.m) STOP 166
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 0 /))) STOP 167
  if (minval (a(::2), mask = l(::2)).ne.m) STOP 168
  if (any (minloc (b, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 169
  if (any (minval (b, dim = 1, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) STOP 170
  if (any (minloc (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 171
  if (any (minval (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) STOP 172
  if (any (minloc (b, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 173
  if (any (minval (b, dim = 2, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) STOP 174
  if (any (minloc (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 175
  if (any (minval (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) STOP 176
  if (any (minloc (b, mask = l2).ne.(/ 0, 0 /))) STOP 177
  if (minval (b, mask = l2).ne.m) STOP 178
  if (any (minloc (b(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) STOP 179
  if (minval (b(::2,::2), mask = l2(::2,::2)).ne.m) STOP 180
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 181
  if (minval (c, dim = 1, mask = l).ne.m) STOP 182
  if (minloc (c(::2), dim = 1, mask = l(::2)).ne.0) STOP 183
  if (minval (c(::2), dim = 1, mask = l(::2)).ne.m) STOP 184
  if (any (minloc (c, mask = l).ne.(/ 0 /))) STOP 185
  if (minval (c, mask = l).ne.m) STOP 186
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 0 /))) STOP 187
  if (minval (c(::2), mask = l(::2)).ne.m) STOP 188
  if (any (minloc (d, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 189
  if (any (minval (d, dim = 1, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) STOP 190
  if (any (minloc (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 191
  if (any (minval (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) STOP 192
  if (any (minloc (d, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 193
  if (any (minval (d, dim = 2, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) STOP 194
  if (any (minloc (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 195
  if (any (minval (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) STOP 196
  if (any (minloc (d, mask = l2).ne.(/ 0, 0 /))) STOP 197
  if (minval (d, mask = l2).ne.m) STOP 198
  if (any (minloc (d(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) STOP 199
  if (minval (d(::2,::2), mask = l2(::2,::2)).ne.m) STOP 200
  if (minloc (e, dim = 1, mask = l).ne.0) STOP 201
  if (minval (e, dim = 1, mask = l).ne.n) STOP 202
  if (minloc (e(::2), dim = 1, mask = l(::2)).ne.0) STOP 203
  if (minval (e(::2), dim = 1, mask = l(::2)).ne.n) STOP 204
  if (any (minloc (e, mask = l).ne.(/ 0 /))) STOP 205
  if (minval (e, mask = l).ne.n) STOP 206
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 0 /))) STOP 207
  if (minval (e(::2), mask = l(::2)).ne.n) STOP 208
  if (any (minloc (f, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 209
  if (any (minval (f, dim = 1, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) STOP 210
  if (any (minloc (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 211
  if (any (minval (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) STOP 212
  if (any (minloc (f, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 213
  if (any (minval (f, dim = 2, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) STOP 214
  if (any (minloc (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 215
  if (any (minval (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) STOP 216
  if (any (minloc (f, mask = l2).ne.(/ 0, 0 /))) STOP 217
  if (minval (f, mask = l2).ne.n) STOP 218
  if (any (minloc (f(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) STOP 219
  if (minval (f(::2,::2), mask = l2(::2,::2)).ne.n) STOP 220
  if (minloc (g, dim = 1, mask = l).ne.0) STOP 221
  if (minval (g, dim = 1, mask = l).ne.n) STOP 222
  if (minloc (g(::2), dim = 1, mask = l(::2)).ne.0) STOP 223
  if (minval (g(::2), dim = 1, mask = l(::2)).ne.n) STOP 224
  if (any (minloc (g, mask = l).ne.(/ 0 /))) STOP 225
  if (minval (g, mask = l).ne.n) STOP 226
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 0 /))) STOP 227
  if (minval (g(::2), mask = l(::2)).ne.n) STOP 228
  if (any (minloc (h, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 229
  if (any (minval (h, dim = 1, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) STOP 230
  if (any (minloc (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 231
  if (any (minval (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) STOP 232
  if (any (minloc (h, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) STOP 233
  if (any (minval (h, dim = 2, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) STOP 234
  if (any (minloc (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) STOP 235
  if (any (minval (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) STOP 236
  if (any (minloc (h, mask = l2).ne.(/ 0, 0 /))) STOP 237
  if (minval (h, mask = l2).ne.n) STOP 238
  if (any (minloc (h(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) STOP 239
  if (minval (h(::2,::2), mask = l2(::2,::2)).ne.n) STOP 240
  a = 7.0
  b = 7.0
  c = 7.0
  d = 7.0
end
