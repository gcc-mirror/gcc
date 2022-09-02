! { dg-do run }
program foo
   use ieee_arithmetic
   use iso_fortran_env
   implicit none

   ! This allows us to test REAL128 if it exists, and still compile
   ! on platforms were it is not present
   ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89639
   integer, parameter :: large = merge(real128, real64, real128 > 0)

   real, volatile :: rnan, rinf
   double precision, volatile :: dnan, dinf
   real(kind=large), volatile :: lnan, linf

   rinf = ieee_value(0., ieee_positive_inf)
   rnan = ieee_value(0., ieee_quiet_nan)

   dinf = ieee_value(0.d0, ieee_positive_inf)
   dnan = ieee_value(0.d0, ieee_quiet_nan)

   linf = ieee_value(0._large, ieee_positive_inf)
   lnan = ieee_value(0._large, ieee_quiet_nan)

   if (.not. ieee_quiet_eq (0., 0.)) stop 1
   if (.not. ieee_quiet_eq (0., -0.)) stop 2
   if (.not. ieee_quiet_eq (1., 1.)) stop 3
   if (.not. ieee_quiet_eq (rinf, rinf)) stop 4
   if (.not. ieee_quiet_eq (-rinf, -rinf)) stop 5
   if (ieee_quiet_eq (rnan, rnan)) stop 6
   if (ieee_quiet_eq (0., 1.)) stop 7
   if (ieee_quiet_eq (0., -1.)) stop 8
   if (ieee_quiet_eq (0., rnan)) stop 9
   if (ieee_quiet_eq (1., rnan)) stop 10
   if (ieee_quiet_eq (0., rinf)) stop 11
   if (ieee_quiet_eq (1., rinf)) stop 12
   if (ieee_quiet_eq (rinf, rnan)) stop 13

   if (.not. ieee_quiet_eq (0.d0, 0.d0)) stop 14
   if (.not. ieee_quiet_eq (0.d0, -0.d0)) stop 15
   if (.not. ieee_quiet_eq (1.d0, 1.d0)) stop 16
   if (.not. ieee_quiet_eq (dinf, dinf)) stop 17
   if (.not. ieee_quiet_eq (-dinf, -dinf)) stop 18
   if (ieee_quiet_eq (dnan, dnan)) stop 19
   if (ieee_quiet_eq (0.d0, 1.d0)) stop 20
   if (ieee_quiet_eq (0.d0, -1.d0)) stop 21
   if (ieee_quiet_eq (0.d0, dnan)) stop 22
   if (ieee_quiet_eq (1.d0, dnan)) stop 23
   if (ieee_quiet_eq (0.d0, dinf)) stop 24
   if (ieee_quiet_eq (1.d0, dinf)) stop 25
   if (ieee_quiet_eq (dinf, dnan)) stop 26

   if (.not. ieee_quiet_eq (0._large, 0._large)) stop 27
   if (.not. ieee_quiet_eq (0._large, -0._large)) stop 28
   if (.not. ieee_quiet_eq (1._large, 1._large)) stop 29
   if (.not. ieee_quiet_eq (linf, linf)) stop 30
   if (.not. ieee_quiet_eq (-linf, -linf)) stop 31
   if (ieee_quiet_eq (lnan, lnan)) stop 32
   if (ieee_quiet_eq (0._large, 1._large)) stop 33
   if (ieee_quiet_eq (0._large, -1._large)) stop 34
   if (ieee_quiet_eq (0._large, lnan)) stop 35
   if (ieee_quiet_eq (1._large, lnan)) stop 36
   if (ieee_quiet_eq (0._large, linf)) stop 37
   if (ieee_quiet_eq (1._large, linf)) stop 38
   if (ieee_quiet_eq (linf, lnan)) stop 39


   if (ieee_quiet_ne (0., 0.)) stop 40
   if (ieee_quiet_ne (0., -0.)) stop 41
   if (ieee_quiet_ne (1., 1.)) stop 42
   if (ieee_quiet_ne (rinf, rinf)) stop 43
   if (ieee_quiet_ne (-rinf, -rinf)) stop 44
   if (.not. ieee_quiet_ne (rnan, rnan)) stop 45
   if (.not. ieee_quiet_ne (0., 1.)) stop 46
   if (.not. ieee_quiet_ne (0., -1.)) stop 47
   if (.not. ieee_quiet_ne (0., rnan)) stop 48
   if (.not. ieee_quiet_ne (1., rnan)) stop 49
   if (.not. ieee_quiet_ne (0., rinf)) stop 50
   if (.not. ieee_quiet_ne (1., rinf)) stop 51
   if (.not. ieee_quiet_ne (rinf, rnan)) stop 52

   if (ieee_quiet_ne (0.d0, 0.d0)) stop 53
   if (ieee_quiet_ne (0.d0, -0.d0)) stop 54
   if (ieee_quiet_ne (1.d0, 1.d0)) stop 55
   if (ieee_quiet_ne (dinf, dinf)) stop 56
   if (ieee_quiet_ne (-dinf, -dinf)) stop 57
   if (.not. ieee_quiet_ne (dnan, dnan)) stop 58
   if (.not. ieee_quiet_ne (0.d0, 1.d0)) stop 59
   if (.not. ieee_quiet_ne (0.d0, -1.d0)) stop 60
   if (.not. ieee_quiet_ne (0.d0, dnan)) stop 61
   if (.not. ieee_quiet_ne (1.d0, dnan)) stop 62
   if (.not. ieee_quiet_ne (0.d0, dinf)) stop 63
   if (.not. ieee_quiet_ne (1.d0, dinf)) stop 64
   if (.not. ieee_quiet_ne (dinf, dnan)) stop 65

   if (ieee_quiet_ne (0._large, 0._large)) stop 66
   if (ieee_quiet_ne (0._large, -0._large)) stop 67
   if (ieee_quiet_ne (1._large, 1._large)) stop 68
   if (ieee_quiet_ne (linf, linf)) stop 69
   if (ieee_quiet_ne (-linf, -linf)) stop 70
   if (.not. ieee_quiet_ne (lnan, lnan)) stop 71
   if (.not. ieee_quiet_ne (0._large, 1._large)) stop 72
   if (.not. ieee_quiet_ne (0._large, -1._large)) stop 73
   if (.not. ieee_quiet_ne (0._large, lnan)) stop 74
   if (.not. ieee_quiet_ne (1._large, lnan)) stop 75
   if (.not. ieee_quiet_ne (0._large, linf)) stop 76
   if (.not. ieee_quiet_ne (1._large, linf)) stop 77
   if (.not. ieee_quiet_ne (linf, lnan)) stop 78


   if (.not. ieee_quiet_le (0., 0.)) stop 79
   if (.not. ieee_quiet_le (0., -0.)) stop 80
   if (.not. ieee_quiet_le (1., 1.)) stop 81
   if (.not. ieee_quiet_le (rinf, rinf)) stop 82
   if (.not. ieee_quiet_le (-rinf, -rinf)) stop 83
   if (ieee_quiet_le (rnan, rnan)) stop 84
   if (.not. ieee_quiet_le (0., 1.)) stop 85
   if (ieee_quiet_le (0., -1.)) stop 86
   if (ieee_quiet_le (0., rnan)) stop 87
   if (ieee_quiet_le (1., rnan)) stop 88
   if (.not. ieee_quiet_le (0., rinf)) stop 89
   if (.not. ieee_quiet_le (1., rinf)) stop 90
   if (ieee_quiet_le (rinf, rnan)) stop 91

   if (.not. ieee_quiet_le (0.d0, 0.d0)) stop 92
   if (.not. ieee_quiet_le (0.d0, -0.d0)) stop 93
   if (.not. ieee_quiet_le (1.d0, 1.d0)) stop 94
   if (.not. ieee_quiet_le (dinf, dinf)) stop 95
   if (.not. ieee_quiet_le (-dinf, -dinf)) stop 96
   if (ieee_quiet_le (dnan, dnan)) stop 97
   if (.not. ieee_quiet_le (0.d0, 1.d0)) stop 98
   if (ieee_quiet_le (0.d0, -1.d0)) stop 99
   if (ieee_quiet_le (0.d0, dnan)) stop 100
   if (ieee_quiet_le (1.d0, dnan)) stop 101
   if (.not. ieee_quiet_le (0.d0, dinf)) stop 102
   if (.not. ieee_quiet_le (1.d0, dinf)) stop 103
   if (ieee_quiet_le (dinf, dnan)) stop 104

   if (.not. ieee_quiet_le (0._large, 0._large)) stop 105
   if (.not. ieee_quiet_le (0._large, -0._large)) stop 106
   if (.not. ieee_quiet_le (1._large, 1._large)) stop 107
   if (.not. ieee_quiet_le (linf, linf)) stop 108
   if (.not. ieee_quiet_le (-linf, -linf)) stop 109
   if (ieee_quiet_le (lnan, lnan)) stop 110
   if (.not. ieee_quiet_le (0._large, 1._large)) stop 111
   if (ieee_quiet_le (0._large, -1._large)) stop 112
   if (ieee_quiet_le (0._large, lnan)) stop 113
   if (ieee_quiet_le (1._large, lnan)) stop 114
   if (.not. ieee_quiet_le (0._large, linf)) stop 115
   if (.not. ieee_quiet_le (1._large, linf)) stop 116
   if (ieee_quiet_le (linf, lnan)) stop 117


   if (.not. ieee_quiet_ge (0., 0.)) stop 118
   if (.not. ieee_quiet_ge (0., -0.)) stop 119
   if (.not. ieee_quiet_ge (1., 1.)) stop 120
   if (.not. ieee_quiet_ge (rinf, rinf)) stop 121
   if (.not. ieee_quiet_ge (-rinf, -rinf)) stop 122
   if (ieee_quiet_ge (rnan, rnan)) stop 123
   if (ieee_quiet_ge (0., 1.)) stop 124
   if (.not. ieee_quiet_ge (0., -1.)) stop 125
   if (ieee_quiet_ge (0., rnan)) stop 126
   if (ieee_quiet_ge (1., rnan)) stop 127
   if (ieee_quiet_ge (0., rinf)) stop 128
   if (ieee_quiet_ge (1., rinf)) stop 129
   if (ieee_quiet_ge (rinf, rnan)) stop 130

   if (.not. ieee_quiet_ge (0.d0, 0.d0)) stop 131
   if (.not. ieee_quiet_ge (0.d0, -0.d0)) stop 132
   if (.not. ieee_quiet_ge (1.d0, 1.d0)) stop 133
   if (.not. ieee_quiet_ge (dinf, dinf)) stop 134
   if (.not. ieee_quiet_ge (-dinf, -dinf)) stop 135
   if (ieee_quiet_ge (dnan, dnan)) stop 136
   if (ieee_quiet_ge (0.d0, 1.d0)) stop 137
   if (.not. ieee_quiet_ge (0.d0, -1.d0)) stop 138
   if (ieee_quiet_ge (0.d0, dnan)) stop 139
   if (ieee_quiet_ge (1.d0, dnan)) stop 140
   if (ieee_quiet_ge (0.d0, dinf)) stop 141
   if (ieee_quiet_ge (1.d0, dinf)) stop 142
   if (ieee_quiet_ge (dinf, dnan)) stop 143

   if (.not. ieee_quiet_ge (0._large, 0._large)) stop 144
   if (.not. ieee_quiet_ge (0._large, -0._large)) stop 145
   if (.not. ieee_quiet_ge (1._large, 1._large)) stop 146
   if (.not. ieee_quiet_ge (linf, linf)) stop 147
   if (.not. ieee_quiet_ge (-linf, -linf)) stop 148
   if (ieee_quiet_ge (lnan, lnan)) stop 149
   if (ieee_quiet_ge (0._large, 1._large)) stop 150
   if (.not. ieee_quiet_ge (0._large, -1._large)) stop 151
   if (ieee_quiet_ge (0._large, lnan)) stop 152
   if (ieee_quiet_ge (1._large, lnan)) stop 153
   if (ieee_quiet_ge (0._large, linf)) stop 154
   if (ieee_quiet_ge (1._large, linf)) stop 155
   if (ieee_quiet_ge (linf, lnan)) stop 156


   if (ieee_quiet_lt (0., 0.)) stop 157
   if (ieee_quiet_lt (0., -0.)) stop 158
   if (ieee_quiet_lt (1., 1.)) stop 159
   if (ieee_quiet_lt (rinf, rinf)) stop 160
   if (ieee_quiet_lt (-rinf, -rinf)) stop 161
   if (ieee_quiet_lt (rnan, rnan)) stop 162
   if (.not. ieee_quiet_lt (0., 1.)) stop 163
   if (ieee_quiet_lt (0., -1.)) stop 164
   if (ieee_quiet_lt (0., rnan)) stop 165
   if (ieee_quiet_lt (1., rnan)) stop 166
   if (.not. ieee_quiet_lt (0., rinf)) stop 167
   if (.not. ieee_quiet_lt (1., rinf)) stop 168
   if (ieee_quiet_lt (rinf, rnan)) stop 169

   if (ieee_quiet_lt (0.d0, 0.d0)) stop 170
   if (ieee_quiet_lt (0.d0, -0.d0)) stop 171
   if (ieee_quiet_lt (1.d0, 1.d0)) stop 172
   if (ieee_quiet_lt (dinf, dinf)) stop 173
   if (ieee_quiet_lt (-dinf, -dinf)) stop 174
   if (ieee_quiet_lt (dnan, dnan)) stop 175
   if (.not. ieee_quiet_lt (0.d0, 1.d0)) stop 176
   if (ieee_quiet_lt (0.d0, -1.d0)) stop 177
   if (ieee_quiet_lt (0.d0, dnan)) stop 178
   if (ieee_quiet_lt (1.d0, dnan)) stop 179
   if (.not. ieee_quiet_lt (0.d0, dinf)) stop 180
   if (.not. ieee_quiet_lt (1.d0, dinf)) stop 181
   if (ieee_quiet_lt (dinf, dnan)) stop 182

   if (ieee_quiet_lt (0._large, 0._large)) stop 183
   if (ieee_quiet_lt (0._large, -0._large)) stop 184
   if (ieee_quiet_lt (1._large, 1._large)) stop 185
   if (ieee_quiet_lt (linf, linf)) stop 186
   if (ieee_quiet_lt (-linf, -linf)) stop 187
   if (ieee_quiet_lt (lnan, lnan)) stop 188
   if (.not. ieee_quiet_lt (0._large, 1._large)) stop 189
   if (ieee_quiet_lt (0._large, -1._large)) stop 190
   if (ieee_quiet_lt (0._large, lnan)) stop 191
   if (ieee_quiet_lt (1._large, lnan)) stop 192
   if (.not. ieee_quiet_lt (0._large, linf)) stop 193
   if (.not. ieee_quiet_lt (1._large, linf)) stop 194
   if (ieee_quiet_lt (linf, lnan)) stop 195


   if (ieee_quiet_gt (0., 0.)) stop 196
   if (ieee_quiet_gt (0., -0.)) stop 197
   if (ieee_quiet_gt (1., 1.)) stop 198
   if (ieee_quiet_gt (rinf, rinf)) stop 199
   if (ieee_quiet_gt (-rinf, -rinf)) stop 200
   if (ieee_quiet_gt (rnan, rnan)) stop 201
   if (ieee_quiet_gt (0., 1.)) stop 202
   if (.not. ieee_quiet_gt (0., -1.)) stop 203
   if (ieee_quiet_gt (0., rnan)) stop 204
   if (ieee_quiet_gt (1., rnan)) stop 205
   if (ieee_quiet_gt (0., rinf)) stop 206
   if (ieee_quiet_gt (1., rinf)) stop 207
   if (ieee_quiet_gt (rinf, rnan)) stop 208

   if (ieee_quiet_gt (0.d0, 0.d0)) stop 209
   if (ieee_quiet_gt (0.d0, -0.d0)) stop 210
   if (ieee_quiet_gt (1.d0, 1.d0)) stop 211
   if (ieee_quiet_gt (dinf, dinf)) stop 212
   if (ieee_quiet_gt (-dinf, -dinf)) stop 213
   if (ieee_quiet_gt (dnan, dnan)) stop 214
   if (ieee_quiet_gt (0.d0, 1.d0)) stop 215
   if (.not. ieee_quiet_gt (0.d0, -1.d0)) stop 216
   if (ieee_quiet_gt (0.d0, dnan)) stop 217
   if (ieee_quiet_gt (1.d0, dnan)) stop 218
   if (ieee_quiet_gt (0.d0, dinf)) stop 219
   if (ieee_quiet_gt (1.d0, dinf)) stop 220
   if (ieee_quiet_gt (dinf, dnan)) stop 221

   if (ieee_quiet_gt (0._large, 0._large)) stop 222
   if (ieee_quiet_gt (0._large, -0._large)) stop 223
   if (ieee_quiet_gt (1._large, 1._large)) stop 224
   if (ieee_quiet_gt (linf, linf)) stop 225
   if (ieee_quiet_gt (-linf, -linf)) stop 226
   if (ieee_quiet_gt (lnan, lnan)) stop 227
   if (ieee_quiet_gt (0._large, 1._large)) stop 228
   if (.not. ieee_quiet_gt (0._large, -1._large)) stop 229
   if (ieee_quiet_gt (0._large, lnan)) stop 230
   if (ieee_quiet_gt (1._large, lnan)) stop 231
   if (ieee_quiet_gt (0._large, linf)) stop 232
   if (ieee_quiet_gt (1._large, linf)) stop 233
   if (ieee_quiet_gt (linf, lnan)) stop 234

end program foo
