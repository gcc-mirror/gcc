! { dg-do run }
!
! Tests fix for PR100120/100816/100818/100819/100821
! 

program main_p

  implicit none

  integer, parameter :: k = 4
  integer, parameter :: n = 11
  integer, parameter :: m = 7
  integer, parameter :: l = 3
  integer, parameter :: u = 5
  integer, parameter :: e = u-l+1
  integer, parameter :: c = int(z"FF00")

  character(kind=k),         target :: c1(n)
  character(len=m, kind=k),  target :: cm(n)
  !
  character(kind=k),        pointer :: s1
  character(len=m, kind=k), pointer :: sm
  character(len=e, kind=k), pointer :: se
  character(len=:, kind=k), pointer :: sd
  !
  character(kind=k),        pointer :: p1(:)
  character(len=m, kind=k), pointer :: pm(:)
  character(len=e, kind=k), pointer :: pe(:)
  character(len=:, kind=k), pointer :: pd(:)
  
  class(*),                 pointer :: su
  class(*),                 pointer :: pu(:)
  
  integer :: i, j

  nullify(s1, sm, se, sd, su)
  nullify(p1, pm, pe, pd, pu)
  c1 = [(char(i+c, kind=k), i=1,n)]
  do i = 1, n
    do j = 1, m
      cm(i)(j:j) = char(i*m+j+c-m, kind=k)
    end do
  end do
  
  s1 => c1(n)
  if(.not.associated(s1))              stop 1
  if(.not.associated(s1, c1(n)))       stop 2
  if(len(s1)/=1)                       stop 3
  if(s1/=c1(n))                        stop 4
  call schar_c1(s1)
  call schar_a1(s1)
  p1 => c1
  if(.not.associated(p1))              stop 5
  if(.not.associated(p1, c1))          stop 6
  if(len(p1)/=1)                       stop 7
  if(any(p1/=c1))                      stop 8
  call achar_c1(p1)
  call achar_a1(p1)
  !
  sm => cm(n)
  if(.not.associated(sm))              stop 9
  if(.not.associated(sm, cm(n)))       stop 10
  if(len(sm)/=m)                       stop 11
  if(sm/=cm(n))                        stop 12
  call schar_cm(sm)
  call schar_am(sm)
  pm => cm
  if(.not.associated(pm))              stop 13
  if(.not.associated(pm, cm))          stop 14
  if(len(pm)/=m)                       stop 15
  if(any(pm/=cm))                      stop 16
  call achar_cm(pm)
  call achar_am(pm)
  !
  se => cm(n)(l:u)
  if(.not.associated(se))              stop 17
  if(.not.associated(se, cm(n)(l:u)))  stop 18
  if(len(se)/=e)                       stop 19
  if(se/=cm(n)(l:u))                   stop 20
  call schar_ce(se)
  call schar_ae(se)
  pe => cm(:)(l:u)
  if(.not.associated(pe))              stop 21
  if(.not.associated(pe, cm(:)(l:u)))  stop 22
  if(len(pe)/=e)                       stop 23
  if(any(pe/=cm(:)(l:u)))              stop 24
  call achar_ce(pe)
  call achar_ae(pe)
  !
  sd => c1(n)
  if(.not.associated(sd))              stop 25
  if(.not.associated(sd, c1(n)))       stop 26
  if(len(sd)/=1)                       stop 27
  if(sd/=c1(n))                        stop 28
  call schar_d1(sd)
  pd => c1
  if(.not.associated(pd))              stop 29
  if(.not.associated(pd, c1))          stop 30
  if(len(pd)/=1)                       stop 31
  if(any(pd/=c1))                      stop 32
  call achar_d1(pd)
  !
  sd => cm(n)
  if(.not.associated(sd))              stop 33
  if(.not.associated(sd, cm(n)))       stop 34
  if(len(sd)/=m)                       stop 35
  if(sd/=cm(n))                        stop 36
  call schar_dm(sd)
  pd => cm
  if(.not.associated(pd))              stop 37
  if(.not.associated(pd, cm))          stop 38
  if(len(pd)/=m)                       stop 39
  if(any(pd/=cm))                      stop 40
  call achar_dm(pd)
  !
  sd => cm(n)(l:u)
  if(.not.associated(sd))              stop 41
  if(.not.associated(sd, cm(n)(l:u)))  stop 42
  if(len(sd)/=e)                       stop 43
  if(sd/=cm(n)(l:u))                   stop 44
  call schar_de(sd)
  pd => cm(:)(l:u)
  if(.not.associated(pd))              stop 45
  if(.not.associated(pd, cm(:)(l:u)))  stop 46
  if(len(pd)/=e)                       stop 47
  if(any(pd/=cm(:)(l:u)))              stop 48
  call achar_de(pd)
  !
  sd => c1(n)
  s1 => sd
  if(.not.associated(s1))              stop 49
  if(.not.associated(s1, c1(n)))       stop 50
  if(len(s1)/=1)                       stop 51
  if(s1/=c1(n))                        stop 52
  call schar_c1(s1)
  call schar_a1(s1)
  pd => c1
  s1 => pd(n)
  if(.not.associated(s1))              stop 53
  if(.not.associated(s1, c1(n)))       stop 54
  if(len(s1)/=1)                       stop 55
  if(s1/=c1(n))                        stop 56
  call schar_c1(s1)
  call schar_a1(s1)
  pd => c1
  p1 => pd
  if(.not.associated(p1))              stop 57
  if(.not.associated(p1, c1))          stop 58
  if(len(p1)/=1)                       stop 59
  if(any(p1/=c1))                      stop 60
  call achar_c1(p1)
  call achar_a1(p1)
  !
  sd => cm(n)
  sm => sd
  if(.not.associated(sm))              stop 61
  if(.not.associated(sm, cm(n)))       stop 62
  if(len(sm)/=m)                       stop 63
  if(sm/=cm(n))                        stop 64
  call schar_cm(sm)
  call schar_am(sm)
  pd => cm
  sm => pd(n)
  if(.not.associated(sm))              stop 65
  if(.not.associated(sm, cm(n)))       stop 66
  if(len(sm)/=m)                       stop 67
  if(sm/=cm(n))                        stop 68
  call schar_cm(sm)
  call schar_am(sm)
  pd => cm
  pm => pd
  if(.not.associated(pm))              stop 69
  if(.not.associated(pm, cm))          stop 70
  if(len(pm)/=m)                       stop 71
  if(any(pm/=cm))                      stop 72
  call achar_cm(pm)
  call achar_am(pm)
  !
  sd => cm(n)(l:u)
  se => sd
  if(.not.associated(se))              stop 73
  if(.not.associated(se, cm(n)(l:u)))  stop 74
  if(len(se)/=e)                       stop 75
  if(se/=cm(n)(l:u))                   stop 76
  call schar_ce(se)
  call schar_ae(se)
  pd => cm(:)(l:u)
  pe => pd
  if(.not.associated(pe))              stop 77
  if(.not.associated(pe, cm(:)(l:u)))  stop 78
  if(len(pe)/=e)                       stop 79
  if(any(pe/=cm(:)(l:u)))              stop 80
  call achar_ce(pe)
  call achar_ae(pe)
  !
  su => c1(n)
  if(.not.associated(su))              stop 81
  if(.not.associated(su, c1(n)))       stop 82
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=1)                     stop 83
    if(su/=c1(n))                      stop 84
  class default
    stop 85
  end select
  call schar_u1(su)
  pu => c1
  if(.not.associated(pu))              stop 86
  if(.not.associated(pu, c1))          stop 87
  select type(pu)
  type is(character(len=*, kind=k))
    if(len(pu)/=1)                     stop 88
    if(any(pu/=c1))                    stop 89
  class default
    stop 90
  end select
  call achar_u1(pu)
  !
  su => cm(n)
  if(.not.associated(su))              stop 91
  if(.not.associated(su))              stop 92
  if(.not.associated(su, cm(n)))       stop 93
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=m)                     stop 94
    if(su/=cm(n))                      stop 95
  class default
    stop 96
  end select
  call schar_um(su)
  pu => cm
  if(.not.associated(pu))              stop 97
  if(.not.associated(pu, cm))          stop 98
  select type(pu)
  type is(character(len=*, kind=k))
    if(len(pu)/=m)                     stop 99
    if(any(pu/=cm))                    stop 100
  class default
    stop 101
  end select
  call achar_um(pu)
  !
  su => cm(n)(l:u)
  if(.not.associated(su))              stop 102
  if(.not.associated(su, cm(n)(l:u)))  stop 103
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=e)                     stop 104
    if(su/=cm(n)(l:u))                 stop 105
  class default
    stop 106
  end select
  call schar_ue(su)
  pu => cm(:)(l:u)
  if(.not.associated(pu))              stop 107
  if(.not.associated(pu, cm(:)(l:u)))  stop 108
  select type(pu)
  type is(character(len=*, kind=k))
    if(len(pu)/=e)                     stop 109
    if(any(pu/=cm(:)(l:u)))            stop 110
  class default
    stop 111
  end select
  call achar_ue(pu)
  !
  sd => c1(n)
  su => sd
  if(.not.associated(su))              stop 112
  if(.not.associated(su, c1(n)))       stop 113
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=1)                     stop 114
    if(su/=c1(n))                      stop 115
  class default
    stop 116
  end select
  call schar_u1(su)
  pd => c1
  su => pd(n)
  if(.not.associated(su))              stop 117
  if(.not.associated(su, c1(n)))       stop 118
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=1)                     stop 119
    if(su/=c1(n))                      stop 120
  class default
    stop 121
  end select
  call schar_u1(su)
  pd => c1
  pu => pd
  if(.not.associated(pu))              stop 122
  if(.not.associated(pu, c1))          stop 123
  select type(pu)
  type is(character(len=*, kind=k))
    if(len(pu)/=1)                     stop 124
    if(any(pu/=c1))                    stop 125
  class default
    stop 126
  end select
  call achar_u1(pu)
  !
  sd => cm(n)
  su => sd
  if(.not.associated(su))              stop 127
  if(.not.associated(su, cm(n)))       stop 128
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=m)                     stop 129
    if(su/=cm(n))                      stop 130
  class default
    stop 131
  end select
  call schar_um(su)
  pd => cm
  su => pd(n)
  if(.not.associated(su))              stop 132
  if(.not.associated(su, cm(n)))       stop 133
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=m)                     stop 134
    if(su/=cm(n))                      stop 135
  class default
    stop 136
  end select
  call schar_um(su)
  pd => cm
  pu => pd
  if(.not.associated(pu))              stop 137
  if(.not.associated(pu, cm))          stop 138
  select type(pu)
  type is(character(len=*, kind=k))
    if(len(pu)/=m)                     stop 139
    if(any(pu/=cm))                    stop 140
  class default
    stop 141
  end select
  call achar_um(pu)
  !
  sd => cm(n)(l:u)
  su => sd
  if(.not.associated(su))              stop 142
  if(.not.associated(su, cm(n)(l:u)))  stop 143
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=e)                     stop 144
    if(su/=cm(n)(l:u))                 stop 145
  class default
    stop 146
  end select
  call schar_ue(su)
  pd => cm(:)(l:u)
  su => pd(n)
  if(.not.associated(su))              stop 147
  if(.not.associated(su, cm(n)(l:u)))  stop 148
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=e)                     stop 149
    if(su/=cm(n)(l:u))                 stop 150
  class default
    stop 151
  end select
  call schar_ue(su)
  pd => cm(:)(l:u)
  pu => pd
  if(.not.associated(pu))              stop 152
  if(.not.associated(pu, cm(:)(l:u)))  stop 153
  select type(pu)
  type is(character(len=*, kind=k))
    if(len(pu)/=e)                     stop 154
    if(any(pu/=cm(:)(l:u)))            stop 155
  class default
    stop 156
  end select
  call achar_ue(pu)
  !
  sd => cm(n)
  su => sd(l:u)
  if(.not.associated(su))              stop 157
  if(.not.associated(su, cm(n)(l:u)))  stop 158
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=e)                     stop 159
    if(su/=cm(n)(l:u))                 stop 160
  class default
    stop 161
  end select
  call schar_ue(su)
  pd => cm(:)
  su => pd(n)(l:u)
  if(.not.associated(su))              stop 162
  if(.not.associated(su, cm(n)(l:u)))  stop 163
  select type(su)
  type is(character(len=*, kind=k))
    if(len(su)/=e)                     stop 164
    if(su/=cm(n)(l:u))                 stop 165
  class default
    stop 166
  end select
  call schar_ue(su)
  pd => cm
  pu => pd(:)(l:u)
  if(.not.associated(pu))              stop 167
  if(.not.associated(pu, cm(:)(l:u)))  stop 168
  select type(pu)
  type is(character(len=*, kind=k))
    if(len(pu)/=e)                     stop 169
    if(any(pu/=cm(:)(l:u)))            stop 170
  class default
    stop 171
  end select
  call achar_ue(pu)
  !
  stop

contains

  subroutine schar_c1(a)
    character(kind=k), pointer, intent(in) :: a

    if(.not.associated(a))             stop 172
    if(.not.associated(a, c1(n)))      stop 173
    if(len(a)/=1)                      stop 174
    if(a/=c1(n))                       stop 175
    return
  end subroutine schar_c1

  subroutine achar_c1(a)
    character(kind=k), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 176
    if(.not.associated(a, c1))         stop 177
    if(len(a)/=1)                      stop 178
    if(any(a/=c1))                     stop 179
    return
  end subroutine achar_c1

  subroutine schar_cm(a)
    character(kind=k, len=m), pointer, intent(in) :: a

    if(.not.associated(a))             stop 180
    if(.not.associated(a, cm(n)))      stop 181
    if(len(a)/=m)                      stop 182
    if(a/=cm(n))                       stop 183
    return
  end subroutine schar_cm

  subroutine achar_cm(a)
    character(kind=k, len=m), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 184
    if(.not.associated(a, cm))         stop 185
    if(len(a)/=m)                      stop 186
    if(any(a/=cm))                     stop 187
    return
  end subroutine achar_cm

  subroutine schar_ce(a)
    character(kind=k, len=e), pointer, intent(in) :: a

    if(.not.associated(a))             stop 188
    if(.not.associated(a, cm(n)(l:u))) stop 189
    if(len(a)/=e)                      stop 190
    if(a/=cm(n)(l:u))                  stop 191
    return
  end subroutine schar_ce

  subroutine achar_ce(a)
    character(kind=k, len=e), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 192
    if(.not.associated(a, cm(:)(l:u))) stop 193
    if(len(a)/=e)                      stop 194
    if(any(a/=cm(:)(l:u)))             stop 195
    return
  end subroutine achar_ce

  subroutine schar_a1(a)
    character(kind=k, len=*), pointer, intent(in) :: a

    if(.not.associated(a))             stop 196
    if(.not.associated(a, c1(n)))      stop 197
    if(len(a)/=1)                      stop 198
    if(a/=c1(n))                       stop 199
    return
  end subroutine schar_a1

  subroutine achar_a1(a)
    character(kind=k, len=*), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 200
    if(.not.associated(a, c1))         stop 201
    if(len(a)/=1)                      stop 202
    if(any(a/=c1))                     stop 203
    return
  end subroutine achar_a1

  subroutine schar_am(a)
    character(kind=k, len=*), pointer, intent(in) :: a

    if(.not.associated(a))             stop 204
    if(.not.associated(a, cm(n)))      stop 205
    if(len(a)/=m)                      stop 206
    if(a/=cm(n))                       stop 207
    return
  end subroutine schar_am

  subroutine achar_am(a)
    character(kind=k, len=*), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 208
    if(.not.associated(a, cm))         stop 209
    if(len(a)/=m)                      stop 210
    if(any(a/=cm))                     stop 211
    return
  end subroutine achar_am

  subroutine schar_ae(a)
    character(kind=k, len=*), pointer, intent(in) :: a

    if(.not.associated(a))             stop 212
    if(.not.associated(a, cm(n)(l:u))) stop 213
    if(len(a)/=e)                      stop 214
    if(a/=cm(n)(l:u))                  stop 215
    return
  end subroutine schar_ae

  subroutine achar_ae(a)
    character(kind=k, len=*), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 216
    if(.not.associated(a, cm(:)(l:u))) stop 217
    if(len(a)/=e)                      stop 218
    if(any(a/=cm(:)(l:u)))             stop 219
    return
  end subroutine achar_ae

  subroutine schar_d1(a)
    character(kind=k, len=:), pointer, intent(in) :: a

    if(.not.associated(a))             stop 220
    if(.not.associated(a, c1(n)))      stop 221
    if(len(a)/=1)                      stop 222
    if(a/=c1(n))                       stop 223
    return
  end subroutine schar_d1

  subroutine achar_d1(a)
    character(kind=k, len=:), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 224
    if(.not.associated(a, c1))         stop 225
    if(len(a)/=1)                      stop 226
    if(any(a/=c1))                     stop 227
    return
  end subroutine achar_d1

  subroutine schar_dm(a)
    character(kind=k, len=:), pointer, intent(in) :: a

    if(.not.associated(a))             stop 228
    if(.not.associated(a, cm(n)))      stop 229
    if(len(a)/=m)                      stop 230
    if(a/=cm(n))                       stop 231
    return
  end subroutine schar_dm

  subroutine achar_dm(a)
    character(kind=k, len=:), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 232
    if(.not.associated(a, cm))         stop 233
    if(len(a)/=m)                      stop 234
    if(any(a/=cm))                     stop 235
    return
  end subroutine achar_dm

  subroutine schar_de(a)
    character(kind=k, len=:), pointer, intent(in) :: a

    if(.not.associated(a))             stop 236
    if(.not.associated(a, cm(n)(l:u))) stop 237
    if(len(a)/=e)                      stop 238
    if(a/=cm(n)(l:u))                  stop 239
    return
  end subroutine schar_de

  subroutine achar_de(a)
    character(kind=k, len=:), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 240
    if(.not.associated(a, cm(:)(l:u))) stop 241
    if(len(a)/=e)                      stop 242
    if(any(a/=cm(:)(l:u)))             stop 243
    return
  end subroutine achar_de

  subroutine schar_u1(a)
    class(*), pointer, intent(in) :: a

    if(.not.associated(a))             stop 244
    if(.not.associated(a, c1(n)))      stop 245
    select type(a)
    type is(character(len=*, kind=k))
      if(len(a)/=1)                    stop 246
      if(a/=c1(n))                     stop 247
    class default
      stop 248
    end select
    return
  end subroutine schar_u1

  subroutine achar_u1(a)
    class(*), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 249
    if(.not.associated(a, c1))         stop 250
    select type(a)
    type is(character(len=*, kind=k))
      if(len(a)/=1)                    stop 251
      if(any(a/=c1))                   stop 252
    class default
      stop 253
    end select
    return
  end subroutine achar_u1

  subroutine schar_um(a)
    class(*), pointer, intent(in) :: a

    if(.not.associated(a))             stop 254
    if(.not.associated(a))             stop 255
    if(.not.associated(a, cm(n)))      stop 256
    select type(a)
    type is(character(len=*, kind=k))
      if(len(a)/=m)                    stop 257
      if(a/=cm(n))                     stop 258
    class default
      stop 259
    end select
    return
  end subroutine schar_um

  subroutine achar_um(a)
    class(*), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 260
    if(.not.associated(a, cm))         stop 261
    select type(a)
    type is(character(len=*, kind=k))
      if(len(a)/=m)                    stop 262
      if(any(a/=cm))                   stop 263
    class default
      stop 264
    end select
    return
  end subroutine achar_um

  subroutine schar_ue(a)
    class(*), pointer, intent(in) :: a

    if(.not.associated(a))             stop 265
    if(.not.associated(a, cm(n)(l:u))) stop 266
    select type(a)
    type is(character(len=*, kind=k))
      if(len(a)/=e)                    stop 267
      if(a/=cm(n)(l:u))                stop 268
    class default
      stop 269
    end select
    return
  end subroutine schar_ue

  subroutine achar_ue(a)
    class(*), pointer, intent(in) :: a(:)

    if(.not.associated(a))             stop 270
    if(.not.associated(a, cm(:)(l:u))) stop 271
    select type(a)
    type is(character(len=*, kind=k))
      if(len(a)/=e)                    stop 272
      if(any(a/=cm(:)(l:u)))           stop 273
    class default
      stop 274
    end select
    return
  end subroutine achar_ue

end program main_p
