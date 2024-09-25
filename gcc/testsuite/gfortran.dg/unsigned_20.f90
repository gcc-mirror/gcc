! { dg-do run }
! { dg-options "-funsigned" }
program memain

  unsigned(1) :: u1
  unsigned(2) :: u2
  unsigned(4) :: u4
  unsigned(8) :: u8

  u1 = 1u_1
  if (shifta (                 1u  , 1) /=                    0u_1) error stop 1
  if (shifta (                 u1  , 1) /=                    0u_1) error stop 2

  u1 = 128u_1
  if (shifta (               128u_1, 1) /=                  192u_1) error stop 3
  if (shiftl (               128u_1, 1) /=                    0u_1) error stop 4
  if (shiftr (               128u_1, 1) /=                   64u_1) error stop 5

  if (shifta (                   u1, 1) /=                  192u_1) error stop 6
  if (shiftl (                   u1, 1) /=                    0u_1) error stop 7
  if (shiftr (                   u1, 1) /=                   64u_1) error stop 8

  u2 = 32768u_2
  if (shifta (             32768u_2, 1) /=                49152u_2) error stop 9
  if (shiftl (             32768u_2, 1) /=                    0u_2) error stop 10
  if (shiftr (             32768u_2, 1) /=                16384u_2) error stop 11
  if (shifta (                   u2, 1) /=                49152u_2) error stop 12
  if (shiftl (                   u2, 1) /=                    0u_2) error stop 13
  if (shiftr (                   u2, 1) /=                16384u_2) error stop 14

  u4 = 2147483648u_4
  if (shifta (        2147483648u_4, 1) /=           3221225472u_4) error stop 15
  if (shiftl (        2147483648u_4, 1) /=                    0u_4) error stop 16
  if (shiftr (        2147483648u_4, 1) /=           1073741824u_4) error stop 17
  if (shifta (                   u4, 1) /=           3221225472u_4) error stop 18
  if (shiftl (                   u4, 1) /=                    0u_4) error stop 19
  if (shiftr (                   u4, 1) /=           1073741824u_4) error stop 20

  u8 = 9223372036854775808u_8
  if (shifta(9223372036854775808u_8, 1) /= 13835058055282163712u_8) error stop 21
  if (shiftl(9223372036854775808u_8, 1) /=                    0u_8) error stop 22
  if (shiftr(9223372036854775808u_8, 1) /=  4611686018427387904u_8) error stop 23
  if (shifta(                    u8, 1) /= 13835058055282163712u_8) error stop 24
  if (shiftl(                    u8, 1) /=                    0u_8) error stop 25
  if (shiftr(                    u8, 1) /=  4611686018427387904u_8) error stop 26
end program memain
