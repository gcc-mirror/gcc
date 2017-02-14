! { dg-do compile }
! { dg-options "-fdec-intrinsic-ints" }
!
! Test B/I/J/K integer intrinsics.
!
program main

implicit none

integer*1 :: ab = 9_1, bb = 3_1, cb
integer*2 :: ai = 9_2, bi = 3_2, ci
integer*4 :: aj = 9_4, bj = 3_4, cj
integer*8 :: ak = 9_8, bk = 3_8, ck
integer   :: a  = 9  , b  = 3  , c

integer*1 :: ib = 9_1, bpos = 3_1
integer*2 :: ii = 9_2, ipos = 3_2
integer*4 :: ij = 9_4, jpos = 3_4
integer*8 :: ik = 9_8, kpos = 3_8
integer   :: i  = 9  ,  pos = 3

integer*1 :: ba,     bc, bd
integer*2 :: ia,     ic, id
integer*4 :: ja, jb, jc, jd
integer*8 :: ka, kb, kc, kd

logical*1 :: lb
logical*2 :: li
logical*4 :: lj
logical*8 :: lk
logical   :: l

real :: r

lb = bbtest(ib, bpos)
li = bitest(ii, ipos)
lj = bjtest(ij, jpos)
lk = bktest(ik, kpos)
l  =  btest(i ,  pos)

lb = bbtest(9_1, 3_1)
li = bitest(9_2, 3_2)
lj = bjtest(9_4, 3_4)
lk = bktest(9_8, 3_8)
l  =  btest(9  , 3  )

r = floati(ai)
r = floatj(aj)
r = floatk(ak)
r = float (a )

r = floati(9_2)
r = floatj(9_4)
r = floatk(9_8)
r = float (9  )

bb =  babs(ab)
bi = iiabs(ai)
bj = jiabs(aj)
bk = kiabs(ak)
b  =  iabs(a )

bb =  babs(9_1)
bi = iiabs(9_2)
bj = jiabs(9_4)
bk = kiabs(9_8)
b  =  iabs(9  )

cb = biand(ab, bb)
ci = iiand(ai, bi)
cj = jiand(aj, bj)
ck = kiand(ak, bk)
c  =  iand(a , b )

cb = biand(9_1, 3_1)
ci = iiand(9_2, 3_2)
cj = jiand(9_4, 3_4)
ck = kiand(9_8, 3_8)
c  =  iand(9  , 3  )

cb =  bbclr(ib, bpos)
ci = iibclr(ii, ipos)
cj = jibclr(ij, jpos)
ck = kibclr(ik, kpos)
c  =  ibclr(i ,  pos)

cb =  bbclr(9_1, 3_1)
ci = iibclr(9_2, 3_2)
cj = jibclr(9_4, 3_4)
ck = kibclr(9_8, 3_8)
c  =  ibclr(i ,  pos)

cb =  bbset(ib, bpos)
ci = iibset(ii, ipos)
cj = jibset(ij, jpos)
ck = kibset(ik, kpos)
c  =  ibset(i ,  pos)

cb =  bbset(9_1, 3_1)
ci = iibset(9_2, 3_2)
cj = jibset(9_4, 3_4)
ck = kibset(9_8, 3_8)
c  =  ibset(i ,  pos)

cb = bieor(ab, bb)
ci = iieor(ai, bi)
cj = jieor(aj, bj)
ck = kieor(ak, bk)
c  =  ieor(a , b )

cb = bieor(9_1, 3_1)
ci = iieor(9_2, 3_2)
cj = jieor(9_4, 3_4)
ck = kieor(9_8, 3_8)
c  =  ieor(9  , 3  )

cb = bior(ab, bb)
ci = iior(ai, bi)
cj = jior(aj, bj)
ck = kior(ak, bk)
c  =  ior(a , b )

cb = bior(9_1, 3_1)
ci = iior(9_2, 3_2)
cj = jior(9_4, 3_4)
ck = kior(9_8, 3_8)
c  =  ior(9  , 3  )

cb = bmod(ab, bb)
ci = imod(ai, bi)
cj = jmod(aj, bj)
ck = kmod(ak, bk)
c  =  mod(a , b )

cb = bmod(9_1, 3_1)
ci = imod(9_2, 3_2)
cj = jmod(9_4, 3_4)
ck = kmod(9_8, 3_8)
c  =  mod(9  , 3  )

ba = bbits(121, 10, 5)
call bmvbits(121_1, 2, 3, ba, 1)
bc = bshftc(ba, 3, 6)
bd = bshft(bc, -3)
ba = bnot(bd)

ia = iibits(357, 10, 5)
call imvbits(357_2, 8, 3, ia, 1)
ic = iishftc(ia, 3, 6)
id = iishft(ic, -3)
ia = inot(id)

ja = jibits(357, 10, 5)
call jmvbits(357_4, 8, 3, ja, 1)
jc = jishftc(ja, 3, 6)
jd = jishft(jc, -3)
ja = jnot(jd)

ka = kibits(357_8, 10_8, 5_8)
call kmvbits(357_8, 8_8, 3_8, ka, 1_8)
kc = kishftc(ka, 3_8, 6_8)
kd = kishft(kc, -3_8)
ka = knot(kd)

end program
