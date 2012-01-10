/* Testing for detecting duplicate ivars. */
/* { dg-do compile } */

/* This check wants to force the compiler to use a hashtable.  To do
   so, we need lots of instance variable.  */

@interface A 
{
  /* That's 200 instance variables, which is enough to trigger the
     hashtable check in the compiler.  */
  char a0; char a1; char a2; char a3; char a4; char a5; char a6; char a7; char a8; char a9;
  char b0; char b1; char b2; char b3; char b4; char b5; char b6; char b7; char b8; char b9;
  char c0; char c1; char c2; char c3; char c4; char c5; char c6; char c7; char c8; char c9;
  char d0; char d1; char d2; char d3; char d4; char d5; char d6; char d7; char d8; char d9;
  char e0; char e1; char e2; char e3; char e4; char e5; char e6; char e7; char e8; char e9;
  char f0; char f1; char f2; char f3; char f4; char f5; char f6; char f7; char f8; char f9;
  char g0; char g1; char g2; char g3; char g4; char g5; char g6; char g7; char g8; char g9;
  char h0; char h1; char h2; char h3; char h4; char h5; char h6; char h7; char h8; char h9;
  char i0; char i1; char i2; char i3; char i4; char i5; char i6; char i7; char i8; char i9;
  char j0; char j1; char j2; char j3; char j4; char j5; char j6; char j7; char j8; char j9;
  char k0; char k1; char k2; char k3; char k4; char k5; char k6; char k7; char k8; char k9;
  char l0; char l1; char l2; char l3; char l4; char l5; char l6; char l7; char l8; char l9;
  char m0; char m1; char m2; char m3; char m4; char m5; char m6; char m7; char m8; char m9;
  char n0; char n1; char n2; char n3; char n4; char n5; char n6; char n7; char n8; char n9;
  char o0; char o1; char o2; char o3; char o4; char o5; char o6; char o7; char o8; char o9;
  char p0; char p1; char p2; char p3; char p4; char p5; char p6; char p7; char p8; char p9;
  char q0; char q1; char q2; char q3; char q4; char q5; char q6; char q7; char q8; char q9;
  char r0; char r1; char r2; char r3; char r4; char r5; char r6; char r7; char r8; char r9;
  char s0; char s1; char s2; char s3; char s4; char s5; char s6; char s7; char s8; char s9;

  char x; /* { dg-message "previous declaration" } */
  char x;

  char z; /* { dg-message "previous declaration" } */
  char k; /* { dg-message "previous declaration" } */
}  /* { dg-error "redeclaration" } */
@end

@interface B : A
{
  /* That's another 200 instance variables, which should be enough to
     trigger the hashtable check in the compiler.  */
  char Ba0; char Ba1; char Ba2; char Ba3; char Ba4; char Ba5; char Ba6; char Ba7; char Ba8; char Ba9;
  char Bb0; char Bb1; char Bb2; char Bb3; char Bb4; char Bb5; char Bb6; char Bb7; char Bb8; char Bb9;
  char Bc0; char Bc1; char Bc2; char Bc3; char Bc4; char Bc5; char Bc6; char Bc7; char Bc8; char Bc9;
  char Bd0; char Bd1; char Bd2; char Bd3; char Bd4; char Bd5; char Bd6; char Bd7; char Bd8; char Bd9;
  char Be0; char Be1; char Be2; char Be3; char Be4; char Be5; char Be6; char Be7; char Be8; char Be9;
  char Bf0; char Bf1; char Bf2; char Bf3; char Bf4; char Bf5; char Bf6; char Bf7; char Bf8; char Bf9;
  char Bg0; char Bg1; char Bg2; char Bg3; char Bg4; char Bg5; char Bg6; char Bg7; char Bg8; char Bg9;
  char Bh0; char Bh1; char Bh2; char Bh3; char Bh4; char Bh5; char Bh6; char Bh7; char Bh8; char Bh9;
  char Bi0; char Bi1; char Bi2; char Bi3; char Bi4; char Bi5; char Bi6; char Bi7; char Bi8; char Bi9;
  char Bj0; char Bj1; char Bj2; char Bj3; char Bj4; char Bj5; char Bj6; char Bj7; char Bj8; char Bj9;
  char Bk0; char Bk1; char Bk2; char Bk3; char Bk4; char Bk5; char Bk6; char Bk7; char Bk8; char Bk9;
  char Bl0; char Bl1; char Bl2; char Bl3; char Bl4; char Bl5; char Bl6; char Bl7; char Bl8; char Bl9;
  char Bm0; char Bm1; char Bm2; char Bm3; char Bm4; char Bm5; char Bm6; char Bm7; char Bm8; char Bm9;
  char Bn0; char Bn1; char Bn2; char Bn3; char Bn4; char Bn5; char Bn6; char Bn7; char Bn8; char Bn9;
  char Bo0; char Bo1; char Bo2; char Bo3; char Bo4; char Bo5; char Bo6; char Bo7; char Bo8; char Bo9;
  char Bp0; char Bp1; char Bp2; char Bp3; char Bp4; char Bp5; char Bp6; char Bp7; char Bp8; char Bp9;
  char Bq0; char Bq1; char Bq2; char Bq3; char Bq4; char Bq5; char Bq6; char Bq7; char Bq8; char Bq9;
  char Br0; char Br1; char Br2; char Br3; char Br4; char Br5; char Br6; char Br7; char Br8; char Br9;
  char Bs0; char Bs1; char Bs2; char Bs3; char Bs4; char Bs5; char Bs6; char Bs7; char Bs8; char Bs9;

  char y; /* { dg-message "previous declaration" } */
  char y;

  char z; /* { dg-error "duplicate instance variable" } */
} /* { dg-error "redeclaration" } */
@end

@interface C : A
{
  char w; /* { dg-message "previous declaration" } */
}
@end

@interface D : C
{
  /* That's another 200 instance variables, which should be enough to
     trigger the hashtable check in the compiler.  */
  char Da0; char Da1; char Da2; char Da3; char Da4; char Da5; char Da6; char Da7; char Da8; char Da9;
  char Db0; char Db1; char Db2; char Db3; char Db4; char Db5; char Db6; char Db7; char Db8; char Db9;
  char Dc0; char Dc1; char Dc2; char Dc3; char Dc4; char Dc5; char Dc6; char Dc7; char Dc8; char Dc9;
  char Dd0; char Dd1; char Dd2; char Dd3; char Dd4; char Dd5; char Dd6; char Dd7; char Dd8; char Dd9;
  char De0; char De1; char De2; char De3; char De4; char De5; char De6; char De7; char De8; char De9;
  char Df0; char Df1; char Df2; char Df3; char Df4; char Df5; char Df6; char Df7; char Df8; char Df9;
  char Dg0; char Dg1; char Dg2; char Dg3; char Dg4; char Dg5; char Dg6; char Dg7; char Dg8; char Dg9;
  char Dh0; char Dh1; char Dh2; char Dh3; char Dh4; char Dh5; char Dh6; char Dh7; char Dh8; char Dh9;
  char Di0; char Di1; char Di2; char Di3; char Di4; char Di5; char Di6; char Di7; char Di8; char Di9;
  char Dj0; char Dj1; char Dj2; char Dj3; char Dj4; char Dj5; char Dj6; char Dj7; char Dj8; char Dj9;
  char Dk0; char Dk1; char Dk2; char Dk3; char Dk4; char Dk5; char Dk6; char Dk7; char Dk8; char Dk9;
  char Dl0; char Dl1; char Dl2; char Dl3; char Dl4; char Dl5; char Dl6; char Dl7; char Dl8; char Dl9;
  char Dm0; char Dm1; char Dm2; char Dm3; char Dm4; char Dm5; char Dm6; char Dm7; char Dm8; char Dm9;
  char Dn0; char Dn1; char Dn2; char Dn3; char Dn4; char Dn5; char Dn6; char Dn7; char Dn8; char Dn9;
  char Do0; char Do1; char Do2; char Do3; char Do4; char Do5; char Do6; char Do7; char Do8; char Do9;
  char Dp0; char Dp1; char Dp2; char Dp3; char Dp4; char Dp5; char Dp6; char Dp7; char Dp8; char Dp9;
  char Dq0; char Dq1; char Dq2; char Dq3; char Dq4; char Dq5; char Dq6; char Dq7; char Dq8; char Dq9;
  char Dr0; char Dr1; char Dr2; char Dr3; char Dr4; char Dr5; char Dr6; char Dr7; char Dr8; char Dr9;
  char Ds0; char Ds1; char Ds2; char Ds3; char Ds4; char Ds5; char Ds6; char Ds7; char Ds8; char Ds9;

  char y; /* { dg-message "previous declaration" } */
  char y;

  char w; /* { dg-error "duplicate instance variable" } */
  char k; /* { dg-error "duplicate instance variable" } */
}  /* { dg-error "redeclaration" } */
@end

/* Finally, make sure that anonymous instance variables don't trigger
   warnings.  This is the same as the anon-1.m testcase, but forcing
   the hashtable check.  */
@interface E : D
{
  char : 1;
  char : 2;
}
@end

@interface F : E
{
  /* That's another 200 instance variables, which should be enough to
     trigger the hashtable check in the compiler.  */
  char Fa0; char Fa1; char Fa2; char Fa3; char Fa4; char Fa5; char Fa6; char Fa7; char Fa8; char Fa9;
  char Fb0; char Fb1; char Fb2; char Fb3; char Fb4; char Fb5; char Fb6; char Fb7; char Fb8; char Fb9;
  char Fc0; char Fc1; char Fc2; char Fc3; char Fc4; char Fc5; char Fc6; char Fc7; char Fc8; char Fc9;
  char Fd0; char Fd1; char Fd2; char Fd3; char Fd4; char Fd5; char Fd6; char Fd7; char Fd8; char Fd9;
  char Fe0; char Fe1; char Fe2; char Fe3; char Fe4; char Fe5; char Fe6; char Fe7; char Fe8; char Fe9;
  char Ff0; char Ff1; char Ff2; char Ff3; char Ff4; char Ff5; char Ff6; char Ff7; char Ff8; char Ff9;
  char Fg0; char Fg1; char Fg2; char Fg3; char Fg4; char Fg5; char Fg6; char Fg7; char Fg8; char Fg9;
  char Fh0; char Fh1; char Fh2; char Fh3; char Fh4; char Fh5; char Fh6; char Fh7; char Fh8; char Fh9;
  char Fi0; char Fi1; char Fi2; char Fi3; char Fi4; char Fi5; char Fi6; char Fi7; char Fi8; char Fi9;
  char Fj0; char Fj1; char Fj2; char Fj3; char Fj4; char Fj5; char Fj6; char Fj7; char Fj8; char Fj9;
  char Fk0; char Fk1; char Fk2; char Fk3; char Fk4; char Fk5; char Fk6; char Fk7; char Fk8; char Fk9;
  char Fl0; char Fl1; char Fl2; char Fl3; char Fl4; char Fl5; char Fl6; char Fl7; char Fl8; char Fl9;
  char Fm0; char Fm1; char Fm2; char Fm3; char Fm4; char Fm5; char Fm6; char Fm7; char Fm8; char Fm9;
  char Fn0; char Fn1; char Fn2; char Fn3; char Fn4; char Fn5; char Fn6; char Fn7; char Fn8; char Fn9;
  char Fo0; char Fo1; char Fo2; char Fo3; char Fo4; char Fo5; char Fo6; char Fo7; char Fo8; char Fo9;
  char Fp0; char Fp1; char Fp2; char Fp3; char Fp4; char Fp5; char Fp6; char Fp7; char Fp8; char Fp9;
  char Fq0; char Fq1; char Fq2; char Fq3; char Fq4; char Fq5; char Fq6; char Fq7; char Fq8; char Fq9;
  char Fr0; char Fr1; char Fr2; char Fr3; char Fr4; char Fr5; char Fr6; char Fr7; char Fr8; char Fr9;
  char Fs0; char Fs1; char Fs2; char Fs3; char Fs4; char Fs5; char Fs6; char Fs7; char Fs8; char Fs9;  

  char : 1;
  char : 2;
}
@end
