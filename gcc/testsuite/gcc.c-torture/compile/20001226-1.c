/* This does not assemble on m68hc11 because the function is larger
   than 64K.  */
/* { dg-do assemble { xfail m6811-*-* m6812-*-* } } */
/* { dg-xfail-if "jump beyond 128K not supported" "xtensa-*-*" "-O0" "" } */
/* { dg-skip-if "" { m32c-*-* } { "*" } { "" } } */

/* This testcase exposed two branch shortening bugs on powerpc.  */

#define C(a,b) \
  if (a > b)  goto gt; \
  if (a < b)  goto lt;

#define C4(x,b) C((x)[0], b) C((x)[1],b) C((x)[2],b) C((x)[3],b)
#define C16(x,y) C4(x, (y)[0]) C4(x, (y)[1]) C4(x, (y)[2]) C4(x, (y)[3])

#define C64(x,y) C16(x,y) C16(x+4,y) C16(x+8,y) C16(x+12,y)
#define C256(x,y) C64(x,y) C64(x,y+4) C64(x,y+8) C64(x,y+12)

#define C1024(x,y) C256(x,y) C256(x+16,y) C256(x+32,y) C256(x+48,y)
#define C4096(x,y) C1024(x,y) C1024(x,y+16) C1024(x,y+32) C1024(x,y+48)

unsigned foo(int x[64], int y[64])
{
  C4096(x,y);
  
  return 0x01234567;
 gt:
  return 0x12345678;
 lt:
  return 0xF0123456;
}

