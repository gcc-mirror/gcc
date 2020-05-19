/* PR c/85870 */
/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -O2 } } } */
/* { dg-extra-ld-options { -r -nostdlib -flinker-output=nolto-rel } } */

typedef unsigned uint32_t __attribute__((mode (__SI__)));

typedef struct abc_s {
  char a1;
  short a2;
  uint32_t a3;
  uint32_t a4;
} abc;

typedef struct xyz_s {
 uint32_t x1;
 uint32_t x2;
 abc *x3;
} xyz;

extern xyz XYZ[3];
static const abc Arr1[]={
                       {0,0,0xdeadbeaf,0xbeefdead} ,
#line 1040
                        {0,0,0xdeadbeaf,0xbeefdead} };

void init_xyz_0() {
  XYZ[0].x1=975753;
  XYZ[0].x2=1024;
  XYZ[0].x3=(abc *)Arr1;

}

int
main ()
{
}
