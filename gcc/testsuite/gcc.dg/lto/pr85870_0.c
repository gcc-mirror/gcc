/* PR c/85870 */
/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -O2 } } } */
/* { dg-extra-ld-options { -r -nostdlib -flinker-output=nolto-rel } } */

typedef struct abc_s {
  char a1;
  short a2;
  unsigned int a3;
  unsigned int a4;
} abc;

typedef struct xyz_s {
 unsigned x1;
 unsigned x2;
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
