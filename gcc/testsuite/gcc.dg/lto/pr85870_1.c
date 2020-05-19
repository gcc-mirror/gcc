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
static const abc Arr2[]={
                       {0,0,0xbeafdead,0xdeadbeef} ,
#line 1048594
                           {0,0,0xbeafdead,0xdeadbeef} };

void init_xyz_1() {
  XYZ[1].x1=425753;
  XYZ[1].x2=1048576;
  XYZ[1].x3=(abc *)Arr2;

}
