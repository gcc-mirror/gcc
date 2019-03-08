typedef struct abc_s {
  char a1;
  short a2;
  unsigned int a3;
  unsigned int a4;
} abc;


typedef struct xyz_s {
 unsigned int x1;
 unsigned int x2;
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
