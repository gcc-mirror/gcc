/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mno-vsx" } */

#include "harness.h"

static unsigned char svuc[16] __attribute__ ((aligned (16)));
static signed char svsc[16] __attribute__ ((aligned (16)));
static unsigned char svbc[16] __attribute__ ((aligned (16)));
static unsigned short svus[8] __attribute__ ((aligned (16)));
static signed short svss[8] __attribute__ ((aligned (16)));
static unsigned short svbs[8] __attribute__ ((aligned (16)));
static unsigned short svp[8] __attribute__ ((aligned (16)));
static unsigned int svui[4] __attribute__ ((aligned (16)));
static signed int svsi[4] __attribute__ ((aligned (16)));
static unsigned int svbi[4] __attribute__ ((aligned (16)));
static float svf[4] __attribute__ ((aligned (16)));

static void check_arrays ()
{
  unsigned int i;
  for (i = 0; i < 16; ++i)
    {
      check (svuc[i] == i, "svuc");
      check (svsc[i] == i - 8, "svsc");
      check (svbc[i] == ((i % 2) ? 0xff : 0), "svbc");
    }
  for (i = 0; i < 8; ++i)
    {
      check (svus[i] == i, "svus");
      check (svss[i] == i - 4, "svss");
      check (svbs[i] == ((i % 2) ? 0xffff : 0), "svbs");
      check (svp[i] == i, "svp");
    }
  for (i = 0; i < 4; ++i)
    {
      check (svui[i] == i, "svui");
      check (svsi[i] == i - 2, "svsi");
      check (svbi[i] == ((i % 2) ? 0xffffffff : 0), "svbi");
      check (svf[i] == i * 1.0f, "svf");
    }
}

static void test ()
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  vector unsigned char vuc = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
  vector signed char vsc = {7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8};
  vector bool char vbc = {255,0,255,0,255,0,255,0,255,0,255,0,255,0,255,0};
  vector unsigned short vus = {7,6,5,4,3,2,1,0};
  vector signed short vss = {3,2,1,0,-1,-2,-3,-4};
  vector bool short vbs = {65535,0,65535,0,65535,0,65535,0};
  vector pixel vp = {7,6,5,4,3,2,1,0};
  vector unsigned int vui = {3,2,1,0};
  vector signed int vsi = {1,0,-1,-2};
  vector bool int vbi = {0xffffffff,0,0xffffffff,0};
  vector float vf = {3.0,2.0,1.0,0.0};
#else
  vector unsigned char vuc = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector signed char vsc = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector bool char vbc = {0,255,0,255,0,255,0,255,0,255,0,255,0,255,0,255};
  vector unsigned short vus = {0,1,2,3,4,5,6,7};
  vector signed short vss = {-4,-3,-2,-1,0,1,2,3};
  vector bool short vbs = {0,65535,0,65535,0,65535,0,65535};
  vector pixel vp = {0,1,2,3,4,5,6,7};
  vector unsigned int vui = {0,1,2,3};
  vector signed int vsi = {-2,-1,0,1};
  vector bool int vbi = {0,0xffffffff,0,0xffffffff};
  vector float vf = {0.0,1.0,2.0,3.0};
#endif

  vec_st (vuc, 0, (vector unsigned char *)svuc);
  vec_st (vsc, 0, (vector signed char *)svsc);
  vec_st (vbc, 0, (vector bool char *)svbc);
  vec_st (vus, 0, (vector unsigned short *)svus);
  vec_st (vss, 0, (vector signed short *)svss);
  vec_st (vbs, 0, (vector bool short *)svbs);
  vec_st (vp,  0, (vector pixel *)svp);
  vec_st (vui, 0, (vector unsigned int *)svui);
  vec_st (vsi, 0, (vector signed int *)svsi);
  vec_st (vbi, 0, (vector bool int *)svbi);
  vec_st (vf,  0, (vector float *)svf);

  check_arrays ();
}
