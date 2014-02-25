#include "harness.h"

static unsigned char svuc[16] __attribute__ ((aligned (16)));
static signed char svsc[16] __attribute__ ((aligned (16)));
static unsigned short svus[8] __attribute__ ((aligned (16)));
static signed short svss[8] __attribute__ ((aligned (16)));
static unsigned int svui[4] __attribute__ ((aligned (16)));
static signed int svsi[4] __attribute__ ((aligned (16)));
static float svf[4] __attribute__ ((aligned (16)));

static void check_arrays ()
{
  check (svuc[9] == 9, "svuc");
  check (svsc[14] == 6, "svsc");
  check (svus[7] == 7, "svus");
  check (svss[1] == -3, "svss");
  check (svui[3] == 3, "svui");
  check (svsi[2] == 0, "svsi");
  check (svf[0] == 0.0, "svf");
}

static void test ()
{
  vector unsigned char vuc = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector signed char vsc = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector unsigned short vus = {0,1,2,3,4,5,6,7};
  vector signed short vss = {-4,-3,-2,-1,0,1,2,3};
  vector unsigned int vui = {0,1,2,3};
  vector signed int vsi = {-2,-1,0,1};
  vector float vf = {0.0,1.0,2.0,3.0};

  vec_ste (vuc, 9*1, (unsigned char *)svuc);
  vec_ste (vsc, 14*1, (signed char *)svsc);
  vec_ste (vus, 7*2, (unsigned short *)svus);
  vec_ste (vss, 1*2, (signed short *)svss);
  vec_ste (vui, 3*4, (unsigned int *)svui);
  vec_ste (vsi, 2*4, (signed int *)svsi);
  vec_ste (vf,  0*4, (float *)svf);

  check_arrays ();
}
