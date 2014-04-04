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

static void init ()
{
  unsigned int i;
  for (i = 0; i < 16; ++i)
    {
      svuc[i] = i;
      svsc[i] = i - 8;
      svbc[i] = (i % 2) ? 0xff : 0;
    }
  for (i = 0; i < 8; ++i)
    {
      svus[i] = i;
      svss[i] = i - 4;
      svbs[i] = (i % 2) ? 0xffff : 0;
      svp[i] = i;
    }
  for (i = 0; i < 4; ++i)
    {
      svui[i] = i;
      svsi[i] = i - 2;
      svbi[i] = (i % 2) ? 0xffffffff : 0;
      svf[i] = i * 1.0f;
    }
}

static void test ()
{
  vector unsigned char evuc = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector signed char evsc = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector bool char evbc = {0,255,0,255,0,255,0,255,0,255,0,255,0,255,0,255};
  vector unsigned short evus = {0,1,2,3,4,5,6,7};
  vector signed short evss = {-4,-3,-2,-1,0,1,2,3};
  vector bool short evbs = {0,65535,0,65535,0,65535,0,65535};
  vector pixel evp = {0,1,2,3,4,5,6,7};
  vector unsigned int evui = {0,1,2,3};
  vector signed int evsi = {-2,-1,0,1};
  vector bool int evbi = {0,0xffffffff,0,0xffffffff};
  vector float evf = {0.0,1.0,2.0,3.0};

  vector unsigned char vuc;
  vector signed char vsc;
  vector bool char vbc;
  vector unsigned short vus;
  vector signed short vss;
  vector bool short vbs;
  vector pixel vp;
  vector unsigned int vui;
  vector signed int vsi;
  vector bool int vbi;
  vector float vf;

  init ();

  vuc = vec_ldl (0, (vector unsigned char *)svuc);
  vsc = vec_ldl (0, (vector signed char *)svsc);
  vbc = vec_ldl (0, (vector bool char *)svbc);
  vus = vec_ldl (0, (vector unsigned short *)svus);
  vss = vec_ldl (0, (vector signed short *)svss);
  vbs = vec_ldl (0, (vector bool short *)svbs);
  vp  = vec_ldl (0, (vector pixel *)svp);
  vui = vec_ldl (0, (vector unsigned int *)svui);
  vsi = vec_ldl (0, (vector signed int *)svsi);
  vbi = vec_ldl (0, (vector bool int *)svbi);
  vf  = vec_ldl (0, (vector float *)svf);

  check (vec_all_eq (vuc, evuc), "vuc");
  check (vec_all_eq (vsc, evsc), "vsc");
  check (vec_all_eq (vbc, evbc), "vbc");
  check (vec_all_eq (vus, evus), "vus");
  check (vec_all_eq (vss, evss), "vss");
  check (vec_all_eq (vbs, evbs), "vbs");
  check (vec_all_eq (vp,  evp ), "vp" );
  check (vec_all_eq (vui, evui), "vui");
  check (vec_all_eq (vsi, evsi), "vsi");
  check (vec_all_eq (vbi, evbi), "vbi");
  check (vec_all_eq (vf,  evf ), "vf" );
}
