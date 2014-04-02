#include "harness.h"

static unsigned char svuc[16] __attribute__ ((aligned (16)));
static signed char svsc[16] __attribute__ ((aligned (16)));
static unsigned short svus[8] __attribute__ ((aligned (16)));
static signed short svss[8] __attribute__ ((aligned (16)));
static unsigned int svui[4] __attribute__ ((aligned (16)));
static signed int svsi[4] __attribute__ ((aligned (16)));
static float svf[4] __attribute__ ((aligned (16)));

static void init ()
{
  unsigned int i;
  for (i = 0; i < 16; ++i)
    {
      svuc[i] = i;
      svsc[i] = i - 8;
    }
  for (i = 0; i < 8; ++i)
    {
      svus[i] = i;
      svss[i] = i - 4;
    }
  for (i = 0; i < 4; ++i)
    {
      svui[i] = i;
      svsi[i] = i - 2;
      svf[i] = i * 1.0f;
    }
}

static void test ()
{
  vector unsigned char vuc;
  vector signed char vsc;
  vector unsigned short vus;
  vector signed short vss;
  vector unsigned int vui;
  vector signed int vsi;
  vector float vf;

  init ();

  vuc = vec_lde (9*1, (unsigned char *)svuc);
  vsc = vec_lde (14*1, (signed char *)svsc);
  vus = vec_lde (7*2, (unsigned short *)svus);
  vss = vec_lde (1*2, (signed short *)svss);
  vui = vec_lde (3*4, (unsigned int *)svui);
  vsi = vec_lde (2*4, (signed int *)svsi);
  vf  = vec_lde (0*4, (float *)svf);

  check (vec_extract (vuc, 9) == 9, "vuc");
  check (vec_extract (vsc, 14) == 6, "vsc");
  check (vec_extract (vus, 7) == 7, "vus");
  check (vec_extract (vss, 1) == -3, "vss");
  check (vec_extract (vui, 3) == 3, "vui");
  check (vec_extract (vsi, 2) == 0, "vsi");
  check (vec_extract (vf,  0) == 0.0, "vf");
}
