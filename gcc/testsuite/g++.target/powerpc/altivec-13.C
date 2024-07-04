/* Check that vec_step can be used with const vector types.   This
   test is derived from parts of gcc.dg/vmx/8-02.c from Motorola's
   AltiVec testsuite.  */

/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

extern vector unsigned char vuc;
extern vector signed char vsc;
extern vector bool char vbc;
extern vector unsigned short vus;
extern vector signed short vss;
extern vector bool short vbs;
extern vector unsigned int vui;
extern vector signed int vsi;
extern vector bool int vbi;
extern vector pixel vp;
extern vector float vf;
extern const vector unsigned char cvuc;
extern const vector signed char cvsc;
extern const vector bool char cvbc;
extern const vector unsigned short cvus;
extern const vector signed short cvss;
extern const vector bool short cvbs;
extern const vector unsigned int cvui;
extern const vector signed int cvsi;
extern const vector bool int cvbi;
extern const vector pixel cvp;
extern const vector float cvf;

void
foo (void)
{
  int i_vuc = vec_step (vuc);
  int i_vsc = vec_step (vsc);
  int i_vbc = vec_step (vbc);
  int i_vus = vec_step (vus);
  int i_vss = vec_step (vss);
  int i_vbs = vec_step (vbs);
  int i_vui = vec_step (vui);
  int i_vsi = vec_step (vsi);
  int i_vbi = vec_step (vbi);
  int i_vp = vec_step (vp);
  int i_vf = vec_step (vf);
  int i_cvuc = vec_step (cvuc);
  int i_cvsc = vec_step (cvsc);
  int i_cvbc = vec_step (cvbc);
  int i_cvus = vec_step (cvus);
  int i_cvss = vec_step (cvss);
  int i_cvbs = vec_step (cvbs);
  int i_cvui = vec_step (cvui);
  int i_cvsi = vec_step (cvsi);
  int i_cvbi = vec_step (cvbi);
  int i_cvp = vec_step (cvp);
  int i_cvf = vec_step (cvf);
}
