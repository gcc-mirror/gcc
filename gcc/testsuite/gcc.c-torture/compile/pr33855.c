/* Testcase by Martin Michlmayr <tbm@cyrius.com> */
/* Used to segfault due to cselim not marking the complex temp var
   as GIMPLE reg.  */

typedef struct {
  int nsant, nvqd;
  _Complex long double *vqd;
} vsorc_t;
vsorc_t vsorc;
long double cabsl(_Complex long double);

void foo(int next_job, int ain_num, int iped, long t) {
  long double zpnorm;

  while (!next_job)
    if (ain_num)
    {
      if (iped == 1)
        zpnorm = 0.0;
      int indx = vsorc.nvqd-1;
      vsorc.vqd[indx] = t*1.0fj;
      if (cabsl(vsorc.vqd[indx]) < 1.e-20)
        vsorc.vqd[indx] = 0.0fj;
      zpnorm = t;
      if (zpnorm > 0.0)
        iped = vsorc.nsant;
    }
}
