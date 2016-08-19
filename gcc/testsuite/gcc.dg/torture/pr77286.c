/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

typedef float real;
typedef struct
{
  int ngtc;
  real *ref_t;
  real *tau_t;
} t_grpopts;
typedef struct
{
  real T;
  real xi;
} t_grp_tcstat;
typedef struct
{
  t_grp_tcstat *tcstat;
} t_groups;
extern real *save_calloc ();
void
nosehoover_tcoupl (t_grpopts * opts, t_groups * grps, real dt, real SAfactor)
{
  static real *Qinv = ((void *) 0);
  int i;
  real reft = 0, xit, oldxi;
  if (Qinv == ((void *) 0))
    {
      (Qinv) =
	save_calloc ("Qinv", "coupling.c", 372, (opts->ngtc),
		     sizeof (*(Qinv)));
      for (i = 0; i < opts->ngtc; i++)
	if ((opts->tau_t[i] > 0))
	  Qinv[i] = 1.0 / opts->tau_t[i];
    }
  for (i = 0; (i < opts->ngtc); i++)
    {
      reft =
	(((0.0) >
	  (opts->ref_t[i] * SAfactor)) ? (0.0) : (opts->ref_t[i] * SAfactor));
      grps->tcstat[i].xi += dt * Qinv[i] * (grps->tcstat[i].T - reft);
    }
}
