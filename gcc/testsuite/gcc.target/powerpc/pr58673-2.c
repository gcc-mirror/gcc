/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3 -funroll-loops" } */

#include <stddef.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

typedef long unsigned int size_t;
typedef struct _IO_FILE FILE;
typedef float real;
typedef real rvec[3];
typedef real matrix[3][3];
typedef real tensor[3][3];
enum
{
  F_BONDS, F_G96BONDS, F_MORSE, F_CUBICBONDS, F_CONNBONDS, F_HARMONIC,
    F_ANGLES, F_G96ANGLES, F_PDIHS, F_RBDIHS, F_IDIHS, F_LJ14, F_COUL14, F_LJ,
    F_BHAM, F_LJLR, F_DISPCORR, F_SR, F_LR, F_WPOL, F_POSRES, F_DISRES,
    F_DISRESVIOL, F_ORIRES, F_ORIRESDEV, F_ANGRES, F_ANGRESZ, F_SHAKE,
    F_SHAKENC, F_SETTLE, F_DUMMY2, F_DUMMY3, F_DUMMY3FD, F_DUMMY3FAD,
    F_DUMMY3OUT, F_DUMMY4FD, F_EQM, F_EPOT, F_EKIN, F_ETOT, F_TEMP, F_PRES,
    F_DVDL, F_DVDLKIN, F_NRE
};
typedef union
{
  struct
  {
  }
  bham;
  struct
  {
    real rA, krA, rB, krB;
  }
  harmonic;
}
t_iparams;
typedef struct
{
  t_iparams *iparams;
}
t_idef;
typedef struct
{
}
t_inputrec;
typedef struct
{
}
t_commrec;
typedef struct
{
}
t_forcerec;
typedef struct
{
}
t_mdatoms;
typedef struct
{
}
t_filenm;
enum
{
  eoPres, eoEpot, eoVir, eoDist, eoMu, eoForce, eoFx, eoFy, eoFz, eoPx, eoPy,
    eoPz, eoPolarizability, eoDipole, eoObsNR, eoMemory =
    eoObsNR, eoInter, eoUseVirial, eoNR
};
extern char *eoNames[eoNR];
typedef struct
{
  int bPrint;
}
t_coupl_LJ;
typedef struct
{
  int eObs;
  t_iparams xi;
}
t_coupl_iparams;
typedef struct
{
  real act_value[eoObsNR];
  real av_value[eoObsNR];
  real ref_value[eoObsNR];
  int bObsUsed[eoObsNR];
  int nLJ, nBU, nQ, nIP;
  t_coupl_LJ *tcLJ;
}
t_coupl_rec;
void xvgr_legend ();
real calc_deviation ();
void pr_dev ();
static void
pr_ff (t_coupl_rec * tcr, real time, t_idef * idef, t_commrec * cr, int nfile,
       t_filenm fnm[])
{
  static FILE *prop;
  static FILE **out = ((void *) 0);
  static FILE **qq = ((void *) 0);
  static FILE **ip = ((void *) 0);
  char buf[256];
  char *leg[] = {
    "C12", "C6"
  };
  char **raleg;
  int i, j, index;
  if ((prop == ((void *) 0)) && (out == ((void *) 0)) && (qq == ((void *) 0))
      && (ip == ((void *) 0)))
    {
      for (i = j = 0; (i < eoObsNR); i++)
	{
	  if (tcr->bObsUsed[i])
	    {
	      raleg[j++] =
		(__extension__
		 (__builtin_constant_p (eoNames[i])
		  && ((size_t) (const void *) ((eoNames[i]) + 1) -
		      (size_t) (const void *) (eoNames[i]) ==
		      1) ? (((const char *) (eoNames[i]))[0] ==
			    '\0' ? (char *) calloc ((size_t) 1,
						    (size_t) 1) : (
									   {
									   size_t
									   __len
									   =
									   strlen
									   (eoNames
									    [i])
									   +
									   1;
									   char
									   *__retval
									   =
									   (char
									    *)
									   malloc
									   (__len);
									   __retval;}
	    )):	    __strdup (eoNames[i])));
	      raleg[j++] =
		(__extension__
		 (__builtin_constant_p (buf)
		  && ((size_t) (const void *) ((buf) + 1) -
		      (size_t) (const void *) (buf) ==
		      1) ? (((const char *) (buf))[0] ==
			    '\0' ? (char *) calloc ((size_t) 1,
						    (size_t) 1) : (
									   {
									   size_t
									   __len
									   =
									   strlen
									   (buf)
									   +
									   1;
									   char
									   *__retval
									   =
									   (char
									    *)
									   malloc
									   (__len);
									   __retval;}
	    )):	    __strdup (buf)));
	    }
	}
      if (tcr->nLJ)
	{
	  for (i = 0; (i < tcr->nLJ); i++)
	    {
	      if (tcr->tcLJ[i].bPrint)
		{
		  xvgr_legend (out[i], (sizeof (leg) / sizeof ((leg)[0])),
			       leg);
		}
	    }
	}
    }
}

void
do_coupling (FILE * log, int nfile, t_filenm fnm[], t_coupl_rec * tcr, real t,
	     int step, real ener[], t_forcerec * fr, t_inputrec * ir,
	     int bMaster, t_mdatoms * md, t_idef * idef, real mu_aver,
	     int nmols, t_commrec * cr, matrix box, tensor virial,
	     tensor pres, rvec mu_tot, rvec x[], rvec f[], int bDoIt)
{
  int i, j, ati, atj, atnr2, type, ftype;
  real deviation[eoObsNR], prdev[eoObsNR], epot0, dist, rmsf;
  real ff6, ff12, ffa, ffb, ffc, ffq, factor, dt, mu_ind;
  int bTest, bPrint;
  t_coupl_iparams *tip;
  if (bPrint)
    {
      pr_ff (tcr, t, idef, cr, nfile, fnm);
    }
  for (i = 0; (i < eoObsNR); i++)
    {
      deviation[i] =
	calc_deviation (tcr->av_value[i], tcr->act_value[i],
			tcr->ref_value[i]);
      prdev[i] = tcr->ref_value[i] - tcr->act_value[i];
    }
  if (bPrint)
    pr_dev (tcr, t, prdev, cr, nfile, fnm);
  for (i = 0; (i < atnr2); i++)
    {
      factor = dt * deviation[tip->eObs];
      switch (ftype)
	{
	case F_BONDS:
	  if (fabs (tip->xi.harmonic.krA) > 1.2e-38)
	    idef->iparams[type].harmonic.krA *=
	      (1 + factor / tip->xi.harmonic.krA);
	}
    }
}
