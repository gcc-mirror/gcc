/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

void __attribute__ ((ms_abi)) foo (void)
{
}

typedef struct _IAVIStreamImpl
{
  int sInfo;
  int has;
} IAVIStreamImpl;

extern int __attribute__ ((ms_abi)) aso (void *);
extern int sre (void *);

int AVIFILE_OpenCompressor (IAVIStreamImpl *This)
{
  if (This->has != 0)
    aso (&This->has);
  sre (&This->sInfo);
  return 0;
}
