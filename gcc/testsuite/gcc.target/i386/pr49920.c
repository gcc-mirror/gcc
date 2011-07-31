/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target ia32 } */

typedef __SIZE_TYPE__ size_t;
extern void *malloc (size_t);

register unsigned int MR_mr0 asm ("esi");
register unsigned int MR_mr1 asm ("edi");

void ml_backend__ml_closure_gen_module11 (void)
{
  unsigned int MR_tempr1, MR_tempr2, MR_tempr3;

  MR_tempr1 = (unsigned int)((char *) malloc (sizeof (unsigned int)) + 4);
  MR_tempr3 = ((unsigned int *) MR_mr0)[0];

  ((unsigned int *) (MR_tempr1 - 4))[0] = MR_tempr3;

  MR_tempr2 = (unsigned int)((char *) malloc (2 * sizeof (unsigned int)));

  ((unsigned int *) MR_tempr2)[1] = MR_tempr1;
}
