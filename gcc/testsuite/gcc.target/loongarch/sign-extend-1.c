/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler-times "slli.w" 1 } } */

extern int PL_savestack_ix;
extern int PL_regsize;
extern int PL_savestack_max;
void Perl_savestack_grow_cnt (int need);
extern void Perl_croak (char *);

int
S_regcppush(int parenfloor)
{
  int retval = PL_savestack_ix;
  int paren_elems_to_push = (PL_regsize - parenfloor) * 4;
  int p;

  if (paren_elems_to_push < 0)
    Perl_croak ("panic: paren_elems_to_push < 0");

  if (PL_savestack_ix + (paren_elems_to_push + 6) > PL_savestack_max)
    Perl_savestack_grow_cnt (paren_elems_to_push + 6);

  return retval;
}
