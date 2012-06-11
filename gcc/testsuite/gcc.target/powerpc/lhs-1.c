/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-options "-O2 -mcpu=power5" } */
/* { dg-final { scan-assembler-times "nop" 3 } } */

/* Test generation of nops in load hit store situation.  */

typedef union {
  double val;
  struct {
    unsigned int w1;
    unsigned int w2;
  };
} words;

unsigned int f (double d, words *u)
{
  u->val = d;
  return u->w2;
}

