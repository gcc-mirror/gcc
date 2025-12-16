/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

extern int use (int *b);

extern int n1;
extern int n2;
extern int n3;

int foo (void)
{
  int *b1 = __builtin_alloca (n1);
  int *b2 = __builtin_alloca (n2);
  int *b3 = __builtin_alloca (n3);
  int a1 = use (b1);
  int a2 = use (b2);
  int a3 = use (b3);

  return a1 + a2 + a3;
}

/* With HWASAN_ALLOCA_POISON now calling irg of its own,  the number of
   expected irg is 3, and stg/st2g is 4 (3 for tag, 1 for untag each in their
   respective loop).  */

/* { dg-final { scan-assembler-times {\tirg\t} 3 } } */
/* { dg-final { scan-assembler-times {stg\t...?, \[...?\], 16\n} 4 } } */
/* { dg-final { scan-assembler-times {st2g\t...?, \[...?\], 32\n} 4 } } */
