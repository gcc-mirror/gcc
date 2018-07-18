/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2" } */
extern void baz(void);
int
foo (long long int a, long long int a2, int b)
{
    if (a < 0 || b)
          baz ();
}
/* { dg-final { scan-assembler "js\[ \t\]\.L" } } */
/* { dg-final { scan-assembler "jne\[ \t\]\.L" } } */
