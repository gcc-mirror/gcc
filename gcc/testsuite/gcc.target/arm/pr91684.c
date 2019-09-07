/* { dg-do compile }  */
/* { dg-require-effective-target arm_prefer_ldrd_strd } */
/* { dg-options "-O3" } */

typedef struct { int a, b, c; } S;

void g (S *s);
void bug1 (void)
{
  S s;
  __builtin_memset (&s, 0, sizeof (S)); 
  g (&s);
}

/* { dg-final { scan-assembler-times "strd" 1 } } */
