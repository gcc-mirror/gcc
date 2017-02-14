/* { dg-do compile } */
/* { dg-options "-O3 -Wno-int-conversion" } */

/* PR77916: This failed with "error: invalid (pointer) operands to plus/minus"
   after SLSR.  */

typedef struct
{
  void *f1;
} S;

S *a;
int b;

void
fn1 (void)
{
  for (; b; b++, a++)
    a->f1 = b;
}
