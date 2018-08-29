/* { dg-do compile } */
/* { dg-options "-O -mmovbe" } */

struct __attribute__((scalar_storage_order("big-endian"))) S
{
  int i;
};

int get (struct S *s)
{
  return s->i;
}

void set (struct S *s, int i)
{
  s->i = i;
}

/* { dg-final { scan-assembler-times "movbel\[ \t\]" 2 } } */
