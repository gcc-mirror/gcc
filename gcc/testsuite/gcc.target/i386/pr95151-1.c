/* { dg-do compile } */
/* { dg-options "-O2 -minline-all-stringops" } */

struct foo
{
  char array[257];
};

extern struct foo x;

int
func (struct foo i)
{
  return __builtin_memcmp (&x, &i, sizeof (x)) ? 1 : 2;
}

/* { dg-final { scan-assembler-not "call\[\\t \]*_?memcmp" } } */
