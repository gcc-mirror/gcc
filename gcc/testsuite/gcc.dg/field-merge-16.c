/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that tests for sign-extension bits are handled correctly.  */

struct s {
  short a;
  short b;
  unsigned short c;
  unsigned short d;
} __attribute__ ((aligned (8)));

struct s p = { -1,  0, 0, 0 };
struct s q = {  0, -1, 0, 0 };
struct s r = {  1,  1, 0, 0 };

const long long mask = 1ll << (sizeof (long long) * __CHAR_BIT__ - 5);

int fp ()
{
  if ((p.a & mask)
      || (p.c & mask)
      || p.d
      || (p.b & mask))
    return 1;
  else
    return -1;
}

int fq ()
{
  if ((q.a & mask)
      || (q.c & mask)
      || q.d
      || (q.b & mask))
    return 1;
  else
    return -1;
}

int fr ()
{
  if ((r.a & mask)
      || (r.c & mask)
      || r.d
      || (r.b & mask))
    return 1;
  else
    return -1;
}

int main () {
  /* Unlikely, but play safe.  */
  if (sizeof (long long) == sizeof (short))
    return 0;
  if (fp () < 0
      || fq () < 0
      || fr () > 0)
    __builtin_abort ();
  return 0;
}

/* We test .b after other fields instead of right after .a to give field
   merging a chance, otherwise the masked compares with zero are combined by
   other ifcombine logic.  The .c test is discarded by earlier optimizers.  */
/* { dg-final { scan-tree-dump-times "optimizing" 6 "ifcombine" } } */
