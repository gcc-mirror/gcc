/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that tests for sign-extension bits are handled correctly.  */

struct s {
  signed char a;
  signed char b;
  unsigned char c;
  unsigned char d;
} __attribute__ ((aligned (4)));

struct s p = { -1,  0, 0, 0 };
struct s q = {  0, -1, 0, 0 };
struct s r = {  1,  1, 0, 0 };

const long mask = 1l << (sizeof (long) * __CHAR_BIT__ - 5);

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
  if (fp () < 0
      || fq () < 0
      || fr () > 0)
    __builtin_abort ();
  return 0;
}

/* We test .b after other fields instead of right after .a to give field
   merging a chance, otherwise the masked compares with zero are combined by
   other ifcombine logic.  The .c test is discarded by earlier optimizers.  */
/* { dg-final { scan-tree-dump-times "optimizing" 6 "ifcombine" { target { ! { avr-*-* pru-*-* } } } } } */
