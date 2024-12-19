/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that bitfield compares-with-zero turned into GT and LE compares with
   powers-of-two minus 1 are optimized.  */

struct s {
  short a : sizeof (short) * __CHAR_BIT__ - 3;
  short b : 3;
  short c : 3;
  short d : sizeof (short) * __CHAR_BIT__ - 3;
} __attribute__ ((aligned (4)));

struct s p = { 15, 7, 3, 1 };
struct s q = { 0, 0, 0, 0 };

void f ()
{
  if (p.a || p.b || p.c || p.d)
    return;
  __builtin_abort ();
}

void g ()
{
  if (q.a || q.b || q.c || q.d)
    __builtin_abort ();
}

int main () {
  f ();
  g ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing" 6 "ifcombine" } } */
