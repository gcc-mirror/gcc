/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef struct { unsigned short a; } A;

extern void abort (void);
extern void exit (int);

void foo (unsigned int x)
{
  if (x != 0x800 && x != 0x810)
    abort ();
}

int
main (int argc, char **argv)
{
  int i;
  for (i = 0; i < 2; ++i)
    foo (((A) { ((!(i >> 4) ? 8 : 64 + (i >> 4)) << 8) + (i << 4) } ).a);
  exit (0);
}
/* { dg-final { scan-tree-dump-times "Invalid sum" 0 "optimized" } } */
