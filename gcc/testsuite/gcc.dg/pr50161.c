/* PR middle-end/50161 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-ter -funroll-loops" } */

extern void abort (void);

int
main ()
{
  unsigned i;
  unsigned long a[16];

  for (i = 0; i < 16; i++)
    a[i] = ~0UL;

  for (i = 0; i < 16; i++)
    if (__builtin_popcountl (a[i]) != sizeof (a[i]) * 8)
      abort ();

  return 0;
}
