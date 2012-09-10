/* { dg-options "-mr10k-cache-barrier=store -G8" } */

/* Test that out-of-range stores to components of static objects
   are protected by a cache barrier.  */

struct { struct { char i[4]; } a; struct { char j[4]; } b; } s;

NOMIPS16 void
foo (int sel1, int sel2, int sel3)
{
  if (sel1)
    s.a.i[8] = 1;
  if (sel2)
    s.b.j[4] = 100;
  if (sel3)
    s.a.i[-1] = 0;
}

/* { dg-final { scan-assembler-times "\tcache\t" 3 } } */
