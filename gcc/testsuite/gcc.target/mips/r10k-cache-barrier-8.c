/* { dg-options "-mr10k-cache-barrier=store -G8" } */

/* Test that in-range stores to components of static objects
   do not get an unnecessary cache barrier.  */

struct { struct { char i[4]; } a; struct { char j[4]; } b; } s;

NOMIPS16 void
foo (int sel)
{
  s.a.i[0] = 1;
  s.b.j[3] = 100;
}

/* { dg-final { scan-assembler-not "\tcache\t" } } */
