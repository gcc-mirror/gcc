/* { dg-do compile } */
/* { dg-additional-options "-funswitch-loops" } */

int op, bs;

void
q0 (void)
{
  op += 1 % (op == bs);
}

void __attribute__ ((returns_twice))
co (void)
{
}

void __attribute__ ((simd))
uq (void)
{
  q0 ();
  co ();

  while (bs < 1)
    ++bs;
}
