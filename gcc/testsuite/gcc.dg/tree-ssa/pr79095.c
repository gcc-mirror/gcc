/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-evrp" } */

extern void arf (unsigned x, unsigned y);
extern void baz (unsigned x, unsigned y);

unsigned
f1 (unsigned a, unsigned b)
{
  b = a + 1;
  if (b < a)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}


unsigned
f1r (unsigned a, unsigned b)
{
  b = a + 1;
  if (a < b)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f1n (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(b < a))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f1nr (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(a < b))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}


unsigned
f1o (unsigned a, unsigned b)
{
  b = a + 1;
  if (a < b)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f1ro (unsigned a, unsigned b)
{
  b = a + 1;
  if (b < a)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f1no (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(a < b))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f1nro (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(b < a))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}


unsigned
f2 (unsigned a, unsigned b)
{
  b = a + 1;
  if (b <= a)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f2r (unsigned a, unsigned b)
{
  b = a + 1;
  if (a <= b)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f2n (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(b <= a))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f2nr (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(a <= b))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}


unsigned
f2o (unsigned a, unsigned b)
{
  b = a + 1;
  if (a <= b)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f2ro (unsigned a, unsigned b)
{
  b = a + 1;
  if (b <= a)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f2no (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(a <= b))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f2nro (unsigned a, unsigned b)
{
  b = a + 1;
  if (!(b <= a))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}


unsigned
f3 (unsigned a, unsigned b)
{
  b = a - 1;
  if (b < a)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f3r (unsigned a, unsigned b)
{
  b = a - 1;
  if (a < b)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f3n (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(b < a))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f3nr (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(a < b))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}


unsigned
f3o (unsigned a, unsigned b)
{
  b = a - 1;
  if (a < b)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f3ro (unsigned a, unsigned b)
{
  b = a - 1;
  if (b < a)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f3no (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(a < b))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f3nro (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(b < a))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}


unsigned
f4 (unsigned a, unsigned b)
{
  b = a - 1;
  if (b <= a)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f4r (unsigned a, unsigned b)
{
  b = a - 1;
  if (a <= b)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f4n (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(b <= a))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f4nr (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(a <= b))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}


unsigned
f4o (unsigned a, unsigned b)
{
  b = a - 1;
  if (a <= b)
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

unsigned
f4ro (unsigned a, unsigned b)
{
  b = a - 1;
  if (b <= a)
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f4no (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(a <= b))
    {
      baz (a, b);
      return 42;
    }
  arf (a, b);
  return b;
}

unsigned
f4nro (unsigned a, unsigned b)
{
  b = a - 1;
  if (!(b <= a))
    {
      arf (a, b);
      return 42;
    }
  baz (a, b);
  return b;
}

/* All calls to baz should still reference a & b as arguments. */
/* { dg-final { scan-tree-dump-times "baz \\(a_\[0-9\]+\\(D\\), b_\[0-9\]+\\)" 32 "evrp"} } */


/* All calls to arf should have constant arguments.  */
/* { dg-final { scan-tree-dump-times "arf \\(\[0-9\]+, \[0-9\]+\\)" 32 "evrp"} } */
