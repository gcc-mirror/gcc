/* { dg-options "-std=gnu99" } */

/* C99 6.5.2.2 Function calls.
   Test structure passing and return values involving decimal floating
   point types.  */

extern void abort (void);

struct example
{
  _Decimal128 d128;
  char dummy1;
  _Decimal64 d64;
  char dummy2;
  _Decimal32 d32;
} nums = { 1.0dl, 'a', 2.0dd, 'b', 3.0df };

_Decimal32
d32_field (struct example s)
{
  return s.d32;
}

_Decimal64
d64_field (struct example s)
{
  return s.d64;
}

_Decimal128
d128_field (struct example s)
{
  return s.d128;
}

char
dummy1_field (struct example s)
{
  return s.dummy1;
}

char
dummy2_field (struct example s)
{
  return s.dummy2;
}

_Decimal32
ptr_d32_field (struct example *s)
{
  return s->d32;
}

_Decimal64
ptr_d64_field (struct example *s)
{
  return s->d64;
}

_Decimal128
ptr_d128_field (struct example *s)
{
  return s->d128;
}

char
ptr_dummy1_field (struct example *s)
{
  return s->dummy1;
}

char
ptr_dummy2_field (struct example *s)
{
  return s->dummy2;
}


int
main ()
{
  if (d32_field (nums) != 3.0df) abort ();
  if (d64_field (nums) != 2.0dd) abort ();
  if (d128_field (nums) != 1.0dl) abort ();
  if (dummy1_field (nums) != 'a') abort ();
  if (dummy2_field (nums) != 'b') abort ();

  if (ptr_d32_field (&nums) != 3.0df) abort ();
  if (ptr_d64_field (&nums) != 2.0dd) abort ();
  if (ptr_d128_field (&nums) != 1.0dl) abort ();
  if (ptr_dummy1_field (&nums) != 'a') abort ();
  if (ptr_dummy2_field (&nums) != 'b') abort ();

  return 0;
}
