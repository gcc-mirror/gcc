// Test EH with V2SI SIMD registers actually restores correct values.
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-options "-O" }
// { dg-do run { target { powerpc_spe && { ! *-*-vxworks* } } } }

extern "C" void abort (void);
extern "C" int memcmp (const void *, const void *, __SIZE_TYPE__);
typedef int __attribute__((vector_size (8))) v2si;

v2si a = { 1, 2 };
v2si b = { 3, 4 };
v2si c = { 4, 6 };
volatile v2si r;
v2si r2;

void
f ()
{
  register v2si v asm("r15");
  v = __builtin_spe_evaddw (b, c);
  asm volatile ("" : "+r" (v));
  r = v;
  throw 1;
}

int
main ()
{
  register v2si v asm("r15");
  v = __builtin_spe_evaddw (a, b);
  asm volatile ("" : "+r" (v));
  try
    {
      f ();
    }
  catch (int)
    {
      r = v;
      r2 = r;
      if (memcmp (&r2, &c, sizeof (v2si)))
	abort ();
    }
  return 0;
}
