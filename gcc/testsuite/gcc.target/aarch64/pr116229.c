/* { dg-do run } */
/* { dg-options "-O2 -fno-signed-zeros" } */

typedef __attribute__((__vector_size__ (8))) unsigned long V;

V __attribute__((__noipa__))
foo (void)
{
  return (V){ 0x8000000000000000 };
}

V ref = (V){ 0x8000000000000000 };

int
main ()
{
  V v = foo ();
  if (v[0] != ref[0])
    __builtin_abort();
}
