/* PR tree-optimization/128142 */
/* With -fwrapv the sum of two nonnegative shorts can wrap to a negative
   value, so (x + y) >> 1 must not become an average.  */
/* { dg-do run } */
/* { dg-additional-options "-fwrapv -Wno-psabi" } */

typedef short v8hi __attribute__ ((vector_size (16)));

__attribute__ ((noipa)) v8hi
f (v8hi a, v8hi b)
{
  return ((a & 0x7fff) + (b & 0x7fff)) >> 1;
}

int
main ()
{
  v8hi a = { 0x7fff, 0x7fff, 0x4000, 3, 0, -1, 0x7ffe, 100 };
  v8hi b = { 0x7fff, 1, 0x4000, 4, 0, -1, 1, 200 };
  v8hi r = f (a, b);
  if (r[0] != -1 || r[1] != -0x4000 || r[2] != -0x4000 || r[3] != 3
      || r[4] != 0 || r[5] != -1 || r[6] != 0x3fff || r[7] != 150)
    __builtin_abort ();
  return 0;
}
