/* Origin: Aldy Hernandez <aldyh@redhat.com>

   Purpose: Test generic SIMD support.  This test should work
   regardless of if the target has SIMD instructions.
*/

typedef int __attribute__((vector_size (16))) vecint;
typedef int __attribute__((mode(SI))) siint;

vecint i = { 150, 100, 150, 200 };
vecint j = { 10, 13, 20, 30 };
vecint k;

union {
  vecint v;
  siint i[4];
} res;

/* This should go away once we can use == and != on vector types.  */
void
verify (siint a1, siint a2, siint a3, siint a4,
	siint b1, siint b2, siint b3, siint b4)
{
  if (a1 != b1
      || a2 != b2
      || a3 != b3
      || a4 != b4)
    abort ();
}

int
main ()
{
  k = i + j;
  res.v = k;

  verify (res.i[0], res.i[1], res.i[2], res.i[3], 160, 113, 170, 230);

  k = i * j;
  res.v = k;

  verify (res.i[0], res.i[1], res.i[2], res.i[3], 1500, 1300, 3000, 6000);

  k = i / j;
  res.v = k;

  verify (res.i[0], res.i[1], res.i[2], res.i[3], 15, 7, 7, 6);

  k = i & j;
  res.v = k;

  verify (res.i[0], res.i[1], res.i[2], res.i[3], 2, 4, 20, 8);

  k = i | j;
  res.v = k;

  verify (res.i[0], res.i[1], res.i[2], res.i[3], 158, 109, 150, 222);

  k = i ^ j;
  res.v = k;

  verify (res.i[0], res.i[1], res.i[2], res.i[3], 156, 105, 130, 214);

  k = -i;
  res.v = k;
  verify (res.i[0], res.i[1], res.i[2], res.i[3],
	  -150, -100, -150, -200);

  k = ~i;
  res.v = k;
  verify (res.i[0], res.i[1], res.i[2], res.i[3], -151, -101, -151, -201);

  exit (0);
}
