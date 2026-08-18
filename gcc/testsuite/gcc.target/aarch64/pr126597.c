/* { dg-do run } */
/* { dg-options "-O0" } */

typedef int v4si __attribute__ ((vector_size (16)));

__attribute__((noipa)) v4si
f (v4si x)
{
  const v4si mask = { 4, 1, 2, 3 };
  return __builtin_shuffle (x, (v4si) { 0, 0, 0, 0 }, mask);
}

int
main (void)
{
  v4si x = { 1, 2, 3, 4 };
  v4si r = f (x);
  if (r[0] != 0 || r[1] != 2 || r[2] != 3 || r[3] != 4)
    __builtin_abort ();
  return 0;
}
