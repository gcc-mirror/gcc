/* Verify straight-line strength reduction for array
   subscripting.

   elems[n-1] is reduced to elems + n * 4 + 0xffffffff * 4, only when
   pointers are of the same size as that of int (assuming 4 bytes).  */

/* { dg-do run } */
/* { dg-options "-O2" } */

struct data
{
  unsigned long elms[1];
} gData;

void __attribute__((noinline))
foo (struct data *dst, unsigned int n)
{
  dst->elms[n - 1] &= 1;
}

int
main ()
{
  foo (&gData, 1);
  return 0;
}

