/* PR rtl-optimization/20532 */

/* { dg-do run } */
/* { dg-options "" } */
/* { dg-options "-march=i386" { target { i?86-*-* && ilp32 } } } */

/* We used to optimize the DImode shift-by-32 to zero because in combine
   we turned:

     (v << 31) + (v << 31)

   into:

     (v * (((HOST_WIDE_INT)1 << 31) + ((HOST_WIDE_INT)1 << 31)))

   With a 32-bit HOST_WIDE_INT, the coefficient overflowed to zero.  */

unsigned long long int badshift(unsigned long long int v)
{
        return v << 31 << 1;
}

extern void abort ();

int main() {
  if (badshift (1) == 0)
    abort ();
  return 0;
}
