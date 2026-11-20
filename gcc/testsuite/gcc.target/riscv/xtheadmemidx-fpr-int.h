/* Keep integer-mode values in FPRs across an empty asm, so the tests do
   not depend on IRA choosing a particular class for an unconstrained copy.  */

#define STORE(NAME, ITYPE, FTYPE)                 \
void NAME (ITYPE *dst, long index, FTYPE value)   \
{                                               \
  union { FTYPE f; ITYPE i; } bits = { value };   \
  asm ("" : "+f" (bits.i));                     \
  dst[index] = bits.i;                           \
}

STORE (store_di, unsigned long long, double)
STORE (store_si, unsigned int, float)

void
store_post (unsigned long long *dst, double value, int n)
{
  union { double f; unsigned long long i; } bits = { value };
  asm ("" : "+f" (bits.i));
  for (int i = 0; i < n; ++i)
    {
      *dst++ = bits.i;
      asm volatile ("" ::: "memory");
    }
}
