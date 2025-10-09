/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#if __SIZEOF_LONG_DOUBLE__ == 8

typedef long double D;
typedef __INT32_TYPE__ int32_t;

D dd = -0x1p31L;
  
int main (void)
{
  if ((int32_t) dd != -0x7fffffff - 1)
    __builtin_abort();

  return 0;
}
#else
int main (void)
{
  return 0;
}
#endif
