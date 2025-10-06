/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#if __SIZEOF_LONG_DOUBLE__ == 8
typedef long double D;

extern void sincosl (D, D*, D*);
extern D sinl (D);
extern D cosl (D);

D s1, c1;

int main (void)
{
  for (D x = -20; x < 20; x += 1.1)
    {
      sincosl (x, &s1, &c1);

      __asm ("" : "+r" (x) :: "memory");

      if (s1 != sinl (x))
	__builtin_exit (1);

      if (c1 != cosl (x))
	__builtin_exit (2);
    }

  return 0;
}
#else
int main (void)
{
  return 0;
}
#endif
