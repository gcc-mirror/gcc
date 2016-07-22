/* PR c++/71675 - __atomic_compare_exchange_n returns wrong type for typed enum
 */
/* { dg-do compile { target c11 } } */

#define Test(T)								\
  do {									\
    static T x;								\
    int r [_Generic (__atomic_compare_exchange_n (&x, &x, x, 0, 0, 0),	\
		     _Bool: 1, default: -1)];				\
    (void)&r;								\
  } while (0)

void f (void)
{
  /* __atomic_compare_exchange_n would fail to return _Bool when
     its arguments were one of the three character types.  */
  Test (char);
  Test (signed char);
  Test (unsigned char);

  Test (int);
  Test (unsigned int);

  Test (long);
  Test (unsigned long);

  Test (long long);
  Test (unsigned long long);

  typedef enum E { e } E;
  Test (E);
}
