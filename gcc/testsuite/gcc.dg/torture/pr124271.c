/* PR target/124271 */
/* { dg-do run } */

typedef unsigned long long v2du __attribute__ ((vector_size (16)));
typedef unsigned long long v4du __attribute__ ((vector_size (32)));
typedef long long v2di __attribute__ ((vector_size (16)));
typedef unsigned int v4su __attribute__ ((vector_size (16)));
typedef unsigned short v8hu __attribute__ ((vector_size (16)));
typedef unsigned char v8qu __attribute__ ((vector_size (8)));

#define TEST(NAME, VT, ET, UT, M)				\
  __attribute__ ((noipa)) void					\
  NAME (VT *r, VT *a, VT *b)					\
  {								\
    *r = (*a & (ET) M) * (*b & (ET) M);				\
  }								\
								\
  void								\
  check_##NAME (void)						\
  {								\
    VT a, b, r;							\
    for (unsigned i = 0; i < sizeof (VT) / sizeof (ET); i++)	\
      {								\
	a[i] = (ET) (0x9e3779b97f4a7c15ULL * (i + 1));		\
	b[i] = (ET) (0xc2b2ae3d27d4eb4fULL * (i + 3));		\
	/* Keep the signed product from overflowing.  */	\
	if ((ET) -1 < 0)					\
	  {							\
	    a[i] &= (ET) ((UT) M >> 1) | ~(ET) M;		\
	    b[i] &= (ET) ((UT) M >> 1) | ~(ET) M;		\
	  }							\
      }								\
    NAME (&r, &a, &b);						\
    for (unsigned i = 0; i < sizeof (VT) / sizeof (ET); i++)	\
      if ((UT) r[i] != (UT) ((UT) (a[i] & (ET) M)		\
			     * (UT) (b[i] & (ET) M)))		\
	__builtin_abort ();					\
  }

TEST (f1, v2du, unsigned long long, unsigned long long, 0xffffffffULL)
TEST (f2, v4du, unsigned long long, unsigned long long, 0xffffffffULL)
TEST (f3, v2di, long long, unsigned long long, 0xffffffffULL)
TEST (f4, v4su, unsigned int, unsigned int, 0xffffU)
TEST (f5, v8hu, unsigned short, unsigned short, 0xffU)
/* Not a low-half mask.  */
TEST (f6, v2du, unsigned long long, unsigned long long, 0x1ffffffffULL)
TEST (f7, v2du, unsigned long long, unsigned long long, 0x7fffffffULL)
/* No integer mode for the half-width element.  */
TEST (f8, v8qu, unsigned char, unsigned char, 0xfU)

int
main ()
{
  check_f1 ();
  check_f2 ();
  check_f3 ();
  check_f4 ();
  check_f5 ();
  check_f6 ();
  check_f7 ();
  check_f8 ();
  return 0;
}
