/* PR tree-optimization/46969 */
/* { dg-do compile } */
/* { dg-options "-O3 -ftree-parallelize-loops=2 -fcompare-debug" } */

extern void abort (void);
#define F(name) \
int								\
name (unsigned char *x)						\
{								\
  int i;							\
  unsigned int c, d, e;						\
  if (x != 0)							\
    {								\
      for (i = 0, d = 0, e = 0xFFFFFFFF;			\
	   i < 64;						\
	   i += (int) sizeof(unsigned int))			\
        {							\
          c = *((unsigned int *)(&x[i]));			\
          d = d | c;						\
          e = e & c;						\
        }							\
      if (!((d == e) && ((d >> 8) == (e & 0x00FFFFFF))))	\
        abort ();						\
    }								\
  return 0;							\
}
F (foo0) F (foo1)
F (foo2) F (foo3)
F (foo4) F (foo5)
F (foo6) F (foo7)
F (foo8) F (foo9)
