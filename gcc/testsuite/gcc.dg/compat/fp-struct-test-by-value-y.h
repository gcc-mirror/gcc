#define TEST(TYPE,TYPE2)					\
extern TYPE g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE;		\
extern TYPE g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE;		\
extern TYPE g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE;	\
extern TYPE g13s##TYPE, g14s##TYPE, g15s##TYPE, g16s##TYPE;	\
								\
extern void check##TYPE (TYPE x, TYPE2 y);			\
								\
void								\
checkg##TYPE (void)						\
{								\
  check##TYPE (  g1s##TYPE,  (TYPE2)1);				\
  check##TYPE (  g2s##TYPE,  (TYPE2)2);				\
  check##TYPE (  g3s##TYPE,  (TYPE2)3);				\
  check##TYPE (  g4s##TYPE,  (TYPE2)4);				\
  check##TYPE (  g5s##TYPE,  (TYPE2)5);				\
  check##TYPE (  g6s##TYPE,  (TYPE2)6);				\
  check##TYPE (  g7s##TYPE,  (TYPE2)7);				\
  check##TYPE (  g8s##TYPE,  (TYPE2)8);				\
  check##TYPE (  g9s##TYPE,  (TYPE2)9);				\
  check##TYPE ( g10s##TYPE, (TYPE2)10);				\
  check##TYPE ( g11s##TYPE, (TYPE2)11);				\
  check##TYPE ( g12s##TYPE, (TYPE2)12);				\
  check##TYPE ( g13s##TYPE, (TYPE2)13);				\
  check##TYPE ( g14s##TYPE, (TYPE2)14);				\
  check##TYPE ( g15s##TYPE, (TYPE2)15);				\
  check##TYPE ( g16s##TYPE, (TYPE2)16);				\
}								\
								\
void								\
test##TYPE (TYPE s1, TYPE s2, TYPE s3, TYPE s4,			\
	    TYPE s5, TYPE s6, TYPE s7, TYPE s8,			\
	    TYPE s9, TYPE s10, TYPE s11, TYPE s12,		\
	    TYPE s13, TYPE s14, TYPE s15, TYPE s16)		\
{								\
  check##TYPE (s1, (TYPE2)1);					\
  check##TYPE (s2, (TYPE2)2);					\
  check##TYPE (s3, (TYPE2)3);					\
  check##TYPE (s4, (TYPE2)4);					\
  check##TYPE (s5, (TYPE2)5);					\
  check##TYPE (s6, (TYPE2)6);					\
  check##TYPE (s7, (TYPE2)7);					\
  check##TYPE (s8, (TYPE2)8);					\
  check##TYPE (s9, (TYPE2)9);					\
  check##TYPE (s10, (TYPE2)10);					\
  check##TYPE (s11, (TYPE2)11);					\
  check##TYPE (s12, (TYPE2)12);					\
  check##TYPE (s13, (TYPE2)13);					\
  check##TYPE (s14, (TYPE2)14);					\
  check##TYPE (s15, (TYPE2)15);					\
  check##TYPE (s16, (TYPE2)16);					\
}								\
								\
void								\
testva##TYPE (int n, ...)					\
{								\
  int i;							\
  va_list ap;							\
  if (test_va)							\
    {								\
      va_start (ap, n);						\
      for (i = 0; i < n; i++)					\
	{							\
	  TYPE t = va_arg (ap, TYPE);				\
	  check##TYPE (t, (TYPE2)i+1);				\
	}							\
      va_end (ap);						\
    }								\
}
