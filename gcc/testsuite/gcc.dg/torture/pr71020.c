/* { dg-options "-funsafe-math-optimizations" } */

double random_double (void);
int setjmp (void *);
void do_something (void);

#define TEST_UNARY(FUNC)			\
  double					\
  FUNC##_dead (void *buffer)			\
  {						\
    double d = random_double ();		\
    setjmp (buffer);				\
    __builtin_##FUNC (d);			\
    d += 1;					\
    do_something ();				\
    return d;					\
  }						\
						\
  double					\
  FUNC##_live (void *buffer)			\
  {						\
    double d = random_double ();		\
    setjmp (buffer);				\
    d = __builtin_##FUNC (d);			\
    do_something ();				\
    return d;					\
  }


#define TEST_BINARY(FUNC)			\
  double					\
  FUNC##_dead (void *buffer)			\
  {						\
    double d1 = random_double ();		\
    double d2 = random_double ();		\
    setjmp (buffer);				\
    __builtin_##FUNC (d1, d2);			\
    d1 += 1;					\
    d2 += 1;					\
    do_something ();				\
    return d1 + d2;				\
  }						\
						\
  double					\
  FUNC##_live (void *buffer)			\
  {						\
    double d1 = random_double ();		\
    double d2 = random_double ();		\
    setjmp (buffer);				\
    d1 = __builtin_##FUNC (d1, d2);		\
    d2 += 1;					\
    return d1 + d2;				\
  }

TEST_UNARY (acos)
TEST_UNARY (asin)
TEST_UNARY (asinh)
TEST_UNARY (atan)
TEST_UNARY (atanh)
TEST_UNARY (cos)
TEST_UNARY (cosh)
TEST_UNARY (exp)
TEST_UNARY (expm1)
TEST_UNARY (exp2)
TEST_UNARY (exp10)
TEST_UNARY (log)
TEST_UNARY (log2)
TEST_UNARY (log10)
TEST_UNARY (log1p)
TEST_UNARY (significand)
TEST_UNARY (sin)
TEST_UNARY (sinh)
TEST_UNARY (sqrt)

TEST_BINARY (fmod)
TEST_BINARY (remainder)
