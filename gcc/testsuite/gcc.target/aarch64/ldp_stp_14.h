#include <stdint.h>

#define PRAGMA(X) _Pragma (#X)
#define UNROLL(COUNT) PRAGMA (GCC unroll (COUNT))

#define CONST_FN(COUNT, TYPE, VAL)		\
  void						\
  const_##COUNT##_##TYPE##_##VAL (TYPE *x)	\
  {						\
    UNROLL (COUNT)				\
    for (int i = 0; i < COUNT; ++i)		\
      x[i] = VAL;				\
  }

#define DUP_FN(COUNT, TYPE)			\
  void						\
  dup_##COUNT##_##TYPE (TYPE *x, TYPE val)	\
  {						\
    UNROLL (COUNT)				\
    for (int i = 0; i < COUNT; ++i)		\
      x[i] = val;				\
  }

#define CONS2_FN(COUNT, TYPE)					\
  void								\
  cons2_##COUNT##_##TYPE (TYPE *x, TYPE val0, TYPE val1)	\
  {								\
    UNROLL (COUNT)						\
    for (int i = 0; i < COUNT * 2; i += 2)			\
      {								\
	x[i + 0] = val0;					\
	x[i + 1] = val1;					\
      }								\
  }

#define CONS4_FN(COUNT, TYPE)					\
  void								\
  cons4_##COUNT##_##TYPE (TYPE *x, TYPE val0, TYPE val1,	\
			  TYPE val2, TYPE val3)			\
  {								\
    UNROLL (COUNT)						\
    for (int i = 0; i < COUNT * 4; i += 4)			\
      {								\
	x[i + 0] = val0;					\
	x[i + 1] = val1;					\
	x[i + 2] = val2;					\
	x[i + 3] = val3;					\
      }								\
  }

