extern void abort (void);

/* { dg-options "-O2" } */
/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

#define TYPE_MAX(type, sign)	\
  ((!sign) ? ((1 << (sizeof (type) * 8 - 1)) - 1) :	\
   ((1 << (sizeof (type) * 8)) - 1))
#define TYPE_MIN(type, sign)	\
  ((!sign) ? -(1 << (sizeof (type) * 8 - 1)) : 0)

#define TEST_FN(NAME, ARG_TYPE, RET_TYPE, CAST_TYPE, VAL, VR_MIN, VR_MAX)\
  __attribute__((noinline, noclone)) RET_TYPE				\
      NAME (ARG_TYPE arg){						\
      RET_TYPE ret = VAL;						\
      if (arg + 1 < VR_MIN || arg + 1 > VR_MAX) return ret;		\
      /* Value Range of arg at this point will be  [VR_min, VR_max].  */\
      arg = arg + VAL;							\
      ret = (CAST_TYPE)arg;						\
      return arg;							\
  }

/* Signed to signed conversion with value in-range.  */
TEST_FN (foo1, short, short, char, 1, TYPE_MIN (char, 0), TYPE_MAX (char, 0));
TEST_FN (foo2, short, short, char, 1, TYPE_MIN (char, 0) + 1,\
	TYPE_MAX (char, 0) - 1);

/* Signed to signed conversion with value not in-range.  */
TEST_FN (foo3, short, short, char, -1, TYPE_MIN (short, 0) + 1,  100);
TEST_FN (foo4, short, short, char, 1, 12, TYPE_MAX (short, 0) + 1);

/* Unsigned to unsigned conversion with value in-range.  */
TEST_FN (foo5, unsigned short, unsigned short, unsigned char, 1,\
	TYPE_MIN (char, 1) + 1, TYPE_MAX (char, 1) - 1);
TEST_FN (foo6, unsigned short, unsigned short, unsigned char, 1,\
	TYPE_MIN (char, 1), TYPE_MAX (char, 1));

/* Unsigned to unsigned conversion with value not in-range.  */
TEST_FN (foo7, unsigned short, unsigned short, unsigned char, 1,\
	TYPE_MIN (short, 1) + 1, TYPE_MAX (short, 1) - 1);
TEST_FN (foo8, unsigned short, unsigned short, unsigned char, 1,\
	TYPE_MIN (short, 1), TYPE_MAX (short, 1));

/* Signed to unsigned conversion with value range positive.  */
TEST_FN (foo9, short, short, unsigned char, -1, 1,\
	TYPE_MAX (char, 1) - 1);
TEST_FN (foo10, short, short, unsigned char, 1, 0,\
	TYPE_MAX (char, 1));

/* Signed to unsigned conversion with value range negative.  */
TEST_FN (foo11, short, short, unsigned char, 1,\
	TYPE_MIN (char, 0) + 1, TYPE_MAX (char, 0) - 1);
TEST_FN (foo12, short, short, unsigned char, 1,\
	TYPE_MIN (char, 0), TYPE_MAX (char, 0));

/* Unsigned to Signed conversion with value range in signed equiv range.  */
TEST_FN (foo13, unsigned short, unsigned short, char, 1,\
	TYPE_MIN (char, 1) + 1, TYPE_MAX (char, 0) - 1);
TEST_FN (foo14, unsigned short, unsigned short, char, 1,\
	TYPE_MIN (char, 1), TYPE_MAX (char, 0));

/* Unsigned to Signed conversion with value range not-in signed range.  */
TEST_FN (foo15, unsigned short, unsigned short, char, 1,\
	TYPE_MIN (char, 1) + 1, TYPE_MAX (char, 1) - 1);
TEST_FN (foo16, unsigned short, unsigned short, char, 1,\
	TYPE_MIN (char, 1), TYPE_MAX (char, 1));

int main ()
{
  /* Signed to signed conversion with value in-range.  */
  /* arg + 1.  */
  if (foo1 (-32) != -31)
    abort ();
  /* arg + 1.  */
  if (foo2 (32) != 33)
    abort ();

  /* Signed to signed conversion with value not in-range.  */
  /* arg - 1.  */
  if (foo3 (-512) != -513)
    abort ();
  /* arg + 1.  */
  if (foo4 (512) != 513)
    abort ();

  /* Unsigned to unsigned conversion with value in-range.  */
  /* arg + 1.  */
  if (foo5 (64) != 65)
    abort ();
  /* arg + 1.  */
  if (foo6 (64) != 65)
    abort ();

  /* Unsigned to unsigned conversion with value not in-range.  */
  /* arg + 1.  */
  if (foo7 (512) != 513)
    abort ();
  /* arg + 1.  */
  if (foo8 (512) != 513)
    abort ();

  /* Signed to unsigned conversion with value range positive.  */
  /* arg - 1.  */
  if (foo9 (2) != 1)
    abort ();
  /* arg + 1.  */
  if (foo10 (2) != 3)
    abort ();

  /* Signed to unsigned conversion with value range negative.  */
  /* arg + 1.  */
  if (foo11 (-125) != -124)
    abort ();
  /* arg + 1.  */
  if (foo12 (-125) != -124)
    abort ();

  /* Unsigned to Signed conversion with value range in signed equiv range.  */
  /* arg + 1.  */
  if (foo13 (125) != 126)
    abort ();
  /* arg + 1.  */
  if (foo14 (125) != 126)
    abort ();

  /* Unsigned to Signed conversion with value range not-in signed range.  */
  /* arg + 1.  */
  if (foo15 (250) != 251)
    abort ();
  /* arg + 1.  */
  if (foo16 (250) != 251)
    abort ();

  return 0;
}

