/* This file tests shifts in QImode, HImode, and SImode.  */

#define CAT(A, B) A ## B

/* Define 8-bit shifts.  */

#define REPEAT_8				\
REPEAT_FN ( 0)					\
REPEAT_FN ( 1)					\
REPEAT_FN ( 2)					\
REPEAT_FN ( 3)					\
REPEAT_FN ( 4)					\
REPEAT_FN ( 5)					\
REPEAT_FN ( 6)					\
REPEAT_FN ( 7)

typedef unsigned char uchar;
typedef signed char schar;

#define REPEAT_FN(COUNT)					\
uchar CAT (ashift_qi_, COUNT) (uchar n) { return n << COUNT; }
REPEAT_8
#undef REPEAT_FN

#define REPEAT_FN(COUNT)						\
uchar CAT (lshiftrt_qi_, COUNT) (uchar n) { return n >> COUNT; }
REPEAT_8
#undef REPEAT_FN

#define REPEAT_FN(COUNT)						\
schar CAT (ashiftrt_qi_, COUNT) (schar n) { return n >> COUNT; }
REPEAT_8
#undef REPEAT_FN

/* Define 16-bit shifts.  */

#define REPEAT_16				\
REPEAT_8					\
REPEAT_FN ( 8)					\
REPEAT_FN ( 9)					\
REPEAT_FN (10)					\
REPEAT_FN (11)					\
REPEAT_FN (12)					\
REPEAT_FN (13)					\
REPEAT_FN (14)					\
REPEAT_FN (15)

typedef unsigned short ushort;
typedef signed short sshort;

#define REPEAT_FN(COUNT)						\
ushort CAT (ashift_hi_, COUNT) (ushort n) { return n << COUNT; }
REPEAT_16
#undef REPEAT_FN

#define REPEAT_FN(COUNT)						\
ushort CAT (lshiftrt_hi_, COUNT) (ushort n) { return n >> COUNT; }
REPEAT_16
#undef REPEAT_FN

#define REPEAT_FN(COUNT)						\
sshort CAT (ashiftrt_hi_, COUNT) (sshort n) { return n >> COUNT; }
REPEAT_16
#undef REPEAT_FN

/* Define 32-bit shifts.  */

#define REPEAT_32				\
REPEAT_16					\
REPEAT_FN (16)					\
REPEAT_FN (17)					\
REPEAT_FN (18)					\
REPEAT_FN (19)					\
REPEAT_FN (20)					\
REPEAT_FN (21)					\
REPEAT_FN (22)					\
REPEAT_FN (23)					\
REPEAT_FN (24)					\
REPEAT_FN (25)					\
REPEAT_FN (26)					\
REPEAT_FN (27)					\
REPEAT_FN (28)					\
REPEAT_FN (29)					\
REPEAT_FN (30)					\
REPEAT_FN (31)

typedef unsigned long ulong;
typedef signed long slong;

#define REPEAT_FN(COUNT)					\
ulong CAT (ashift_si_, COUNT) (ulong n) { return n << COUNT; }
REPEAT_32
#undef REPEAT_FN

#define REPEAT_FN(COUNT)						\
ulong CAT (lshiftrt_si_, COUNT) (ulong n) { return n >> COUNT; }
REPEAT_32
#undef REPEAT_FN

#define REPEAT_FN(COUNT)						\
slong CAT (ashiftrt_si_, COUNT) (slong n) { return n >> COUNT; }
REPEAT_32
#undef REPEAT_FN

extern void abort (void);
extern void exit (int);

int
main ()
{
  /* Test 8-bit shifts.  */

#define REPEAT_FN(COUNT)			\
  if (CAT (ashift_qi_, COUNT) (0xff)		\
      != (uchar) (0xff << COUNT)) abort ();
  REPEAT_8;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)			\
  if (CAT (lshiftrt_qi_, COUNT) (0xff)	\
      != (uchar) (0xff >> COUNT)) abort ();
  REPEAT_8;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)				\
  if (CAT (ashiftrt_qi_, COUNT) (-1) != -1) abort ();
  REPEAT_8;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)				\
  if (CAT (ashiftrt_qi_, COUNT) (0) != 0) abort ();
  REPEAT_8;
#undef REPEAT_FN

  /* Test 16-bit shifts.  */

#define REPEAT_FN(COUNT)			\
  if (CAT (ashift_hi_, COUNT) (0xffff)		\
      != (ushort) (0xffff << COUNT)) abort ();
  REPEAT_16;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)			\
  if (CAT (lshiftrt_hi_, COUNT) (0xffff)	\
      != (ushort) (0xffff >> COUNT)) abort ();
  REPEAT_16;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)				\
  if (CAT (ashiftrt_hi_, COUNT) (-1) != -1) abort ();
  REPEAT_16;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)				\
  if (CAT (ashiftrt_hi_, COUNT) (0) != 0) abort ();
  REPEAT_16;
#undef REPEAT_FN

  /* Test 32-bit shifts.  */

#define REPEAT_FN(COUNT)				\
  if (CAT (ashift_si_, COUNT) (0xffffffff)		\
      != (ulong) (0xffffffff << COUNT)) abort ();
  REPEAT_32;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)				\
  if (CAT (lshiftrt_si_, COUNT) (0xffffffff)		\
      != (ulong) (0xffffffff >> COUNT)) abort ();
  REPEAT_32;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)				\
  if (CAT (ashiftrt_si_, COUNT) (-1) != -1) abort ();
  REPEAT_32;
#undef REPEAT_FN

#define REPEAT_FN(COUNT)				\
  if (CAT (ashiftrt_si_, COUNT) (0) != 0) abort ();
  REPEAT_32;
#undef REPEAT_FN

  exit (0);
}
