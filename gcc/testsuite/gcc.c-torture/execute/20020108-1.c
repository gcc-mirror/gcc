/* This file tests shifts in various integral modes.  */

#include <limits.h>

#define CAT(A, B) A ## B

#define REPEAT_8	\
REPEAT_FN ( 0)		\
REPEAT_FN ( 1)		\
REPEAT_FN ( 2)		\
REPEAT_FN ( 3)		\
REPEAT_FN ( 4)		\
REPEAT_FN ( 5)		\
REPEAT_FN ( 6)		\
REPEAT_FN ( 7)

#define REPEAT_16	\
REPEAT_8		\
REPEAT_FN ( 8)		\
REPEAT_FN ( 9)		\
REPEAT_FN (10)		\
REPEAT_FN (11)		\
REPEAT_FN (12)		\
REPEAT_FN (13)		\
REPEAT_FN (14)		\
REPEAT_FN (15)

#define REPEAT_32	\
REPEAT_16		\
REPEAT_FN (16)		\
REPEAT_FN (17)		\
REPEAT_FN (18)		\
REPEAT_FN (19)		\
REPEAT_FN (20)		\
REPEAT_FN (21)		\
REPEAT_FN (22)		\
REPEAT_FN (23)		\
REPEAT_FN (24)		\
REPEAT_FN (25)		\
REPEAT_FN (26)		\
REPEAT_FN (27)		\
REPEAT_FN (28)		\
REPEAT_FN (29)		\
REPEAT_FN (30)		\
REPEAT_FN (31)

/* Define 8-bit shifts.  */
#if CHAR_BIT == 8
typedef unsigned int u8 __attribute__((mode(QI)));
typedef signed int s8 __attribute__((mode(QI)));

#define REPEAT_FN(COUNT) \
u8 CAT (ashift_qi_, COUNT) (u8 n) { return n << COUNT; }
REPEAT_8
#undef REPEAT_FN

#define REPEAT_FN(COUNT) \
u8 CAT (lshiftrt_qi_, COUNT) (u8 n) { return n >> COUNT; }
REPEAT_8
#undef REPEAT_FN

#define REPEAT_FN(COUNT) \
s8 CAT (ashiftrt_qi_, COUNT) (s8 n) { return n >> COUNT; }
REPEAT_8
#undef REPEAT_FN
#endif /* CHAR_BIT == 8 */

/* Define 16-bit shifts.  */
#if CHAR_BIT == 8 || CHAR_BIT == 16
#if CHAR_BIT == 8
typedef unsigned int u16 __attribute__((mode(HI)));
typedef signed int s16 __attribute__((mode(HI)));
#elif CHAR_BIT == 16
typedef unsigned int u16 __attribute__((mode(QI)));
typedef signed int s16 __attribute__((mode(QI)));
#endif

#define REPEAT_FN(COUNT) \
u16 CAT (ashift_hi_, COUNT) (u16 n) { return n << COUNT; }
REPEAT_16
#undef REPEAT_FN

#define REPEAT_FN(COUNT) \
u16 CAT (lshiftrt_hi_, COUNT) (u16 n) { return n >> COUNT; }
REPEAT_16
#undef REPEAT_FN

#define REPEAT_FN(COUNT) \
s16 CAT (ashiftrt_hi_, COUNT) (s16 n) { return n >> COUNT; }
REPEAT_16
#undef REPEAT_FN
#endif /* CHAR_BIT == 8 || CHAR_BIT == 16 */

/* Define 32-bit shifts.  */
#if CHAR_BIT == 8 || CHAR_BIT == 16 || CHAR_BIT == 32
#if CHAR_BIT == 8
typedef unsigned int u32 __attribute__((mode(SI)));
typedef signed int s32 __attribute__((mode(SI)));
#elif CHAR_BIT == 16
typedef unsigned int u32 __attribute__((mode(HI)));
typedef signed int s32 __attribute__((mode(HI)));
#elif CHAR_BIT == 32
typedef unsigned int u32 __attribute__((mode(QI)));
typedef signed int s32 __attribute__((mode(QI)));
#endif

#define REPEAT_FN(COUNT) \
u32 CAT (ashift_si_, COUNT) (u32 n) { return n << COUNT; }
REPEAT_32
#undef REPEAT_FN

#define REPEAT_FN(COUNT) \
u32 CAT (lshiftrt_si_, COUNT) (u32 n) { return n >> COUNT; }
REPEAT_32
#undef REPEAT_FN

#define REPEAT_FN(COUNT) \
s32 CAT (ashiftrt_si_, COUNT) (s32 n) { return n >> COUNT; }
REPEAT_32
#undef REPEAT_FN
#endif /* CHAR_BIT == 8 || CHAR_BIT == 16 || CHAR_BIT == 32 */

extern void abort (void);
extern void exit (int);

int
main ()
{
  /* Test 8-bit shifts.  */
#if CHAR_BIT == 8
# define REPEAT_FN(COUNT) \
  if (CAT (ashift_qi_, COUNT) (0xff) != (u8) ((u8)0xff << COUNT)) abort ();
  REPEAT_8;
# undef REPEAT_FN

# define REPEAT_FN(COUNT) \
  if (CAT (lshiftrt_qi_, COUNT) (0xff) != (u8) ((u8)0xff >> COUNT)) abort ();
  REPEAT_8;
# undef REPEAT_FN

# define REPEAT_FN(COUNT) \
  if (CAT (ashiftrt_qi_, COUNT) (-1) != -1) abort ();
  REPEAT_8;
# undef REPEAT_FN

# define REPEAT_FN(COUNT) \
  if (CAT (ashiftrt_qi_, COUNT) (0) != 0) abort ();
  REPEAT_8;
# undef REPEAT_FN
#endif /* CHAR_BIT == 8 */

  /* Test 16-bit shifts.  */
#if CHAR_BIT == 8 || CHAR_BIT == 16
# define REPEAT_FN(COUNT)			\
  if (CAT (ashift_hi_, COUNT) (0xffff)		\
      != (u16) ((u16) 0xffff << COUNT)) abort ();
  REPEAT_16;
# undef REPEAT_FN

# define REPEAT_FN(COUNT)			\
  if (CAT (lshiftrt_hi_, COUNT) (0xffff)	\
      != (u16) ((u16) 0xffff >> COUNT)) abort ();
  REPEAT_16;
# undef REPEAT_FN

# define REPEAT_FN(COUNT) \
  if (CAT (ashiftrt_hi_, COUNT) (-1) != -1) abort ();
  REPEAT_16;
# undef REPEAT_FN

# define REPEAT_FN(COUNT) \
  if (CAT (ashiftrt_hi_, COUNT) (0) != 0) abort ();
  REPEAT_16;
# undef REPEAT_FN
#endif /* CHAR_BIT == 8 || CHAR_BIT == 16 */

  /* Test 32-bit shifts.  */
#if CHAR_BIT == 8 || CHAR_BIT == 16 || CHAR_BIT == 32
# define REPEAT_FN(COUNT)				\
  if (CAT (ashift_si_, COUNT) (0xffffffff)		\
      != (u32) ((u32) 0xffffffff << COUNT)) abort ();
  REPEAT_32;
# undef REPEAT_FN

# define REPEAT_FN(COUNT)				\
  if (CAT (lshiftrt_si_, COUNT) (0xffffffff)		\
      != (u32) ((u32) 0xffffffff >> COUNT)) abort ();
  REPEAT_32;
# undef REPEAT_FN

# define REPEAT_FN(COUNT) \
  if (CAT (ashiftrt_si_, COUNT) (-1) != -1) abort ();
  REPEAT_32;
# undef REPEAT_FN

# define REPEAT_FN(COUNT) \
  if (CAT (ashiftrt_si_, COUNT) (0) != 0) abort ();
  REPEAT_32;
# undef REPEAT_FN
#endif /* CHAR_BIT == 8 || CHAR_BIT == 16 || CHAR_BIT == 32 */

  exit (0);
}
