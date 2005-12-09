// { dg-do run { target i?86-*-linux* x86_64-*-linux* i?86-*-freebsd* } }
// { dg-require-effective-target ilp32 }
// { dg-options "-malign-double" }
// Origin: Alex Samuel <samuel@codesourcery.com>

/* Test the layout of bitfields in C aggretagtes for compliance with
   the IA-64 ABI.  */

#include <cstring>

template<typename T>
inline unsigned
alignmentof ()
{
  struct S
  {
    char start_;
    T object_;
  };

  return (unsigned) & ((S *) 0)->object_;
}

/* Computes the alignment, in bytes, of TYPE.  */

#define alignof(type) (alignmentof<type> ())

/* Returns true iff all the bits in the range 
   START_BIT <= bit < START_BIT + NUM_BITS, and only those bits, are
   set in the region of memory starting at BUF of LENGTH bytes.  */

bool
check_bits (char *buf,
	    unsigned length,
	    unsigned start_bit,
	    unsigned num_bits)
{
  for (unsigned bit = 0; bit < 8 * length; ++bit) {
    bool is_set = (buf[bit / 8] & (1 << (bit % 8))) != 0;
    if (start_bit <= bit && bit < start_bit + num_bits) {
      if (! is_set)
	return false;
    }
    else {
      if (is_set)
	return false;
    }
  }
  return true;
}

/* Creates a variable of type AGGREGATE, sets FIELD to -1, and
   verifies that NUM_BITS bits starting at START_BIT, and no other
   bits, are set.  If the check fails, returns with value RVAL.  */

#define CHECK_FIELD(AGGREGATE, FIELD, START_BIT, NUM_BITS, RVAL)          \
  do {                                                                    \
    AGGREGATE a__;                                                        \
    std::memset (& a__, 0, sizeof (a__));                                 \
    a__.FIELD = -1;                                                       \
    if (! check_bits ((char *) & a__, sizeof (a__), START_BIT, NUM_BITS)) \
      return RVAL;                                                        \
  } while (0);



/* Structs S1, S2, S3, S4, and union U5 are taken from Intel, "IA-64
   Software Conventions and Runtime Architecture Guide", version of
   August 1999.  */

struct S1
{
  int       j : 5;
  int       k : 6;
  int       m : 7;
};

#ifdef HAVE_IA64_TYPES
struct S2
{
  short     s : 9;
  __int64   j : 9;
  char      c    ;
  short     t : 9;
  short     u : 9;
  char      d    ;
};
#endif  /* HAVE_IA64_TYPES  */

struct S3
{
  char      c    ;
  short     s : 8;
};

union U4
{
  char      c    ;
  short     s : 8;
};

struct S5
{
  char      c    ;
  int         : 0;
  char      d    ;
  short       : 9;
  char      e    ;
  char        : 0;
};


int
main ()
{
  if (sizeof (struct S1)		!=  4)
    return 1;
  if (alignof (struct S1)		!=  4)
    return 2;
  CHECK_FIELD (S1, j,  0,  5,  3);
  CHECK_FIELD (S1, k,  5,  6,  4);
  CHECK_FIELD (S1, m, 11,  7,  5);

#ifdef HAVE_IA64_TYPES
  if (sizeof (struct S2)		!= 16)
    return 6;
  if (alignof (struct S2)		!=  8)
    return 7;
  CHECK_FIELD (S2, s,  0,  9,  8);
  CHECK_FIELD (S2, j,  9,  9,  9);
  CHECK_FIELD (S2, c, 24,  8, 10);
  CHECK_FIELD (S2, t, 32,  9, 11);
  CHECK_FIELD (S2, u, 48,  9, 12);
  CHECK_FIELD (S2, d, 64,  8, 13);
#endif  /* HAVE_IA64_TYPES  */

  if (sizeof (struct S3)		!=  2)
    return 14;
  if (sizeof (struct S3)		!=  2)
    return 15;
  CHECK_FIELD (S3, c,  0,  8, 16);
  CHECK_FIELD (S3, s,  8,  8, 17);

  if (sizeof (union U4) 		!=  2)
    return 18;
  if (alignof (union U4)		!=  2)
    return 19;
  CHECK_FIELD (U4, c,  0,  8, 20);
  CHECK_FIELD (U4, s,  0,  8, 21);

  if (sizeof (struct S5)		!=  9)
    return 22;
  if (alignof (struct S5)		!=  1)
    return 23;
  CHECK_FIELD (S5, c,  0,  8, 24);
  CHECK_FIELD (S5, d, 32,  8, 25);
  CHECK_FIELD (S5, e, 64,  8, 26);

  return 0;
}
