/*	A C version of Kahan's Floating Point Test "Paranoia"

Thos Sumner, UCSF, Feb. 1985
David Gay, BTL, Jan. 1986

This is a rewrite from the Pascal version by

B. A. Wichmann, 18 Jan. 1985

(and does NOT exhibit good C programming style).

Adjusted to use Standard C headers 19 Jan. 1992 (dmg);

(C) Apr 19 1983 in BASIC version by:
Professor W. M. Kahan,
567 Evans Hall
Electrical Engineering & Computer Science Dept.
University of California
Berkeley, California 94720
USA

converted to Pascal by:
B. A. Wichmann
National Physical Laboratory
Teddington Middx
TW11 OLW
UK

converted to C by:

David M. Gay		and	Thos Sumner
AT&T Bell Labs			Computer Center, Rm. U-76
600 Mountain Avenue		University of California
Murray Hill, NJ 07974		San Francisco, CA 94143
USA				USA

with simultaneous corrections to the Pascal source (reflected
in the Pascal source available over netlib).
[A couple of bug fixes from dgh = sun!dhough incorporated 31 July 1986.]

Reports of results on various systems from all the versions
of Paranoia are being collected by Richard Karpinski at the
same address as Thos Sumner.  This includes sample outputs,
bug reports, and criticisms.

You may copy this program freely if you acknowledge its source.
Comments on the Pascal version to NPL, please.

The following is from the introductory commentary from Wichmann's work:

The BASIC program of Kahan is written in Microsoft BASIC using many
facilities which have no exact analogy in Pascal.  The Pascal
version below cannot therefore be exactly the same.  Rather than be
a minimal transcription of the BASIC program, the Pascal coding
follows the conventional style of block-structured languages.  Hence
the Pascal version could be useful in producing versions in other
structured languages.

Rather than use identifiers of minimal length (which therefore have
little mnemonic significance), the Pascal version uses meaningful
identifiers as follows [Note: A few changes have been made for C]:


BASIC   C               BASIC   C               BASIC   C

A                       J                       S    StickyBit
A1   AInverse           J0   NoErrors           T
B    Radix                    [Failure]         T0   Underflow
B1   BInverse           J1   NoErrors           T2   ThirtyTwo
B2   RadixD2                  [SeriousDefect]   T5   OneAndHalf
B9   BMinusU2           J2   NoErrors           T7   TwentySeven
C                             [Defect]          T8   TwoForty
C1   CInverse           J3   NoErrors           U    OneUlp
D                             [Flaw]            U0   UnderflowThreshold
D4   FourD              K    PageNo             U1
E0                      L    Milestone          U2
E1                      M                       V
E2   Exp2               N                       V0
E3                      N1                      V8
E5   MinSqEr            O    Zero               V9
E6   SqEr               O1   One                W
E7   MaxSqEr            O2   Two                X
E8                      O3   Three              X1
E9                      O4   Four               X8
F1   MinusOne           O5   Five               X9   Random1
F2   Half               O8   Eight              Y
F3   Third              O9   Nine               Y1
F6                      P    Precision          Y2
F9                      Q                       Y9   Random2
G1   GMult              Q8                      Z
G2   GDiv               Q9                      Z0   PseudoZero
G3   GAddSub            R                       Z1
H                       R1   RMult              Z2
H1   HInverse           R2   RDiv               Z9
I                       R3   RAddSub
IO   NoTrials           R4   RSqrt
I3   IEEE               R9   Random9

SqRWrng

All the variables in BASIC are true variables and in consequence,
the program is more difficult to follow since the "constants" must
be determined (the glossary is very helpful).  The Pascal version
uses Real constants, but checks are added to ensure that the values
are correctly converted by the compiler.

The major textual change to the Pascal version apart from the
identifiersis that named procedures are used, inserting parameters
wherehelpful.  New procedures are also introduced.  The
correspondence is as follows:


BASIC       Pascal
lines

90- 140   Pause
170- 250   Instructions
380- 460   Heading
480- 670   Characteristics
690- 870   History
2940-2950   Random
3710-3740   NewD
4040-4080   DoesYequalX
4090-4110   PrintIfNPositive
4640-4850   TestPartialUnderflow

*/

  /* This version of paranoia has been modified to work with GCC's internal
     software floating point emulation library, as a sanity check of same.

     I'm doing this in C++ so that I can do operator overloading and not
     have to modify so damned much of the existing code.  */

  extern "C" {
#include <stdio.h>
#include <stddef.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <float.h>

    /* This part is made all the more awful because many gcc headers are
       not prepared at all to be parsed as C++.  The biggest stickler
       here is const structure members.  So we include exactly the pieces
       that we need.  */

#define GTY(x)

#include "ansidecl.h"
#include "auto-host.h"
#include "hwint.h"

#undef EXTRA_MODES_FILE

    struct rtx_def;
    typedef struct rtx_def *rtx;
    struct rtvec_def;
    typedef struct rtvec_def *rtvec;
    union tree_node;
    typedef union tree_node *tree;

#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   SYM,
    enum tree_code {
#include "tree.def"
      LAST_AND_UNUSED_TREE_CODE
    };
#undef DEFTREECODE

#define ENUM_BITFIELD(X) enum X
#define class klass

#include "real.h"

#undef class
  }

/* We never produce signals from the library.  Thus setjmp need do nothing.  */
#undef setjmp
#define setjmp(x)  (0)

static bool verbose = false;
static int verbose_index = 0;

/* ====================================================================== */
/* The implementation of the abstract floating point class based on gcc's
   real.c.  I.e. the object of this excersize.  Templated so that we can
   all fp sizes.  */

class real_c_float
{
 public:
  static const enum machine_mode MODE = SFmode;

 private:
  static const int external_max = 128 / 32;
  static const int internal_max
    = (sizeof (REAL_VALUE_TYPE) + sizeof (long) + 1) / sizeof (long);
  long image[external_max < internal_max ? internal_max : external_max];

  void from_long(long);
  void from_str(const char *);
  void binop(int code, const real_c_float&);
  void unop(int code);
  bool cmp(int code, const real_c_float&) const;

 public:
  real_c_float()
    { }
  real_c_float(long l)
    { from_long(l); }
  real_c_float(const char *s)
    { from_str(s); }
  real_c_float(const real_c_float &b)
    { memcpy(image, b.image, sizeof(image)); }

  const real_c_float& operator= (long l)
    { from_long(l); return *this; }
  const real_c_float& operator= (const char *s)
    { from_str(s); return *this; }
  const real_c_float& operator= (const real_c_float &b)
    { memcpy(image, b.image, sizeof(image)); return *this; }

  const real_c_float& operator+= (const real_c_float &b)
    { binop(PLUS_EXPR, b); return *this; }
  const real_c_float& operator-= (const real_c_float &b)
    { binop(MINUS_EXPR, b); return *this; }
  const real_c_float& operator*= (const real_c_float &b)
    { binop(MULT_EXPR, b); return *this; }
  const real_c_float& operator/= (const real_c_float &b)
    { binop(RDIV_EXPR, b); return *this; }

  real_c_float operator- () const
    { real_c_float r(*this); r.unop(NEGATE_EXPR); return r; }
  real_c_float abs () const
    { real_c_float r(*this); r.unop(ABS_EXPR); return r; }

  bool operator <  (const real_c_float &b) const { return cmp(LT_EXPR, b); }
  bool operator <= (const real_c_float &b) const { return cmp(LE_EXPR, b); }
  bool operator == (const real_c_float &b) const { return cmp(EQ_EXPR, b); }
  bool operator != (const real_c_float &b) const { return cmp(NE_EXPR, b); }
  bool operator >= (const real_c_float &b) const { return cmp(GE_EXPR, b); }
  bool operator >  (const real_c_float &b) const { return cmp(GT_EXPR, b); }

  const char * str () const;
  const char * hex () const;
  long integer () const;
  int exp () const;
  void ldexp (int);
};

void
real_c_float::from_long (long l)
{
  REAL_VALUE_TYPE f;

  real_from_integer (&f, MODE, l, l < 0 ? -1 : 0, 0);
  real_to_target (image, &f, MODE);
}

void
real_c_float::from_str (const char *s)
{
  REAL_VALUE_TYPE f;
  const char *p = s;

  if (*p == '-' || *p == '+')
    p++;
  if (strcasecmp(p, "inf") == 0)
    {
      real_inf (&f);
      if (*s == '-')
        real_arithmetic (&f, NEGATE_EXPR, &f, NULL);
    }
  else if (strcasecmp(p, "nan") == 0)
    real_nan (&f, "", 1, MODE);
  else
    real_from_string (&f, s);

  real_to_target (image, &f, MODE);
}

void
real_c_float::binop (int code, const real_c_float &b)
{
  REAL_VALUE_TYPE ai, bi, ri;

  real_from_target (&ai, image, MODE);
  real_from_target (&bi, b.image, MODE);
  real_arithmetic (&ri, code, &ai, &bi);
  real_to_target (image, &ri, MODE);

  if (verbose)
    {
      char ab[64], bb[64], rb[64];
      const real_format *fmt = real_format_for_mode[MODE - QFmode];
      const int digits = (fmt->p * fmt->log2_b + 3) / 4;
      char symbol_for_code;

      real_from_target (&ri, image, MODE);
      real_to_hexadecimal (ab, &ai, sizeof(ab), digits, 0);
      real_to_hexadecimal (bb, &bi, sizeof(bb), digits, 0);
      real_to_hexadecimal (rb, &ri, sizeof(rb), digits, 0);

      switch (code)
	{
	case PLUS_EXPR:
	  symbol_for_code = '+';
	  break;
	case MINUS_EXPR:
	  symbol_for_code = '-';
	  break;
	case MULT_EXPR:
	  symbol_for_code = '*';
	  break;
	case RDIV_EXPR:
	  symbol_for_code = '/';
	  break;
	default:
	  abort ();
	}

      fprintf (stderr, "%6d: %s %c %s = %s\n", verbose_index++,
	       ab, symbol_for_code, bb, rb);
    }
}

void
real_c_float::unop (int code)
{
  REAL_VALUE_TYPE ai, ri;

  real_from_target (&ai, image, MODE);
  real_arithmetic (&ri, code, &ai, NULL);
  real_to_target (image, &ri, MODE);

  if (verbose)
    {
      char ab[64], rb[64];
      const real_format *fmt = real_format_for_mode[MODE - QFmode];
      const int digits = (fmt->p * fmt->log2_b + 3) / 4;
      const char *symbol_for_code;

      real_from_target (&ri, image, MODE);
      real_to_hexadecimal (ab, &ai, sizeof(ab), digits, 0);
      real_to_hexadecimal (rb, &ri, sizeof(rb), digits, 0);

      switch (code)
	{
	case NEGATE_EXPR:
	  symbol_for_code = "-";
	  break;
	case ABS_EXPR:
	  symbol_for_code = "abs ";
	  break;
	default:
	  abort ();
	}

      fprintf (stderr, "%6d: %s%s = %s\n", verbose_index++,
	       symbol_for_code, ab, rb);
    }
}

bool
real_c_float::cmp (int code, const real_c_float &b) const
{
  REAL_VALUE_TYPE ai, bi;
  bool ret;

  real_from_target (&ai, image, MODE);
  real_from_target (&bi, b.image, MODE);
  ret = real_compare (code, &ai, &bi);

  if (verbose)
    {
      char ab[64], bb[64];
      const real_format *fmt = real_format_for_mode[MODE - QFmode];
      const int digits = (fmt->p * fmt->log2_b + 3) / 4;
      const char *symbol_for_code;

      real_to_hexadecimal (ab, &ai, sizeof(ab), digits, 0);
      real_to_hexadecimal (bb, &bi, sizeof(bb), digits, 0);

      switch (code)
	{
	case LT_EXPR:
	  symbol_for_code = "<";
	  break;
	case LE_EXPR:
	  symbol_for_code = "<=";
	  break;
	case EQ_EXPR:
	  symbol_for_code = "==";
	  break;
	case NE_EXPR:
	  symbol_for_code = "!=";
	  break;
	case GE_EXPR:
	  symbol_for_code = ">=";
	  break;
	case GT_EXPR:
	  symbol_for_code = ">";
	  break;
	default:
	  abort ();
	}

      fprintf (stderr, "%6d: %s %s %s = %s\n", verbose_index++,
	       ab, symbol_for_code, bb, (ret ? "true" : "false"));
    }

  return ret;
}

const char *
real_c_float::str() const
{
  REAL_VALUE_TYPE f;
  const real_format *fmt = real_format_for_mode[MODE - QFmode];
  const int digits = int(fmt->p * fmt->log2_b * .30102999566398119521 + 1);

  real_from_target (&f, image, MODE);
  char *buf = new char[digits + 10];
  real_to_decimal (buf, &f, digits+10, digits, 0);

  return buf;
}

const char *
real_c_float::hex() const
{
  REAL_VALUE_TYPE f;
  const real_format *fmt = real_format_for_mode[MODE - QFmode];
  const int digits = (fmt->p * fmt->log2_b + 3) / 4;

  real_from_target (&f, image, MODE);
  char *buf = new char[digits + 10];
  real_to_hexadecimal (buf, &f, digits+10, digits, 0);

  return buf;
}

long
real_c_float::integer() const
{
  REAL_VALUE_TYPE f;
  real_from_target (&f, image, MODE);
  return real_to_integer (&f);
}

int
real_c_float::exp() const
{
  REAL_VALUE_TYPE f;
  real_from_target (&f, image, MODE);
  return real_exponent (&f);
}

void
real_c_float::ldexp (int exp)
{
  REAL_VALUE_TYPE ai;

  real_from_target (&ai, image, MODE);
  real_ldexp (&ai, &ai, exp);
  real_to_target (image, &ai, MODE);
}

/* ====================================================================== */
/* An implementation of the abstract floating point class that uses native
   arithmetic.  Exists for reference and debugging.  */

template<typename T>
class native_float
{
 private:
  // Force intermediate results back to memory.
  volatile T image;

  static T from_str (const char *);
  static T do_abs (T);
  static T verbose_binop (T, char, T, T);
  static T verbose_unop (const char *, T, T);
  static bool verbose_cmp (T, const char *, T, bool);

 public:
  native_float()
    { }
  native_float(long l)
    { image = l; }
  native_float(const char *s)
    { image = from_str(s); }
  native_float(const native_float &b)
    { image = b.image; }

  const native_float& operator= (long l)
    { image = l; return *this; }
  const native_float& operator= (const char *s)
    { image = from_str(s); return *this; }
  const native_float& operator= (const native_float &b)
    { image = b.image; return *this; }

  const native_float& operator+= (const native_float &b)
    {
      image = verbose_binop(image, '+', b.image, image + b.image);
      return *this;
    }
  const native_float& operator-= (const native_float &b)
    {
      image = verbose_binop(image, '-', b.image, image - b.image);
      return *this;
    }
  const native_float& operator*= (const native_float &b)
    {
      image = verbose_binop(image, '*', b.image, image * b.image);
      return *this;
    }
  const native_float& operator/= (const native_float &b)
    {
      image = verbose_binop(image, '/', b.image, image / b.image);
      return *this;
    }

  native_float operator- () const
    {
      native_float r;
      r.image = verbose_unop("-", image, -image);
      return r;
    }
  native_float abs () const
    {
      native_float r;
      r.image = verbose_unop("abs ", image, do_abs(image));
      return r;
    }

  bool operator <  (const native_float &b) const
    { return verbose_cmp(image, "<", b.image, image <  b.image); }
  bool operator <= (const native_float &b) const
    { return verbose_cmp(image, "<=", b.image, image <= b.image); }
  bool operator == (const native_float &b) const
    { return verbose_cmp(image, "==", b.image, image == b.image); }
  bool operator != (const native_float &b) const
    { return verbose_cmp(image, "!=", b.image, image != b.image); }
  bool operator >= (const native_float &b) const
    { return verbose_cmp(image, ">=", b.image, image >= b.image); }
  bool operator >  (const native_float &b) const
    { return verbose_cmp(image, ">", b.image, image > b.image); }

  const char * str () const;
  const char * hex () const;
  long integer () const
    { return long(image); }
  int exp () const;
  void ldexp (int);
};

template<typename T>
inline T
native_float<T>::from_str (const char *s)
{
  return strtold (s, NULL);
}

template<>
inline float
native_float<float>::from_str (const char *s)
{
  return strtof (s, NULL);
}

template<>
inline double
native_float<double>::from_str (const char *s)
{
  return strtod (s, NULL);
}

template<typename T>
inline T
native_float<T>::do_abs (T image)
{
  return fabsl (image);
}

template<>
inline float
native_float<float>::do_abs (float image)
{
  return fabsf (image);
}

template<>
inline double
native_float<double>::do_abs (double image)
{
  return fabs (image);
}

template<typename T>
T
native_float<T>::verbose_binop (T a, char symbol, T b, T r)
{
  if (verbose)
    {
      const int digits = int(sizeof(T) * CHAR_BIT / 4) - 1;
#ifdef NO_LONG_DOUBLE
      fprintf (stderr, "%6d: %.*a %c %.*a = %.*a\n", verbose_index++,
	       digits, (double)a, symbol,
	       digits, (double)b, digits, (double)r);
#else
      fprintf (stderr, "%6d: %.*La %c %.*La = %.*La\n", verbose_index++,
	       digits, (long double)a, symbol,
	       digits, (long double)b, digits, (long double)r);
#endif
    }
  return r;
}

template<typename T>
T
native_float<T>::verbose_unop (const char *symbol, T a, T r)
{
  if (verbose)
    {
      const int digits = int(sizeof(T) * CHAR_BIT / 4) - 1;
#ifdef NO_LONG_DOUBLE
      fprintf (stderr, "%6d: %s%.*a = %.*a\n", verbose_index++,
	       symbol, digits, (double)a, digits, (double)r);
#else
      fprintf (stderr, "%6d: %s%.*La = %.*La\n", verbose_index++,
	       symbol, digits, (long double)a, digits, (long double)r);
#endif
    }
  return r;
}

template<typename T>
bool
native_float<T>::verbose_cmp (T a, const char *symbol, T b, bool r)
{
  if (verbose)
    {
      const int digits = int(sizeof(T) * CHAR_BIT / 4) - 1;
#ifndef NO_LONG_DOUBLE
      fprintf (stderr, "%6d: %.*a %s %.*a = %s\n", verbose_index++,
	       digits, (double)a, symbol,
	       digits, (double)b, (r ? "true" : "false"));
#else
      fprintf (stderr, "%6d: %.*La %s %.*La = %s\n", verbose_index++,
	       digits, (long double)a, symbol,
	       digits, (long double)b, (r ? "true" : "false"));
#endif
    }
  return r;
}

template<typename T>
const char *
native_float<T>::str() const
{
  char *buf = new char[50];
  const int digits = int(sizeof(T) * CHAR_BIT * .30102999566398119521 + 1);
#ifndef NO_LONG_DOUBLE
  sprintf (buf, "%.*e", digits - 1, (double) image);
#else
  sprintf (buf, "%.*Le", digits - 1, (long double) image);
#endif
  return buf;
}

template<typename T>
const char *
native_float<T>::hex() const
{
  char *buf = new char[50];
  const int digits = int(sizeof(T) * CHAR_BIT / 4);
#ifndef NO_LONG_DOUBLE
  sprintf (buf, "%.*a", digits - 1, (double) image);
#else
  sprintf (buf, "%.*La", digits - 1, (long double) image);
#endif
  return buf;
}

template<typename T>
int
native_float<T>::exp() const
{
  int e;
  frexp (image, &e);
  return e;
}

template<typename T>
void
native_float<T>::ldexp (int exp)
{
  image = ldexpl (image, exp);
}

template<>
void
native_float<float>::ldexp (int exp)
{
  image = ldexpf (image, exp);
}

template<>
void
native_float<double>::ldexp (int exp)
{
  image = ::ldexp (image, exp);
}

/* ====================================================================== */
/* Some libm routines that Paranoia expects to be available.  */

template<typename FLOAT>
inline FLOAT
FABS (const FLOAT &f)
{
  return f.abs();
}

template<typename FLOAT, typename RHS>
inline FLOAT
operator+ (const FLOAT &a, const RHS &b)
{
  return FLOAT(a) += FLOAT(b);
}

template<typename FLOAT, typename RHS>
inline FLOAT
operator- (const FLOAT &a, const RHS &b)
{
  return FLOAT(a) -= FLOAT(b);
}

template<typename FLOAT, typename RHS>
inline FLOAT
operator* (const FLOAT &a, const RHS &b)
{
  return FLOAT(a) *= FLOAT(b);
}

template<typename FLOAT, typename RHS>
inline FLOAT
operator/ (const FLOAT &a, const RHS &b)
{
  return FLOAT(a) /= FLOAT(b);
}

template<typename FLOAT>
FLOAT
FLOOR (const FLOAT &f)
{
  /* ??? This is only correct when F is representable as an integer.  */
  long i = f.integer();
  FLOAT r;

  r = i;
  if (i < 0 && f != r)
    r = i - 1;

  return r;
}

template<typename FLOAT>
FLOAT
SQRT (const FLOAT &f)
{
#if 0
  FLOAT zero = long(0);
  FLOAT two = 2;
  FLOAT one = 1;
  FLOAT diff, diff2;
  FLOAT z, t;

  if (f == zero)
    return zero;
  if (f < zero)
    return zero / zero;
  if (f == one)
    return f;

  z = f;
  z.ldexp (-f.exp() / 2);

  diff2 = FABS (z * z - f);
  if (diff2 > zero)
    while (1)
      {
	t = (f / (two * z)) + (z / two);
	diff = FABS (t * t - f);
	if (diff >= diff2)
	  break;
	z = t;
	diff2 = diff;
      }

  return z;
#elif defined(NO_LONG_DOUBLE)
  double d;
  char buf[64];

  d = strtod (f.hex(), NULL);
  d = sqrt (d);
  sprintf(buf, "%.35a", d);

  return FLOAT(buf);
#else
  long double ld;
  char buf[64];

  ld = strtold (f.hex(), NULL);
  ld = sqrtl (ld);
  sprintf(buf, "%.35La", ld);

  return FLOAT(buf);
#endif
}

template<typename FLOAT>
FLOAT
LOG (FLOAT x)
{
#if 0
  FLOAT zero = long(0);
  FLOAT one = 1;

  if (x <= zero)
    return zero / zero;
  if (x == one)
    return zero;

  int exp = x.exp() - 1;
  x.ldexp(-exp);

  FLOAT xm1 = x - one;
  FLOAT y = xm1;
  long n = 2;

  FLOAT sum = xm1;
  while (1)
    {
      y *= xm1;
      FLOAT term = y / FLOAT (n);
      FLOAT next = sum + term;
      if (next == sum)
	break;
      sum = next;
      if (++n == 1000)
	break;
    }

  if (exp)
    sum += FLOAT (exp) * FLOAT(".69314718055994530941");

  return sum;
#elif defined (NO_LONG_DOUBLE)
  double d;
  char buf[64];

  d = strtod (x.hex(), NULL);
  d = log (d);
  sprintf(buf, "%.35a", d);

  return FLOAT(buf);
#else
  long double ld;
  char buf[64];

  ld = strtold (x.hex(), NULL);
  ld = logl (ld);
  sprintf(buf, "%.35La", ld);

  return FLOAT(buf);
#endif
}

template<typename FLOAT>
FLOAT
EXP (const FLOAT &x)
{
  /* Cheat.  */
#ifdef NO_LONG_DOUBLE
  double d;
  char buf[64];

  d = strtod (x.hex(), NULL);
  d = exp (d);
  sprintf(buf, "%.35a", d);

  return FLOAT(buf);
#else
  long double ld;
  char buf[64];

  ld = strtold (x.hex(), NULL);
  ld = expl (ld);
  sprintf(buf, "%.35La", ld);

  return FLOAT(buf);
#endif
}

template<typename FLOAT>
FLOAT
POW (const FLOAT &base, const FLOAT &exp)
{
  /* Cheat.  */
#ifdef NO_LONG_DOUBLE
  double d1, d2;
  char buf[64];

  d1 = strtod (base.hex(), NULL);
  d2 = strtod (exp.hex(), NULL);
  d1 = pow (d1, d2);
  sprintf(buf, "%.35a", d1);

  return FLOAT(buf);
#else
  long double ld1, ld2;
  char buf[64];

  ld1 = strtold (base.hex(), NULL);
  ld2 = strtold (exp.hex(), NULL);
  ld1 = powl (ld1, ld2);
  sprintf(buf, "%.35La", ld1);

  return FLOAT(buf);
#endif
}

/* ====================================================================== */
/* Real Paranoia begins again here.  We wrap the thing in a template so
   that we can instantiate it for each floating point type we care for.  */

int NoTrials = 20;		/*Number of tests for commutativity. */
bool do_pause = false;

enum Guard { No, Yes };
enum Rounding { Other, Rounded, Chopped };
enum Class { Failure, Serious, Defect, Flaw };

template<typename FLOAT>
struct Paranoia
{
  FLOAT Radix, BInvrse, RadixD2, BMinusU2;

  /* Small floating point constants.  */
  FLOAT Zero;
  FLOAT Half;
  FLOAT One;
  FLOAT Two;
  FLOAT Three;
  FLOAT Four;
  FLOAT Five;
  FLOAT Eight;
  FLOAT Nine;
  FLOAT TwentySeven;
  FLOAT ThirtyTwo;
  FLOAT TwoForty;
  FLOAT MinusOne;
  FLOAT OneAndHalf;

  /* Declarations of Variables.  */
  int Indx;
  char ch[8];
  FLOAT AInvrse, A1;
  FLOAT C, CInvrse;
  FLOAT D, FourD;
  FLOAT E0, E1, Exp2, E3, MinSqEr;
  FLOAT SqEr, MaxSqEr, E9;
  FLOAT Third;
  FLOAT F6, F9;
  FLOAT H, HInvrse;
  int I;
  FLOAT StickyBit, J;
  FLOAT MyZero;
  FLOAT Precision;
  FLOAT Q, Q9;
  FLOAT R, Random9;
  FLOAT T, Underflow, S;
  FLOAT OneUlp, UfThold, U1, U2;
  FLOAT V, V0, V9;
  FLOAT W;
  FLOAT X, X1, X2, X8, Random1;
  FLOAT Y, Y1, Y2, Random2;
  FLOAT Z, PseudoZero, Z1, Z2, Z9;
  int ErrCnt[4];
  int Milestone;
  int PageNo;
  int M, N, N1;
  Guard GMult, GDiv, GAddSub;
  Rounding RMult, RDiv, RAddSub, RSqrt;
  int Break, Done, NotMonot, Monot, Anomaly, IEEE, SqRWrng, UfNGrad;

  /* Computed constants. */
  /*U1  gap below 1.0, i.e, 1.0-U1 is next number below 1.0 */
  /*U2  gap above 1.0, i.e, 1.0+U2 is next number above 1.0 */

  int main ();

  FLOAT Sign (FLOAT);
  FLOAT Random ();
  void Pause ();
  void BadCond (int, const char *);
  void SqXMinX (int);
  void TstCond (int, int, const char *);
  void notify (const char *);
  void IsYeqX ();
  void NewD ();
  void PrintIfNPositive ();
  void SR3750 ();
  void TstPtUf ();

  // Pretend we're bss.
  Paranoia() { memset(this, 0, sizeof (*this)); }
};

template<typename FLOAT>
int
Paranoia<FLOAT>::main()
{
  /* First two assignments use integer right-hand sides. */
  Zero = long(0);
  One = long(1);
  Two = long(2);
  Three = long(3);
  Four = long(4);
  Five = long(5);
  Eight = long(8);
  Nine = long(9);
  TwentySeven = long(27);
  ThirtyTwo = long(32);
  TwoForty = long(240);
  MinusOne = long(-1);
  Half = "0x1p-1";
  OneAndHalf = "0x3p-1";
  ErrCnt[Failure] = 0;
  ErrCnt[Serious] = 0;
  ErrCnt[Defect] = 0;
  ErrCnt[Flaw] = 0;
  PageNo = 1;
	/*=============================================*/
  Milestone = 7;
	/*=============================================*/
  printf ("Program is now RUNNING tests on small integers:\n");

  TstCond (Failure, (Zero + Zero == Zero), "0+0 != 0");
  TstCond (Failure, (One - One == Zero), "1-1 != 0");
  TstCond (Failure, (One > Zero), "1 <= 0");
  TstCond (Failure, (One + One == Two), "1+1 != 2");

  Z = -Zero;
  if (Z != Zero)
    {
      ErrCnt[Failure] = ErrCnt[Failure] + 1;
      printf ("Comparison alleges that -0.0 is Non-zero!\n");
      U2 = "0.001";
      Radix = 1;
      TstPtUf ();
    }

  TstCond (Failure, (Three == Two + One), "3 != 2+1");
  TstCond (Failure, (Four == Three + One), "4 != 3+1");
  TstCond (Failure, (Four + Two * (-Two) == Zero), "4 + 2*(-2) != 0");
  TstCond (Failure, (Four - Three - One == Zero), "4-3-1 != 0");

  TstCond (Failure, (MinusOne == (Zero - One)), "-1 != 0-1");
  TstCond (Failure, (MinusOne + One == Zero), "-1+1 != 0");
  TstCond (Failure, (One + MinusOne == Zero), "1+(-1) != 0");
  TstCond (Failure, (MinusOne + FABS (One) == Zero), "-1+abs(1) != 0");
  TstCond (Failure, (MinusOne + MinusOne * MinusOne == Zero),
	   "-1+(-1)*(-1) != 0");

  TstCond (Failure, Half + MinusOne + Half == Zero, "1/2 + (-1) + 1/2 != 0");

	/*=============================================*/
  Milestone = 10;
	/*=============================================*/

  TstCond (Failure, (Nine == Three * Three), "9 != 3*3");
  TstCond (Failure, (TwentySeven == Nine * Three), "27 != 9*3");
  TstCond (Failure, (Eight == Four + Four), "8 != 4+4");
  TstCond (Failure, (ThirtyTwo == Eight * Four), "32 != 8*4");
  TstCond (Failure, (ThirtyTwo - TwentySeven - Four - One == Zero),
	   "32-27-4-1 != 0");

  TstCond (Failure, Five == Four + One, "5 != 4+1");
  TstCond (Failure, TwoForty == Four * Five * Three * Four, "240 != 4*5*3*4");
  TstCond (Failure, TwoForty / Three - Four * Four * Five == Zero,
	   "240/3 - 4*4*5 != 0");
  TstCond (Failure, TwoForty / Four - Five * Three * Four == Zero,
	   "240/4 - 5*3*4 != 0");
  TstCond (Failure, TwoForty / Five - Four * Three * Four == Zero,
	   "240/5 - 4*3*4 != 0");

  if (ErrCnt[Failure] == 0)
    {
      printf ("-1, 0, 1/2, 1, 2, 3, 4, 5, 9, 27, 32 & 240 are O.K.\n");
      printf ("\n");
    }
  printf ("Searching for Radix and Precision.\n");
  W = One;
  do
    {
      W = W + W;
      Y = W + One;
      Z = Y - W;
      Y = Z - One;
    }
  while (MinusOne + FABS (Y) < Zero);
  /*.. now W is just big enough that |((W+1)-W)-1| >= 1 ... */
  Precision = Zero;
  Y = One;
  do
    {
      Radix = W + Y;
      Y = Y + Y;
      Radix = Radix - W;
    }
  while (Radix == Zero);
  if (Radix < Two)
    Radix = One;
  printf ("Radix = %s .\n", Radix.str());
  if (Radix != One)
    {
      W = One;
      do
	{
	  Precision = Precision + One;
	  W = W * Radix;
	  Y = W + One;
	}
      while ((Y - W) == One);
    }
  /*... now W == Radix^Precision is barely too big to satisfy (W+1)-W == 1
     ... */
  U1 = One / W;
  U2 = Radix * U1;
  printf ("Closest relative separation found is U1 = %s .\n\n", U1.str());
  printf ("Recalculating radix and precision\n ");

  /*save old values */
  E0 = Radix;
  E1 = U1;
  E9 = U2;
  E3 = Precision;

  X = Four / Three;
  Third = X - One;
  F6 = Half - Third;
  X = F6 + F6;
  X = FABS (X - Third);
  if (X < U2)
    X = U2;

  /*... now X = (unknown no.) ulps of 1+... */
  do
    {
      U2 = X;
      Y = Half * U2 + ThirtyTwo * U2 * U2;
      Y = One + Y;
      X = Y - One;
    }
  while (!((U2 <= X) || (X <= Zero)));

  /*... now U2 == 1 ulp of 1 + ... */
  X = Two / Three;
  F6 = X - Half;
  Third = F6 + F6;
  X = Third - Half;
  X = FABS (X + F6);
  if (X < U1)
    X = U1;

  /*... now  X == (unknown no.) ulps of 1 -... */
  do
    {
      U1 = X;
      Y = Half * U1 + ThirtyTwo * U1 * U1;
      Y = Half - Y;
      X = Half + Y;
      Y = Half - X;
      X = Half + Y;
    }
  while (!((U1 <= X) || (X <= Zero)));
  /*... now U1 == 1 ulp of 1 - ... */
  if (U1 == E1)
    printf ("confirms closest relative separation U1 .\n");
  else
    printf ("gets better closest relative separation U1 = %s .\n", U1.str());
  W = One / U1;
  F9 = (Half - U1) + Half;

  Radix = FLOOR (FLOAT ("0.01") + U2 / U1);
  if (Radix == E0)
    printf ("Radix confirmed.\n");
  else
    printf ("MYSTERY: recalculated Radix = %s .\n", Radix.str());
  TstCond (Defect, Radix <= Eight + Eight,
	   "Radix is too big: roundoff problems");
  TstCond (Flaw, (Radix == Two) || (Radix == 10)
	   || (Radix == One), "Radix is not as good as 2 or 10");
	/*=============================================*/
  Milestone = 20;
	/*=============================================*/
  TstCond (Failure, F9 - Half < Half,
	   "(1-U1)-1/2 < 1/2 is FALSE, prog. fails?");
  X = F9;
  I = 1;
  Y = X - Half;
  Z = Y - Half;
  TstCond (Failure, (X != One)
	   || (Z == Zero), "Comparison is fuzzy,X=1 but X-1/2-1/2 != 0");
  X = One + U2;
  I = 0;
	/*=============================================*/
  Milestone = 25;
	/*=============================================*/
  /*... BMinusU2 = nextafter(Radix, 0) */
  BMinusU2 = Radix - One;
  BMinusU2 = (BMinusU2 - U2) + One;
  /* Purify Integers */
  if (Radix != One)
    {
      X = -TwoForty * LOG (U1) / LOG (Radix);
      Y = FLOOR (Half + X);
      if (FABS (X - Y) * Four < One)
	X = Y;
      Precision = X / TwoForty;
      Y = FLOOR (Half + Precision);
      if (FABS (Precision - Y) * TwoForty < Half)
	Precision = Y;
    }
  if ((Precision != FLOOR (Precision)) || (Radix == One))
    {
      printf ("Precision cannot be characterized by an Integer number\n");
      printf
	("of significant digits but, by itself, this is a minor flaw.\n");
    }
  if (Radix == One)
    printf
      ("logarithmic encoding has precision characterized solely by U1.\n");
  else
    printf ("The number of significant digits of the Radix is %s .\n",
	    Precision.str());
  TstCond (Serious, U2 * Nine * Nine * TwoForty < One,
	   "Precision worse than 5 decimal figures  ");
	/*=============================================*/
  Milestone = 30;
	/*=============================================*/
  /* Test for extra-precise subexpressions */
  X = FABS (((Four / Three - One) - One / Four) * Three - One / Four);
  do
    {
      Z2 = X;
      X = (One + (Half * Z2 + ThirtyTwo * Z2 * Z2)) - One;
    }
  while (!((Z2 <= X) || (X <= Zero)));
  X = Y = Z = FABS ((Three / Four - Two / Three) * Three - One / Four);
  do
    {
      Z1 = Z;
      Z = (One / Two - ((One / Two - (Half * Z1 + ThirtyTwo * Z1 * Z1))
			+ One / Two)) + One / Two;
    }
  while (!((Z1 <= Z) || (Z <= Zero)));
  do
    {
      do
	{
	  Y1 = Y;
	  Y =
	    (Half - ((Half - (Half * Y1 + ThirtyTwo * Y1 * Y1)) + Half)) +
	    Half;
	}
      while (!((Y1 <= Y) || (Y <= Zero)));
      X1 = X;
      X = ((Half * X1 + ThirtyTwo * X1 * X1) - F9) + F9;
    }
  while (!((X1 <= X) || (X <= Zero)));
  if ((X1 != Y1) || (X1 != Z1))
    {
      BadCond (Serious, "Disagreements among the values X1, Y1, Z1,\n");
      printf ("respectively  %s,  %s,  %s,\n", X1.str(), Y1.str(), Z1.str());
      printf ("are symptoms of inconsistencies introduced\n");
      printf ("by extra-precise evaluation of arithmetic subexpressions.\n");
      notify ("Possibly some part of this");
      if ((X1 == U1) || (Y1 == U1) || (Z1 == U1))
	printf ("That feature is not tested further by this program.\n");
    }
  else
    {
      if ((Z1 != U1) || (Z2 != U2))
	{
	  if ((Z1 >= U1) || (Z2 >= U2))
	    {
	      BadCond (Failure, "");
	      notify ("Precision");
	      printf ("\tU1 = %s, Z1 - U1 = %s\n", U1.str(), (Z1 - U1).str());
	      printf ("\tU2 = %s, Z2 - U2 = %s\n", U2.str(), (Z2 - U2).str());
	    }
	  else
	    {
	      if ((Z1 <= Zero) || (Z2 <= Zero))
		{
		  printf ("Because of unusual Radix = %s", Radix.str());
		  printf (", or exact rational arithmetic a result\n");
		  printf ("Z1 = %s, or Z2 = %s ", Z1.str(), Z2.str());
		  notify ("of an\nextra-precision");
		}
	      if (Z1 != Z2 || Z1 > Zero)
		{
		  X = Z1 / U1;
		  Y = Z2 / U2;
		  if (Y > X)
		    X = Y;
		  Q = -LOG (X);
		  printf ("Some subexpressions appear to be calculated "
			  "extra precisely\n");
		  printf ("with about %s extra B-digits, i.e.\n",
			  (Q / LOG (Radix)).str());
		  printf ("roughly %s extra significant decimals.\n",
			  (Q / LOG (FLOAT (10))).str());
		}
	      printf
		("That feature is not tested further by this program.\n");
	    }
	}
    }
  Pause ();
	/*=============================================*/
  Milestone = 35;
	/*=============================================*/
  if (Radix >= Two)
    {
      X = W / (Radix * Radix);
      Y = X + One;
      Z = Y - X;
      T = Z + U2;
      X = T - Z;
      TstCond (Failure, X == U2,
	       "Subtraction is not normalized X=Y,X+Z != Y+Z!");
      if (X == U2)
	printf ("Subtraction appears to be normalized, as it should be.");
    }
  printf ("\nChecking for guard digit in *, /, and -.\n");
  Y = F9 * One;
  Z = One * F9;
  X = F9 - Half;
  Y = (Y - Half) - X;
  Z = (Z - Half) - X;
  X = One + U2;
  T = X * Radix;
  R = Radix * X;
  X = T - Radix;
  X = X - Radix * U2;
  T = R - Radix;
  T = T - Radix * U2;
  X = X * (Radix - One);
  T = T * (Radix - One);
  if ((X == Zero) && (Y == Zero) && (Z == Zero) && (T == Zero))
    GMult = Yes;
  else
    {
      GMult = No;
      TstCond (Serious, false, "* lacks a Guard Digit, so 1*X != X");
    }
  Z = Radix * U2;
  X = One + Z;
  Y = FABS ((X + Z) - X * X) - U2;
  X = One - U2;
  Z = FABS ((X - U2) - X * X) - U1;
  TstCond (Failure, (Y <= Zero)
	   && (Z <= Zero), "* gets too many final digits wrong.\n");
  Y = One - U2;
  X = One + U2;
  Z = One / Y;
  Y = Z - X;
  X = One / Three;
  Z = Three / Nine;
  X = X - Z;
  T = Nine / TwentySeven;
  Z = Z - T;
  TstCond (Defect, X == Zero && Y == Zero && Z == Zero,
	   "Division lacks a Guard Digit, so error can exceed 1 ulp\n"
	   "or  1/3  and  3/9  and  9/27 may disagree");
  Y = F9 / One;
  X = F9 - Half;
  Y = (Y - Half) - X;
  X = One + U2;
  T = X / One;
  X = T - X;
  if ((X == Zero) && (Y == Zero) && (Z == Zero))
    GDiv = Yes;
  else
    {
      GDiv = No;
      TstCond (Serious, false, "Division lacks a Guard Digit, so X/1 != X");
    }
  X = One / (One + U2);
  Y = X - Half - Half;
  TstCond (Serious, Y < Zero, "Computed value of 1/1.000..1 >= 1");
  X = One - U2;
  Y = One + Radix * U2;
  Z = X * Radix;
  T = Y * Radix;
  R = Z / Radix;
  StickyBit = T / Radix;
  X = R - X;
  Y = StickyBit - Y;
  TstCond (Failure, X == Zero && Y == Zero,
	   "* and/or / gets too many last digits wrong");
  Y = One - U1;
  X = One - F9;
  Y = One - Y;
  T = Radix - U2;
  Z = Radix - BMinusU2;
  T = Radix - T;
  if ((X == U1) && (Y == U1) && (Z == U2) && (T == U2))
    GAddSub = Yes;
  else
    {
      GAddSub = No;
      TstCond (Serious, false,
	       "- lacks Guard Digit, so cancellation is obscured");
    }
  if (F9 != One && F9 - One >= Zero)
    {
      BadCond (Serious, "comparison alleges  (1-U1) < 1  although\n");
      printf ("  subtraction yields  (1-U1) - 1 = 0 , thereby vitiating\n");
      printf ("  such precautions against division by zero as\n");
      printf ("  ...  if (X == 1.0) {.....} else {.../(X-1.0)...}\n");
    }
  if (GMult == Yes && GDiv == Yes && GAddSub == Yes)
    printf
      ("     *, /, and - appear to have guard digits, as they should.\n");
	/*=============================================*/
  Milestone = 40;
	/*=============================================*/
  Pause ();
  printf ("Checking rounding on multiply, divide and add/subtract.\n");
  RMult = Other;
  RDiv = Other;
  RAddSub = Other;
  RadixD2 = Radix / Two;
  A1 = Two;
  Done = false;
  do
    {
      AInvrse = Radix;
      do
	{
	  X = AInvrse;
	  AInvrse = AInvrse / A1;
	}
      while (!(FLOOR (AInvrse) != AInvrse));
      Done = (X == One) || (A1 > Three);
      if (!Done)
	A1 = Nine + One;
    }
  while (!(Done));
  if (X == One)
    A1 = Radix;
  AInvrse = One / A1;
  X = A1;
  Y = AInvrse;
  Done = false;
  do
    {
      Z = X * Y - Half;
      TstCond (Failure, Z == Half, "X * (1/X) differs from 1");
      Done = X == Radix;
      X = Radix;
      Y = One / X;
    }
  while (!(Done));
  Y2 = One + U2;
  Y1 = One - U2;
  X = OneAndHalf - U2;
  Y = OneAndHalf + U2;
  Z = (X - U2) * Y2;
  T = Y * Y1;
  Z = Z - X;
  T = T - X;
  X = X * Y2;
  Y = (Y + U2) * Y1;
  X = X - OneAndHalf;
  Y = Y - OneAndHalf;
  if ((X == Zero) && (Y == Zero) && (Z == Zero) && (T <= Zero))
    {
      X = (OneAndHalf + U2) * Y2;
      Y = OneAndHalf - U2 - U2;
      Z = OneAndHalf + U2 + U2;
      T = (OneAndHalf - U2) * Y1;
      X = X - (Z + U2);
      StickyBit = Y * Y1;
      S = Z * Y2;
      T = T - Y;
      Y = (U2 - Y) + StickyBit;
      Z = S - (Z + U2 + U2);
      StickyBit = (Y2 + U2) * Y1;
      Y1 = Y2 * Y1;
      StickyBit = StickyBit - Y2;
      Y1 = Y1 - Half;
      if ((X == Zero) && (Y == Zero) && (Z == Zero) && (T == Zero)
	  && (StickyBit == Zero) && (Y1 == Half))
	{
	  RMult = Rounded;
	  printf ("Multiplication appears to round correctly.\n");
	}
      else if ((X + U2 == Zero) && (Y < Zero) && (Z + U2 == Zero)
	       && (T < Zero) && (StickyBit + U2 == Zero) && (Y1 < Half))
	{
	  RMult = Chopped;
	  printf ("Multiplication appears to chop.\n");
	}
      else
	printf ("* is neither chopped nor correctly rounded.\n");
      if ((RMult == Rounded) && (GMult == No))
	notify ("Multiplication");
    }
  else
    printf ("* is neither chopped nor correctly rounded.\n");
	/*=============================================*/
  Milestone = 45;
	/*=============================================*/
  Y2 = One + U2;
  Y1 = One - U2;
  Z = OneAndHalf + U2 + U2;
  X = Z / Y2;
  T = OneAndHalf - U2 - U2;
  Y = (T - U2) / Y1;
  Z = (Z + U2) / Y2;
  X = X - OneAndHalf;
  Y = Y - T;
  T = T / Y1;
  Z = Z - (OneAndHalf + U2);
  T = (U2 - OneAndHalf) + T;
  if (!((X > Zero) || (Y > Zero) || (Z > Zero) || (T > Zero)))
    {
      X = OneAndHalf / Y2;
      Y = OneAndHalf - U2;
      Z = OneAndHalf + U2;
      X = X - Y;
      T = OneAndHalf / Y1;
      Y = Y / Y1;
      T = T - (Z + U2);
      Y = Y - Z;
      Z = Z / Y2;
      Y1 = (Y2 + U2) / Y2;
      Z = Z - OneAndHalf;
      Y2 = Y1 - Y2;
      Y1 = (F9 - U1) / F9;
      if ((X == Zero) && (Y == Zero) && (Z == Zero) && (T == Zero)
	  && (Y2 == Zero) && (Y2 == Zero) && (Y1 - Half == F9 - Half))
	{
	  RDiv = Rounded;
	  printf ("Division appears to round correctly.\n");
	  if (GDiv == No)
	    notify ("Division");
	}
      else if ((X < Zero) && (Y < Zero) && (Z < Zero) && (T < Zero)
	       && (Y2 < Zero) && (Y1 - Half < F9 - Half))
	{
	  RDiv = Chopped;
	  printf ("Division appears to chop.\n");
	}
    }
  if (RDiv == Other)
    printf ("/ is neither chopped nor correctly rounded.\n");
  BInvrse = One / Radix;
  TstCond (Failure, (BInvrse * Radix - Half == Half),
	   "Radix * ( 1 / Radix ) differs from 1");
	/*=============================================*/
  Milestone = 50;
	/*=============================================*/
  TstCond (Failure, ((F9 + U1) - Half == Half)
	   && ((BMinusU2 + U2) - One == Radix - One),
	   "Incomplete carry-propagation in Addition");
  X = One - U1 * U1;
  Y = One + U2 * (One - U2);
  Z = F9 - Half;
  X = (X - Half) - Z;
  Y = Y - One;
  if ((X == Zero) && (Y == Zero))
    {
      RAddSub = Chopped;
      printf ("Add/Subtract appears to be chopped.\n");
    }
  if (GAddSub == Yes)
    {
      X = (Half + U2) * U2;
      Y = (Half - U2) * U2;
      X = One + X;
      Y = One + Y;
      X = (One + U2) - X;
      Y = One - Y;
      if ((X == Zero) && (Y == Zero))
	{
	  X = (Half + U2) * U1;
	  Y = (Half - U2) * U1;
	  X = One - X;
	  Y = One - Y;
	  X = F9 - X;
	  Y = One - Y;
	  if ((X == Zero) && (Y == Zero))
	    {
	      RAddSub = Rounded;
	      printf ("Addition/Subtraction appears to round correctly.\n");
	      if (GAddSub == No)
		notify ("Add/Subtract");
	    }
	  else
	    printf ("Addition/Subtraction neither rounds nor chops.\n");
	}
      else
	printf ("Addition/Subtraction neither rounds nor chops.\n");
    }
  else
    printf ("Addition/Subtraction neither rounds nor chops.\n");
  S = One;
  X = One + Half * (One + Half);
  Y = (One + U2) * Half;
  Z = X - Y;
  T = Y - X;
  StickyBit = Z + T;
  if (StickyBit != Zero)
    {
      S = Zero;
      BadCond (Flaw, "(X - Y) + (Y - X) is non zero!\n");
    }
  StickyBit = Zero;
  if ((GMult == Yes) && (GDiv == Yes) && (GAddSub == Yes)
      && (RMult == Rounded) && (RDiv == Rounded)
      && (RAddSub == Rounded) && (FLOOR (RadixD2) == RadixD2))
    {
      printf ("Checking for sticky bit.\n");
      X = (Half + U1) * U2;
      Y = Half * U2;
      Z = One + Y;
      T = One + X;
      if ((Z - One <= Zero) && (T - One >= U2))
	{
	  Z = T + Y;
	  Y = Z - X;
	  if ((Z - T >= U2) && (Y - T == Zero))
	    {
	      X = (Half + U1) * U1;
	      Y = Half * U1;
	      Z = One - Y;
	      T = One - X;
	      if ((Z - One == Zero) && (T - F9 == Zero))
		{
		  Z = (Half - U1) * U1;
		  T = F9 - Z;
		  Q = F9 - Y;
		  if ((T - F9 == Zero) && (F9 - U1 - Q == Zero))
		    {
		      Z = (One + U2) * OneAndHalf;
		      T = (OneAndHalf + U2) - Z + U2;
		      X = One + Half / Radix;
		      Y = One + Radix * U2;
		      Z = X * Y;
		      if (T == Zero && X + Radix * U2 - Z == Zero)
			{
			  if (Radix != Two)
			    {
			      X = Two + U2;
			      Y = X / Two;
			      if ((Y - One == Zero))
				StickyBit = S;
			    }
			  else
			    StickyBit = S;
			}
		    }
		}
	    }
	}
    }
  if (StickyBit == One)
    printf ("Sticky bit apparently used correctly.\n");
  else
    printf ("Sticky bit used incorrectly or not at all.\n");
  TstCond (Flaw, !(GMult == No || GDiv == No || GAddSub == No ||
		   RMult == Other || RDiv == Other || RAddSub == Other),
	   "lack(s) of guard digits or failure(s) to correctly round or chop\n\
(noted above) count as one flaw in the final tally below");
	/*=============================================*/
  Milestone = 60;
	/*=============================================*/
  printf ("\n");
  printf ("Does Multiplication commute?  ");
  printf ("Testing on %d random pairs.\n", NoTrials);
  Random9 = SQRT (FLOAT (3));
  Random1 = Third;
  I = 1;
  do
    {
      X = Random ();
      Y = Random ();
      Z9 = Y * X;
      Z = X * Y;
      Z9 = Z - Z9;
      I = I + 1;
    }
  while (!((I > NoTrials) || (Z9 != Zero)));
  if (I == NoTrials)
    {
      Random1 = One + Half / Three;
      Random2 = (U2 + U1) + One;
      Z = Random1 * Random2;
      Y = Random2 * Random1;
      Z9 = (One + Half / Three) * ((U2 + U1) + One) - (One + Half /
						       Three) * ((U2 + U1) +
								 One);
    }
  if (!((I == NoTrials) || (Z9 == Zero)))
    BadCond (Defect, "X * Y == Y * X trial fails.\n");
  else
    printf ("     No failures found in %d integer pairs.\n", NoTrials);
	/*=============================================*/
  Milestone = 70;
	/*=============================================*/
  printf ("\nRunning test of square root(x).\n");
  TstCond (Failure, (Zero == SQRT (Zero))
	   && (-Zero == SQRT (-Zero))
	   && (One == SQRT (One)), "Square root of 0.0, -0.0 or 1.0 wrong");
  MinSqEr = Zero;
  MaxSqEr = Zero;
  J = Zero;
  X = Radix;
  OneUlp = U2;
  SqXMinX (Serious);
  X = BInvrse;
  OneUlp = BInvrse * U1;
  SqXMinX (Serious);
  X = U1;
  OneUlp = U1 * U1;
  SqXMinX (Serious);
  if (J != Zero)
    Pause ();
  printf ("Testing if sqrt(X * X) == X for %d Integers X.\n", NoTrials);
  J = Zero;
  X = Two;
  Y = Radix;
  if ((Radix != One))
    do
      {
	X = Y;
	Y = Radix * Y;
      }
    while (!((Y - X >= NoTrials)));
  OneUlp = X * U2;
  I = 1;
  while (I <= NoTrials)
    {
      X = X + One;
      SqXMinX (Defect);
      if (J > Zero)
	break;
      I = I + 1;
    }
  printf ("Test for sqrt monotonicity.\n");
  I = -1;
  X = BMinusU2;
  Y = Radix;
  Z = Radix + Radix * U2;
  NotMonot = false;
  Monot = false;
  while (!(NotMonot || Monot))
    {
      I = I + 1;
      X = SQRT (X);
      Q = SQRT (Y);
      Z = SQRT (Z);
      if ((X > Q) || (Q > Z))
	NotMonot = true;
      else
	{
	  Q = FLOOR (Q + Half);
	  if (!(I > 0 || Radix == Q * Q))
	    Monot = true;
	  else if (I > 0)
	    {
	      if (I > 1)
		Monot = true;
	      else
		{
		  Y = Y * BInvrse;
		  X = Y - U1;
		  Z = Y + U1;
		}
	    }
	  else
	    {
	      Y = Q;
	      X = Y - U2;
	      Z = Y + U2;
	    }
	}
    }
  if (Monot)
    printf ("sqrt has passed a test for Monotonicity.\n");
  else
    {
      BadCond (Defect, "");
      printf ("sqrt(X) is non-monotonic for X near %s .\n", Y.str());
    }
	/*=============================================*/
  Milestone = 110;
	/*=============================================*/
  printf ("Seeking Underflow thresholds UfThold and E0.\n");
  D = U1;
  if (Precision != FLOOR (Precision))
    {
      D = BInvrse;
      X = Precision;
      do
	{
	  D = D * BInvrse;
	  X = X - One;
	}
      while (X > Zero);
    }
  Y = One;
  Z = D;
  /* ... D is power of 1/Radix < 1. */
  do
    {
      C = Y;
      Y = Z;
      Z = Y * Y;
    }
  while ((Y > Z) && (Z + Z > Z));
  Y = C;
  Z = Y * D;
  do
    {
      C = Y;
      Y = Z;
      Z = Y * D;
    }
  while ((Y > Z) && (Z + Z > Z));
  if (Radix < Two)
    HInvrse = Two;
  else
    HInvrse = Radix;
  H = One / HInvrse;
  /* ... 1/HInvrse == H == Min(1/Radix, 1/2) */
  CInvrse = One / C;
  E0 = C;
  Z = E0 * H;
  /* ...1/Radix^(BIG Integer) << 1 << CInvrse == 1/C */
  do
    {
      Y = E0;
      E0 = Z;
      Z = E0 * H;
    }
  while ((E0 > Z) && (Z + Z > Z));
  UfThold = E0;
  E1 = Zero;
  Q = Zero;
  E9 = U2;
  S = One + E9;
  D = C * S;
  if (D <= C)
    {
      E9 = Radix * U2;
      S = One + E9;
      D = C * S;
      if (D <= C)
	{
	  BadCond (Failure,
		   "multiplication gets too many last digits wrong.\n");
	  Underflow = E0;
	  Y1 = Zero;
	  PseudoZero = Z;
	  Pause ();
	}
    }
  else
    {
      Underflow = D;
      PseudoZero = Underflow * H;
      UfThold = Zero;
      do
	{
	  Y1 = Underflow;
	  Underflow = PseudoZero;
	  if (E1 + E1 <= E1)
	    {
	      Y2 = Underflow * HInvrse;
	      E1 = FABS (Y1 - Y2);
	      Q = Y1;
	      if ((UfThold == Zero) && (Y1 != Y2))
		UfThold = Y1;
	    }
	  PseudoZero = PseudoZero * H;
	}
      while ((Underflow > PseudoZero)
	     && (PseudoZero + PseudoZero > PseudoZero));
    }
  /* Comment line 4530 .. 4560 */
  if (PseudoZero != Zero)
    {
      printf ("\n");
      Z = PseudoZero;
      /* ... Test PseudoZero for "phoney- zero" violates */
      /* ... PseudoZero < Underflow or PseudoZero < PseudoZero + PseudoZero
         ... */
      if (PseudoZero <= Zero)
	{
	  BadCond (Failure, "Positive expressions can underflow to an\n");
	  printf ("allegedly negative value\n");
	  printf ("PseudoZero that prints out as: %s .\n", PseudoZero.str());
	  X = -PseudoZero;
	  if (X <= Zero)
	    {
	      printf ("But -PseudoZero, which should be\n");
	      printf ("positive, isn't; it prints out as  %s .\n", X.str());
	    }
	}
      else
	{
	  BadCond (Flaw, "Underflow can stick at an allegedly positive\n");
	  printf ("value PseudoZero that prints out as %s .\n",
		  PseudoZero.str());
	}
      TstPtUf ();
    }
	/*=============================================*/
  Milestone = 120;
	/*=============================================*/
  if (CInvrse * Y > CInvrse * Y1)
    {
      S = H * S;
      E0 = Underflow;
    }
  if (!((E1 == Zero) || (E1 == E0)))
    {
      BadCond (Defect, "");
      if (E1 < E0)
	{
	  printf ("Products underflow at a higher");
	  printf (" threshold than differences.\n");
	  if (PseudoZero == Zero)
	    E0 = E1;
	}
      else
	{
	  printf ("Difference underflows at a higher");
	  printf (" threshold than products.\n");
	}
    }
  printf ("Smallest strictly positive number found is E0 = %s .\n", E0.str());
  Z = E0;
  TstPtUf ();
  Underflow = E0;
  if (N == 1)
    Underflow = Y;
  I = 4;
  if (E1 == Zero)
    I = 3;
  if (UfThold == Zero)
    I = I - 2;
  UfNGrad = true;
  switch (I)
    {
    case 1:
      UfThold = Underflow;
      if ((CInvrse * Q) != ((CInvrse * Y) * S))
	{
	  UfThold = Y;
	  BadCond (Failure, "Either accuracy deteriorates as numbers\n");
	  printf ("approach a threshold = %s\n", UfThold.str());
	  printf (" coming down from %s\n", C.str());
	  printf
	    (" or else multiplication gets too many last digits wrong.\n");
	}
      Pause ();
      break;

    case 2:
      BadCond (Failure,
	       "Underflow confuses Comparison, which alleges that\n");
      printf ("Q == Y while denying that |Q - Y| == 0; these values\n");
      printf ("print out as Q = %s, Y = %s .\n", Q.str(), Y2.str());
      printf ("|Q - Y| = %s .\n", FABS (Q - Y2).str());
      UfThold = Q;
      break;

    case 3:
      X = X;
      break;

    case 4:
      if ((Q == UfThold) && (E1 == E0) && (FABS (UfThold - E1 / E9) <= E1))
	{
	  UfNGrad = false;
	  printf ("Underflow is gradual; it incurs Absolute Error =\n");
	  printf ("(roundoff in UfThold) < E0.\n");
	  Y = E0 * CInvrse;
	  Y = Y * (OneAndHalf + U2);
	  X = CInvrse * (One + U2);
	  Y = Y / X;
	  IEEE = (Y == E0);
	}
    }
  if (UfNGrad)
    {
      printf ("\n");
      if (setjmp (ovfl_buf))
	{
	  printf ("Underflow / UfThold failed!\n");
	  R = H + H;
	}
      else
	R = SQRT (Underflow / UfThold);
      if (R <= H)
	{
	  Z = R * UfThold;
	  X = Z * (One + R * H * (One + H));
	}
      else
	{
	  Z = UfThold;
	  X = Z * (One + H * H * (One + H));
	}
      if (!((X == Z) || (X - Z != Zero)))
	{
	  BadCond (Flaw, "");
	  printf ("X = %s\n\tis not equal to Z = %s .\n", X.str(), Z.str());
	  Z9 = X - Z;
	  printf ("yet X - Z yields %s .\n", Z9.str());
	  printf ("    Should this NOT signal Underflow, ");
	  printf ("this is a SERIOUS DEFECT\nthat causes ");
	  printf ("confusion when innocent statements like\n");;
	  printf ("    if (X == Z)  ...  else");
	  printf ("  ... (f(X) - f(Z)) / (X - Z) ...\n");
	  printf ("encounter Division by Zero although actually\n");
	  if (setjmp (ovfl_buf))
	    printf ("X / Z fails!\n");
	  else
	    printf ("X / Z = 1 + %s .\n", ((X / Z - Half) - Half).str());
	}
    }
  printf ("The Underflow threshold is %s, below which\n", UfThold.str());
  printf ("calculation may suffer larger Relative error than ");
  printf ("merely roundoff.\n");
  Y2 = U1 * U1;
  Y = Y2 * Y2;
  Y2 = Y * U1;
  if (Y2 <= UfThold)
    {
      if (Y > E0)
	{
	  BadCond (Defect, "");
	  I = 5;
	}
      else
	{
	  BadCond (Serious, "");
	  I = 4;
	}
      printf ("Range is too narrow; U1^%d Underflows.\n", I);
    }
	/*=============================================*/
  Milestone = 130;
	/*=============================================*/
  Y = -FLOOR (Half - TwoForty * LOG (UfThold) / LOG (HInvrse)) / TwoForty;
  Y2 = Y + Y;
  printf ("Since underflow occurs below the threshold\n");
  printf ("UfThold = (%s) ^ (%s)\nonly underflow ", HInvrse.str(), Y.str());
  printf ("should afflict the expression\n\t(%s) ^ (%s);\n",
	  HInvrse.str(), Y2.str());
  printf ("actually calculating yields:");
  if (setjmp (ovfl_buf))
    {
      BadCond (Serious, "trap on underflow.\n");
    }
  else
    {
      V9 = POW (HInvrse, Y2);
      printf (" %s .\n", V9.str());
      if (!((V9 >= Zero) && (V9 <= (Radix + Radix + E9) * UfThold)))
	{
	  BadCond (Serious, "this is not between 0 and underflow\n");
	  printf ("   threshold = %s .\n", UfThold.str());
	}
      else if (!(V9 > UfThold * (One + E9)))
	printf ("This computed value is O.K.\n");
      else
	{
	  BadCond (Defect, "this is not between 0 and underflow\n");
	  printf ("   threshold = %s .\n", UfThold.str());
	}
    }
	/*=============================================*/
  Milestone = 160;
	/*=============================================*/
  Pause ();
  printf ("Searching for Overflow threshold:\n");
  printf ("This may generate an error.\n");
  Y = -CInvrse;
  V9 = HInvrse * Y;
  if (setjmp (ovfl_buf))
    {
      I = 0;
      V9 = Y;
      goto overflow;
    }
  do
    {
      V = Y;
      Y = V9;
      V9 = HInvrse * Y;
    }
  while (V9 < Y);
  I = 1;
overflow:
  Z = V9;
  printf ("Can `Z = -Y' overflow?\n");
  printf ("Trying it on Y = %s .\n", Y.str());
  V9 = -Y;
  V0 = V9;
  if (V - Y == V + V0)
    printf ("Seems O.K.\n");
  else
    {
      printf ("finds a ");
      BadCond (Flaw, "-(-Y) differs from Y.\n");
    }
  if (Z != Y)
    {
      BadCond (Serious, "");
      printf ("overflow past %s\n\tshrinks to %s .\n", Y.str(), Z.str());
    }
  if (I)
    {
      Y = V * (HInvrse * U2 - HInvrse);
      Z = Y + ((One - HInvrse) * U2) * V;
      if (Z < V0)
	Y = Z;
      if (Y < V0)
	V = Y;
      if (V0 - V < V0)
	V = V0;
    }
  else
    {
      V = Y * (HInvrse * U2 - HInvrse);
      V = V + ((One - HInvrse) * U2) * Y;
    }
  printf ("Overflow threshold is V  = %s .\n", V.str());
  if (I)
    printf ("Overflow saturates at V0 = %s .\n", V0.str());
  else
    printf ("There is no saturation value because "
	    "the system traps on overflow.\n");
  V9 = V * One;
  printf ("No Overflow should be signaled for V * 1 = %s\n", V9.str());
  V9 = V / One;
  printf ("                           nor for V / 1 = %s.\n", V9.str());
  printf ("Any overflow signal separating this * from the one\n");
  printf ("above is a DEFECT.\n");
	/*=============================================*/
  Milestone = 170;
	/*=============================================*/
  if (!(-V < V && -V0 < V0 && -UfThold < V && UfThold < V))
    {
      BadCond (Failure, "Comparisons involving ");
      printf ("+-%s, +-%s\nand +-%s are confused by Overflow.",
	      V.str(), V0.str(), UfThold.str());
    }
	/*=============================================*/
  Milestone = 175;
	/*=============================================*/
  printf ("\n");
  for (Indx = 1; Indx <= 3; ++Indx)
    {
      switch (Indx)
	{
	case 1:
	  Z = UfThold;
	  break;
	case 2:
	  Z = E0;
	  break;
	case 3:
	  Z = PseudoZero;
	  break;
	}
      if (Z != Zero)
	{
	  V9 = SQRT (Z);
	  Y = V9 * V9;
	  if (Y / (One - Radix * E9) < Z || Y > (One + Radix * E9) * Z)
	    {			/* dgh: + E9 --> * E9 */
	      if (V9 > U1)
		BadCond (Serious, "");
	      else
		BadCond (Defect, "");
	      printf ("Comparison alleges that what prints as Z = %s\n",
		      Z.str());
	      printf (" is too far from sqrt(Z) ^ 2 = %s .\n", Y.str());
	    }
	}
    }
	/*=============================================*/
  Milestone = 180;
	/*=============================================*/
  for (Indx = 1; Indx <= 2; ++Indx)
    {
      if (Indx == 1)
	Z = V;
      else
	Z = V0;
      V9 = SQRT (Z);
      X = (One - Radix * E9) * V9;
      V9 = V9 * X;
      if (((V9 < (One - Two * Radix * E9) * Z) || (V9 > Z)))
	{
	  Y = V9;
	  if (X < W)
	    BadCond (Serious, "");
	  else
	    BadCond (Defect, "");
	  printf ("Comparison alleges that Z = %s\n", Z.str());
	  printf (" is too far from sqrt(Z) ^ 2 (%s) .\n", Y.str());
	}
    }
	/*=============================================*/
  Milestone = 190;
	/*=============================================*/
  Pause ();
  X = UfThold * V;
  Y = Radix * Radix;
  if (X * Y < One || X > Y)
    {
      if (X * Y < U1 || X > Y / U1)
	BadCond (Defect, "Badly");
      else
	BadCond (Flaw, "");

      printf (" unbalanced range; UfThold * V = %s\n\t%s\n",
	      X.str(), "is too far from 1.\n");
    }
	/*=============================================*/
  Milestone = 200;
	/*=============================================*/
  for (Indx = 1; Indx <= 5; ++Indx)
    {
      X = F9;
      switch (Indx)
	{
	case 2:
	  X = One + U2;
	  break;
	case 3:
	  X = V;
	  break;
	case 4:
	  X = UfThold;
	  break;
	case 5:
	  X = Radix;
	}
      Y = X;
      if (setjmp (ovfl_buf))
	printf ("  X / X  traps when X = %s\n", X.str());
      else
	{
	  V9 = (Y / X - Half) - Half;
	  if (V9 == Zero)
	    continue;
	  if (V9 == -U1 && Indx < 5)
	    BadCond (Flaw, "");
	  else
	    BadCond (Serious, "");
	  printf ("  X / X differs from 1 when X = %s\n", X.str());
	  printf ("  instead, X / X - 1/2 - 1/2 = %s .\n", V9.str());
	}
    }
	/*=============================================*/
  Milestone = 210;
	/*=============================================*/
  MyZero = Zero;
  printf ("\n");
  printf ("What message and/or values does Division by Zero produce?\n");
  printf ("    Trying to compute 1 / 0 produces ...");
  if (!setjmp (ovfl_buf))
    printf ("  %s .\n", (One / MyZero).str());
  printf ("\n    Trying to compute 0 / 0 produces ...");
  if (!setjmp (ovfl_buf))
    printf ("  %s .\n", (Zero / MyZero).str());
	/*=============================================*/
  Milestone = 220;
	/*=============================================*/
  Pause ();
  printf ("\n");
  {
    static const char *msg[] = {
      "FAILUREs  encountered =",
      "SERIOUS DEFECTs  discovered =",
      "DEFECTs  discovered =",
      "FLAWs  discovered ="
    };
    int i;
    for (i = 0; i < 4; i++)
      if (ErrCnt[i])
	printf ("The number of  %-29s %d.\n", msg[i], ErrCnt[i]);
  }
  printf ("\n");
  if ((ErrCnt[Failure] + ErrCnt[Serious] + ErrCnt[Defect] + ErrCnt[Flaw]) > 0)
    {
      if ((ErrCnt[Failure] + ErrCnt[Serious] + ErrCnt[Defect] == 0)
	  && (ErrCnt[Flaw] > 0))
	{
	  printf ("The arithmetic diagnosed seems ");
	  printf ("Satisfactory though flawed.\n");
	}
      if ((ErrCnt[Failure] + ErrCnt[Serious] == 0) && (ErrCnt[Defect] > 0))
	{
	  printf ("The arithmetic diagnosed may be Acceptable\n");
	  printf ("despite inconvenient Defects.\n");
	}
      if ((ErrCnt[Failure] + ErrCnt[Serious]) > 0)
	{
	  printf ("The arithmetic diagnosed has ");
	  printf ("unacceptable Serious Defects.\n");
	}
      if (ErrCnt[Failure] > 0)
	{
	  printf ("Potentially fatal FAILURE may have spoiled this");
	  printf (" program's subsequent diagnoses.\n");
	}
    }
  else
    {
      printf ("No failures, defects nor flaws have been discovered.\n");
      if (!((RMult == Rounded) && (RDiv == Rounded)
	    && (RAddSub == Rounded) && (RSqrt == Rounded)))
	printf ("The arithmetic diagnosed seems Satisfactory.\n");
      else
	{
	  if (StickyBit >= One &&
	      (Radix - Two) * (Radix - Nine - One) == Zero)
	    {
	      printf ("Rounding appears to conform to ");
	      printf ("the proposed IEEE standard P");
	      if ((Radix == Two) &&
		  ((Precision - Four * Three * Two) *
		   (Precision - TwentySeven - TwentySeven + One) == Zero))
		printf ("754");
	      else
		printf ("854");
	      if (IEEE)
		printf (".\n");
	      else
		{
		  printf (",\nexcept for possibly Double Rounding");
		  printf (" during Gradual Underflow.\n");
		}
	    }
	  printf ("The arithmetic diagnosed appears to be Excellent!\n");
	}
    }
  printf ("END OF TEST.\n");
  return 0;
}

template<typename FLOAT>
FLOAT
Paranoia<FLOAT>::Sign (FLOAT X)
{
  return X >= FLOAT (long (0)) ? 1 : -1;
}

template<typename FLOAT>
void
Paranoia<FLOAT>::Pause ()
{
  if (do_pause)
    {
      fputs ("Press return...", stdout);
      fflush (stdout);
      getchar();
    }
  printf ("\nDiagnosis resumes after milestone Number %d", Milestone);
  printf ("          Page: %d\n\n", PageNo);
  ++Milestone;
  ++PageNo;
}

template<typename FLOAT>
void
Paranoia<FLOAT>::TstCond (int K, int Valid, const char *T)
{
  if (!Valid)
    {
      BadCond (K, T);
      printf (".\n");
    }
}

template<typename FLOAT>
void
Paranoia<FLOAT>::BadCond (int K, const char *T)
{
  static const char *msg[] = { "FAILURE", "SERIOUS DEFECT", "DEFECT", "FLAW" };

  ErrCnt[K] = ErrCnt[K] + 1;
  printf ("%s:  %s", msg[K], T);
}

/* Random computes
     X = (Random1 + Random9)^5
     Random1 = X - FLOOR(X) + 0.000005 * X;
   and returns the new value of Random1.  */

template<typename FLOAT>
FLOAT
Paranoia<FLOAT>::Random ()
{
  FLOAT X, Y;

  X = Random1 + Random9;
  Y = X * X;
  Y = Y * Y;
  X = X * Y;
  Y = X - FLOOR (X);
  Random1 = Y + X * FLOAT ("0.000005");
  return (Random1);
}

template<typename FLOAT>
void
Paranoia<FLOAT>::SqXMinX (int ErrKind)
{
  FLOAT XA, XB;

  XB = X * BInvrse;
  XA = X - XB;
  SqEr = ((SQRT (X * X) - XB) - XA) / OneUlp;
  if (SqEr != Zero)
    {
      if (SqEr < MinSqEr)
	MinSqEr = SqEr;
      if (SqEr > MaxSqEr)
	MaxSqEr = SqEr;
      J = J + 1;
      BadCond (ErrKind, "\n");
      printf ("sqrt(%s) - %s  = %s\n", (X * X).str(), X.str(),
	      (OneUlp * SqEr).str());
      printf ("\tinstead of correct value 0 .\n");
    }
}

template<typename FLOAT>
void
Paranoia<FLOAT>::NewD ()
{
  X = Z1 * Q;
  X = FLOOR (Half - X / Radix) * Radix + X;
  Q = (Q - X * Z) / Radix + X * X * (D / Radix);
  Z = Z - Two * X * D;
  if (Z <= Zero)
    {
      Z = -Z;
      Z1 = -Z1;
    }
  D = Radix * D;
}

template<typename FLOAT>
void
Paranoia<FLOAT>::SR3750 ()
{
  if (!((X - Radix < Z2 - Radix) || (X - Z2 > W - Z2)))
    {
      I = I + 1;
      X2 = SQRT (X * D);
      Y2 = (X2 - Z2) - (Y - Z2);
      X2 = X8 / (Y - Half);
      X2 = X2 - Half * X2 * X2;
      SqEr = (Y2 + Half) + (Half - X2);
      if (SqEr < MinSqEr)
	MinSqEr = SqEr;
      SqEr = Y2 - X2;
      if (SqEr > MaxSqEr)
	MaxSqEr = SqEr;
    }
}

template<typename FLOAT>
void
Paranoia<FLOAT>::IsYeqX ()
{
  if (Y != X)
    {
      if (N <= 0)
	{
	  if (Z == Zero && Q <= Zero)
	    printf ("WARNING:  computing\n");
	  else
	    BadCond (Defect, "computing\n");
	  printf ("\t(%s) ^ (%s)\n", Z.str(), Q.str());
	  printf ("\tyielded %s;\n", Y.str());
	  printf ("\twhich compared unequal to correct %s ;\n", X.str());
	  printf ("\t\tthey differ by %s .\n", (Y - X).str());
	}
      N = N + 1;		/* ... count discrepancies. */
    }
}

template<typename FLOAT>
void
Paranoia<FLOAT>::PrintIfNPositive ()
{
  if (N > 0)
    printf ("Similar discrepancies have occurred %d times.\n", N);
}

template<typename FLOAT>
void
Paranoia<FLOAT>::TstPtUf ()
{
  N = 0;
  if (Z != Zero)
    {
      printf ("Since comparison denies Z = 0, evaluating ");
      printf ("(Z + Z) / Z should be safe.\n");
      if (setjmp (ovfl_buf))
	goto very_serious;
      Q9 = (Z + Z) / Z;
      printf ("What the machine gets for (Z + Z) / Z is %s .\n", Q9.str());
      if (FABS (Q9 - Two) < Radix * U2)
	{
	  printf ("This is O.K., provided Over/Underflow");
	  printf (" has NOT just been signaled.\n");
	}
      else
	{
	  if ((Q9 < One) || (Q9 > Two))
	    {
	    very_serious:
	      N = 1;
	      ErrCnt[Serious] = ErrCnt[Serious] + 1;
	      printf ("This is a VERY SERIOUS DEFECT!\n");
	    }
	  else
	    {
	      N = 1;
	      ErrCnt[Defect] = ErrCnt[Defect] + 1;
	      printf ("This is a DEFECT!\n");
	    }
	}
      V9 = Z * One;
      Random1 = V9;
      V9 = One * Z;
      Random2 = V9;
      V9 = Z / One;
      if ((Z == Random1) && (Z == Random2) && (Z == V9))
	{
	  if (N > 0)
	    Pause ();
	}
      else
	{
	  N = 1;
	  BadCond (Defect, "What prints as Z = ");
	  printf ("%s\n\tcompares different from  ", Z.str());
	  if (Z != Random1)
	    printf ("Z * 1 = %s ", Random1.str());
	  if (!((Z == Random2) || (Random2 == Random1)))
	    printf ("1 * Z == %s\n", Random2.str());
	  if (!(Z == V9))
	    printf ("Z / 1 = %s\n", V9.str());
	  if (Random2 != Random1)
	    {
	      ErrCnt[Defect] = ErrCnt[Defect] + 1;
	      BadCond (Defect, "Multiplication does not commute!\n");
	      printf ("\tComparison alleges that 1 * Z = %s\n", Random2.str());
	      printf ("\tdiffers from Z * 1 = %s\n", Random1.str());
	    }
	  Pause ();
	}
    }
}

template<typename FLOAT>
void
Paranoia<FLOAT>::notify (const char *s)
{
  printf ("%s test appears to be inconsistent...\n", s);
  printf ("   PLEASE NOTIFY KARPINKSI!\n");
}

/* ====================================================================== */

int main(int ac, char **av)
{
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);

  while (1)
    switch (getopt (ac, av, "pvg:fdl"))
      {
      case -1:
	return 0;
      case 'p':
	do_pause = true;
	break;
      case 'v':
	verbose = true;
	break;
      case 'g':
	{
	  static const struct {
	    const char *name;
	    const struct real_format *fmt;
	  } fmts[] = {
#define F(x) { #x, &x##_format }
	    F(ieee_single),
	    F(ieee_double),
	    F(ieee_extended_motorola),
	    F(ieee_extended_intel_96),
	    F(ieee_extended_intel_128),
	    F(ibm_extended),
	    F(ieee_quad),
	    F(vax_f),
	    F(vax_d),
	    F(vax_g),
	    F(i370_single),
	    F(i370_double),
	    F(c4x_single),
	    F(c4x_extended),
	    F(real_internal),
#undef F
	  };

	  int i, n = sizeof (fmts)/sizeof(*fmts);

	  for (i = 0; i < n; ++i)
	    if (strcmp (fmts[i].name, optarg) == 0)
	      break;

	  if (i == n)
	    {
	      printf ("Unknown implementation \"%s\"; "
		      "available implementations:\n", optarg);
	      for (i = 0; i < n; ++i)
		printf ("\t%s\n", fmts[i].name);
	      return 1;
	    }

	  // We cheat and use the same mode all the time, but vary
	  // the format used for that mode.
	  real_format_for_mode[int(real_c_float::MODE) - int(QFmode)]
	    = fmts[i].fmt;

	  Paranoia<real_c_float>().main();
	  break;
	}

      case 'f':
	Paranoia < native_float<float> >().main();
	break;
      case 'd':
	Paranoia < native_float<double> >().main();
	break;
      case 'l':
#ifndef NO_LONG_DOUBLE
	Paranoia < native_float<long double> >().main();
#endif
	break;

      case '?':
	puts ("-p\tpause between pages");
	puts ("-g<FMT>\treal.c implementation FMT");
	puts ("-f\tnative float");
	puts ("-d\tnative double");
	puts ("-l\tnative long double");
	return 0;
      }
}

/* GCC stuff referenced by real.o.  */

extern "C" void
fancy_abort ()
{
  abort ();
}

int target_flags = 0;

extern "C" int
floor_log2_wide (unsigned HOST_WIDE_INT x)
{
  int log = -1;
  while (x != 0)
    log++,
    x >>= 1;
  return log;
}
