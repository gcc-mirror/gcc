/* PR middle-end/80364 - sanitizer detects signed integer overflow
   in gimple-ssa-sprintf.c
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow=1 -ftrack-macro-expansion=0" }
   { dg-require-effective-target int32plus } */

typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;

void sink (void*);
void* get_value (void);

/* Return a random width as type T.  */
#define W(T) *(T*)get_value ()

/* Return a random precision as type T.  */
#define P(T) *(T*)get_value ()

/* Return a random value as type T.  */
#define V(T) *(T*)get_value ()

extern char buf[1];

/* Test convenience macro.  */
#define T(fmt, ...)					\
  __builtin_sprintf (buf + 1, fmt, __VA_ARGS__);	\
  sink (buf)

typedef signed char         schar_t;
typedef unsigned char       uchar_t;
typedef signed short        sshort_t;
typedef unsigned short      ushort_t;
typedef signed int          sint_t;
typedef unsigned int        uint_t;
typedef signed long         slong_t;
typedef unsigned long       ulong_t;
typedef signed long long    sllong_t;
typedef unsigned long long  ullong_t;

#if __SIZEOF_INT128__
typedef __int128_t          sint128_t;
typedef __uint128_t         uint128_t;
#else
/* When __int128_t is not available, repeat the same tests with long long.
   This is to avoid having to guard the tests below and to avoid making
   the dg-warning directives conditional.  */
typedef signed long long    sint128_t;
typedef unsigned long long  uint128_t;
#endif

const sint128_t sint128_max
  = (sint128_t)1 << (sizeof sint128_max * __CHAR_BIT__ - 2);
const sint128_t uint128_max = (uint128_t)-1;

void test_width_cst (void)
{
  T ("%*i", W (schar_t), 1);     /* { dg-warning "between 1 and 128 " } */
  T ("%*i", W (uchar_t), 12);    /* { dg-warning "between 2 and 255 " } */

  T ("%*i", W (sshort_t), 123);  /* { dg-warning "between 3 and 32768 " } */
  T ("%*i", W (ushort_t), 1234); /* { dg-warning "between 4 and 65535 " } */

  T ("%*i", W (sint_t), 12345);  /* { dg-warning "between 5 and 2147483648 " } */
  T ("%*i", W (uint_t), 123456); /* { dg-warning "between 6 and 2147483648 " } */

  /* Exercise calls with invalid arguments (to verify there is no ICE).  */
  T ("%*li", W (slong_t), 1234567L);  /* { dg-warning "between 7 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%*li", W (ulong_t), 12345678L); /* { dg-warning "between 8 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%*lli", W (sllong_t), 123456789LL);  /* { dg-warning "between 9 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%*lli", W (ullong_t), 1234567890LL); /* { dg-warning "between 10 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%*i", W (sint128_t), 0);  /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%*i", W (uint128_t), 1); /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  {
    extern sint128_t si128;
    if (si128 < sint128_max / 2 || sint128_max - 8 < si128)
      si128 = sint128_max / 2;

    T ("%*i", si128, 0);  /* { dg-warning "between 1 and 2147483648 " } */
    /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

    extern uint128_t ui128;
    if (ui128 < uint128_max / 2 || uint128_max - 8 < ui128)
      ui128 = uint128_max / 2;

    T ("%*i", ui128, 0);  /* { dg-warning "between 1 and 2147483648 " } */
    /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  }

  T ("%*i", W (float), 2);  /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%*i", W (double), 3); /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
}

void test_width_var (void)
{
  T ("%*i", W (schar_t), V (schar_t));     /* { dg-warning "between 1 and 128 " } */
  T ("%*i", W (uchar_t), V (uchar_t));    /* { dg-warning "between 1 and 255 " } */

  T ("%*i", W (sshort_t), V (sshort_t));  /* { dg-warning "between 1 and 32768 " } */
  T ("%*i", W (ushort_t), V (ushort_t)); /* { dg-warning "between 1 and 65535 " } */

  T ("%*i", W (sint_t), V (sint_t));  /* { dg-warning "between 1 and 2147483648 " } */
  T ("%*i", W (uint_t), V (uint_t)); /* { dg-warning "between 1 and 2147483648 " } */

  /* Exercise calls with invalid arguments (to verify there is no ICE).  */
  T ("%*li", W (slong_t), V (slong_t));  /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%*li", W (ulong_t), V (ulong_t)); /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%*lli", W (sllong_t), V (sllong_t));  /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%*lli", W (ullong_t), V (ullong_t)); /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%*i", W (float), V (int));  /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%*i", W (double), V (int)); /* { dg-warning "between 1 and 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  {
    /* Create an unsigned range with a lower bound greater than 1 and
       an upper bound in excess of INT_MAX and verify that the lower
       bound isn't used as the minimum output (since the excessive
       upper bound wraps around zero).  It's possible to constrain
       the upper bound on the output more, based on the upper bound
       of the width here, but not worth the trouble.  */
    extern unsigned w;
    if (w < 5 || (unsigned)-1 - 7 < w)
      w = 5;

    T ("%*u", w, V (int));   /* { dg-warning "between 1 and 2147483648 " } */
  }

  {
    /* Verify that enums are correctly handled (i.e., that the warning
       doesn't just test for TREE_CODE(type) == INTEGER_TYPE but instead
       uses INTEGRAL_TYPE_P() or some equivalent.  */
    enum WidthEnum { e7 = 7, e9 = 9 };
    enum WidthEnum w = V (enum WidthEnum);
    if (w < e7 || e9 < w)
      w = e7;

    T ("%*hu", w, V (int));   /* { dg-warning "between 7 and 9 " } */
  }
}

void test_precision_cst (void)
{
  T ("%.*i", P (schar_t), 1);     /* { dg-warning "between 1 and 127 " } */
  T ("%.*i", P (uchar_t), 12);    /* { dg-warning "between 2 and 255 " } */

  T ("%.*i", P (sshort_t), 123);  /* { dg-warning "between 3 and 32767 " } */
  T ("%.*i", P (ushort_t), 1234); /* { dg-warning "between 4 and 65535 " } */

  T ("%.*i", P (sint_t), 12345);  /* { dg-warning "between 5 and 2147483647 " } */
  T ("%.*i", P (uint_t), 123456); /* { dg-warning "between 6 and 2147483647 " } */

  /* Exercise calls with invalid arguments (to verify there is no ICE).  */
  T ("%.*li", P (slong_t), 1234567L);  /* { dg-warning "between 7 and 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%.*li", P (ulong_t), 12345678L); /* { dg-warning "between 8 and 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%.*lli", P (sllong_t), 123456789LL);  /* { dg-warning "between 9 and 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%.*lli", P (ullong_t), 1234567890LL); /* { dg-warning "between 10 and 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%.*i", P (sint128_t), 0);  /* { dg-warning "up to 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%.*i", P (uint128_t), 1); /* { dg-warning "between 1 and 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  {
    extern sint128_t si128;
    if (si128 < sint128_max / 2 || sint128_max - 8 < si128)
      si128 = sint128_max / 2;

    T ("%.*i", si128, 0);  /* { dg-warning "up to 2147483647 " } */
    /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

    extern uint128_t ui128;
    if (ui128 < uint128_max / 2 || uint128_max - 8 < ui128)
      ui128 = uint128_max / 2;

    T ("%.*i", ui128, 0);  /* { dg-warning "up to 2147483647 " } */
    /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  }

  T ("%.*i", P (float), 0);  /* { dg-warning "up to 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%.*i", P (double), 1); /* { dg-warning "between 1 and 2147483647 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
}

void test_precision_var (void)
{
  T ("%.*i", P (schar_t), V (schar_t));     /* { dg-warning "up to 128 " } */
  T ("%.*i", P (uchar_t), V (uchar_t));    /* { dg-warning "up to 255 " } */

  T ("%.*i", P (sshort_t), V (sshort_t));  /* { dg-warning "up to 32768 " } */
  T ("%.*i", P (ushort_t), V (ushort_t)); /* { dg-warning "up to 65535 " } */

  T ("%.*i", P (sint_t), V (sint_t));  /* { dg-warning "up to 2147483648 " } */
  T ("%.*i", P (uint_t), V (uint_t)); /* { dg-warning "up to 2147483648 " } */

  /* Exercise calls with invalid arguments (to verify there is no ICE).  */
  T ("%.*li", P (slong_t), V (slong_t));  /* { dg-warning "up to 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%.*li", P (ulong_t), V (ulong_t)); /* { dg-warning "up to 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%.*lli", P (sllong_t), V (sllong_t));  /* { dg-warning "up to 2147483648" } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%.*lli", P (ullong_t), V (ullong_t)); /* { dg-warning "up to 2147483648" } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  T ("%.*i", P (float), V (int));  /* { dg-warning "up to 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */
  T ("%.*i", P (double), V (int)); /* { dg-warning "up to 2147483648 " } */
  /* { dg-warning "expects argument of type .int." "" { target *-*-* } .-1 } */

  {
    /* Similar to the corresponding width case, create an unsigned range
       with a lower bound greater than 1 and an upper bound in excess of
       INT_MAX and verify that the lower bound isn't used as the minimum
       output (since the excessive upper bound wraps around zero).  */
    extern unsigned p;
    if (p < 7 || (unsigned)-1 - 9 < p)
      p = 7;

    T ("%.*u", p, V (int));   /* { dg-warning "up to 2147483647 " } */
  }

  {
    /* Verify that enums are correctly handled.  */
    enum PrecEnum { e9 = 9, e17 = 17 };
    enum PrecEnum p = V (enum PrecEnum);
    if (p < e9 || e17 < p)
      p = e9;

    T ("%.*u", p, V (int));   /* { dg-warning "between 9 and 17 " } */
  }
}
