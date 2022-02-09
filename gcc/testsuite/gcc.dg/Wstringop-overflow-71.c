/* PR tree-optimization/97027 - missing warning on buffer overflow storing
   a larger scalar into a smaller array
   Verify warnings for overflow by stores of results of built-in functions.
   { dg-do compile }
   { dg-options "-O2" }
   { dg-require-effective-target alloca } */

typedef __INT16_TYPE__ int16_t;
typedef __SIZE_TYPE__  size_t;

extern int abs (int);

extern void* alloca (size_t);

extern double nan (const char *);

#ifdef __DEC32_MAX__
  _Decimal32 nand32 (const char *);
#else
/* _Decimal32 is supported only conditionally and not available on all
   targets.  */
#  define _Decimal32  double
#  define nand32(s)   nan (s)
#endif

extern size_t strlen (const char *);
extern char* strcpy (char *, const char *);


extern unsigned char ax[], a1[1], a2[2], a8[8];


void nowarn_abs (int i)
{
  *(int *)ax = abs (i);
  *(char *)a1 = abs (i);
}

void warn_abs (int i)
{
  *(int *)a1 = abs (i);         // { dg-warning "\\\[-Wstringop-overflow" }
}


void nowarn_alloca (size_t n)
{
  *(void **)ax = alloca (n);
}

void warn_alloca (size_t n)
{
  *(void **)a1 = alloca (n);    // { dg-warning "\\\[-Wstringop-overflow" }
}


void nowarn_complex (double x, double i)
{
  *(_Complex double *)ax = __builtin_complex (x, i);
}

void warn_complex (double x, double i)
{
  _Complex double *p = (_Complex double *)a1;
  *p = __builtin_complex (x, i);  // { dg-warning "\\\[-Wstringop-overflow" "pr101455" { xfail *-*-* } }
}


__attribute__ ((noipa)) void nowarn_nan (const char *s)
{
  *(double *)ax = nan (s);
}

__attribute__ ((noipa)) void warn_nan (const char *s)
{
  *(double *)a1 = nan (s);      // { dg-warning "\\\[-Wstringop-overflow" }
}


__attribute__ ((noipa)) void nowarn_nand32 (const char *s)
{
  *(_Decimal32 *)ax = nand32 (s);
}

__attribute__ ((noipa)) void warn_nand32 (const char *s)
{
  *(_Decimal32 *)a1 = nand32 (s); // { dg-warning "\\\[-Wstringop-overflow" }
}


void nowarn_strlen (const char *s1, const char *s2, const char *s3)
{
  *(char *)ax = strlen (s1);
  *(char *)a1 = strlen (s2);
  *(size_t *)a8 = strlen (s3);
}

void warn_strlen (const char *s1, const char *s2)
{
  *(int16_t *)a1 = strlen (s1); // { dg-warning "\\\[-Wstringop-overflow" }
  *(size_t *)a2 = strlen (s2);  // { dg-warning "\\\[-Wstringop-overflow" "!ptr_eq_short" { target { ! ptr_eq_short } } }
}


void nowarn_strcpy (char *s1, char *s2, const char *s3)
{
  *(char **)ax = strcpy (s1, s2);
  *(char **)a8 = strcpy (s2, s3);
}

void warn_strcpy (char *s1, char *s2, const char *s3)
{
  *(char **)a1 = strcpy (s1, s2);   // { dg-warning "\\\[-Wstringop-overflow" }
  *(char **)a2 = strcpy (s2, s3);   // { dg-warning "\\\[-Wstringop-overflow" "!ptr_eq_short" { target { ! ptr_eq_short } } }
}
