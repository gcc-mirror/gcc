/* Test for printf formats.  Formats using C99 features should be rejected
   outside of C99 mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ ptrdiff_t;

__extension__ typedef long long int llong;

/* This next definition is a kludge.  When GCC has a <stdint.h> it
   should be used.
*/
/* (T *) if E is zero, (void *) otherwise.  */
#define type_if_not(T, E) __typeof__(0 ? (T *)0 : (void *)(E))

/* (T *) if E is nonzero, (void *) otherwise.  */
#define type_if(T, E) type_if_not(T, !(E))

/* Combine pointer types, all but one (void *).  */
#define type_comb2(T1, T2) __typeof__(0 ? (T1)0 : (T2)0)
#define type_comb3(T1, T2, T3) type_comb2(T1, type_comb2(T2, T3))

#define maybe_int_ptr type_if(int, sizeof(int) == sizeof(llong))
#define maybe_long_ptr type_if(long, sizeof(long) == sizeof(llong) && sizeof(long) > sizeof(int))
#define maybe_long_long_ptr type_if(llong, sizeof(llong) > sizeof(long))

#define intmax_type_ptr type_comb3(maybe_int_ptr, maybe_long_ptr, maybe_long_long_ptr)

typedef __typeof__(*((intmax_type_ptr)0)) intmax_t;

extern int printf (const char *, ...);

void
foo (int i, double d, llong ll, intmax_t j, size_t z, ptrdiff_t t)
{
  /* Some tests already in c90-printf-1.c, e.g. %lf.  */
  /* The widths hh, ll, j, z, t are new.  */
  printf ("%hhd", i); /* { dg-warning "length|C" "%hh in C90" } */
  printf ("%lld", ll); /* { dg-warning "length|C" "%ll in C90" } */
  printf ("%jd", j); /* { dg-warning "length|C" "%j in C90" } */
  printf ("%zu", z); /* { dg-warning "length|C" "%z in C90" } */
  printf ("%td", t); /* { dg-warning "length|C" "%t in C90" } */
  /* The formats F, a, A are new.  */
  printf ("%F", d); /* { dg-warning "C" "%F in C90" } */
  printf ("%a", d); /* { dg-warning "C" "%a in C90" } */
  printf ("%A", d); /* { dg-warning "C" "%A in C90" } */
}
