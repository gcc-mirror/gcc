/* Test for _Bool and <stdbool.h> in C99.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* _Bool must be a builtin type.  */

_Bool foo;

#include <stdbool.h>

/* Three macros must be integer constant expressions suitable for use
   in #if.
*/

#if !defined(true) || (true != 1)
#error "bad stdbool true" /* { dg-bogus "#error" "bad stdbool.h" } */
#endif

#if !defined(false) || (false != 0)
#error "bad stdbool false" /* { dg-bogus "#error" "bad stdbool.h" } */
#endif

#if !defined(__bool_true_false_are_defined) || (__bool_true_false_are_defined != 1)
#error "bad stdbool __bool_true_false_are_defined" /* { dg-bogus "#error" "bad stdbool.h" } */
#endif

int a = true;
int b = false;
int c = __bool_true_false_are_defined;

struct foo
{
  _Bool a : 1;
  _Bool b : 2;
  _Bool c : 7;
} sf;

#define str(x) xstr(x)
#define xstr(x) #x


extern void abort (void);
extern void exit (int);
extern int strcmp (const char *, const char *);

int
main (void)
{
  /* The macro `bool' must expand to _Bool.  */
  const char *t = str (bool);
  _Bool u, v;
  if (strcmp (t, "_Bool"))
    abort ();
  if (a != 1 || b != 0 || c != 1)
    abort ();
  /* Casts to _Bool have a specified behaviour.  */
  if ((int)(_Bool)2 != 1)
    abort ();
  if ((int)(_Bool)0.2 != 1)
    abort ();
  /* Pointers may be assigned to _Bool.  */
  if ((u = t) != 1)
    abort ();
  /* _Bool may be used to subscript arrays.  */
  u = 0;
  if (t[u] != '_')
    abort ();
  if (u[t] != '_')
    abort ();
  u = 1;
  if (t[u] != 'B')
    abort ();
  if (u[t] != 'B')
    abort ();
  /* Test increment and decrement operators.  */
  u = 0;
  if (u++ != 0)
    abort ();
  if (u != 1)
    abort ();
  if (u++ != 1)
    abort ();
  if (u != 1)
    abort ();
  u = 0;
  if (++u != 1)
    abort ();
  if (u != 1)
    abort ();
  if (++u != 1)
    abort ();
  if (u != 1)
    abort ();
  u = 0;
  if (u-- != 0)
    abort ();
  if (u != 1)
    abort ();
  if (u-- != 1)
    abort ();
  if (u != 0)
    abort ();
  u = 0;
  if (--u != 1)
    abort ();
  if (u != 1)
    abort ();
  if (--u != 0)
    abort ();
  if (u != 0)
    abort ();
  /* Test unary + - ~ !.  */
  u = 0;
  if (+u != 0)
    abort ();
  if (-u != 0)
    abort ();
  u = 1;
  if (+u != 1)
    abort ();
  if (-u != -1)
    abort ();
  u = 2;
  if (+u != 1)
    abort ();
  if (-u != -1)
    abort ();
  u = 0;
  if (~u != ~(int)0)
    abort ();
  u = 1;
  if (~u != ~(int)1)
    abort ();
  u = 0;
  if (!u != 1)
    abort ();
  u = 1;
  if (!u != 0)
    abort ();
  /* Test arithmetic * / % + - (which all apply promotions).  */
  u = 0;
  if (u + 2 != 2)
    abort ();
  u = 1;
  if (u * 4 != 4)
    abort ();
  if (u % 3 != 1)
    abort ();
  if (u / 1 != 1)
    abort ();
  if (4 / u != 4)
    abort ();
  if (u - 7 != -6)
    abort ();
  /* Test bitwise shift << >>.  */
  u = 1;
  if (u << 1 != 2)
    abort ();
  if (u >> 1 != 0)
    abort ();
  /* Test relational and equality operators < > <= >= == !=.  */
  u = 0;
  v = 0;
  if (u < v || u > v || !(u <= v) || !(u >= v) || !(u == v) || u != v)
    abort ();
  u = 0;
  v = 1;
  if (!(u < v) || u > v || !(u <= v) || u >= v || u == v || !(u != v))
    abort ();
  /* Test bitwise operators & ^ |.  */
  u = 1;
  if ((u | 2) != 3)
    abort ();
  if ((u ^ 3) != 2)
    abort ();
  if ((u & 1) != 1)
    abort ();
  if ((u & 0) != 0)
    abort ();
  /* Test logical && ||.  */
  u = 0;
  v = 1;
  if (!(u || v))
    abort ();
  if (!(v || u))
    abort ();
  if (u && v)
    abort ();
  if (v && u)
    abort ();
  u = 1;
  v = 1;
  if (!(u && v))
    abort ();
  /* Test conditional ? :.  */
  u = 0;
  if ((u ? 4 : 7) != 7)
    abort ();
  u = 1;
  v = 0;
  if ((1 ? u : v) != 1)
    abort ();
  if ((1 ? 4 : u) != 4)
    abort ();
  /* Test assignment operators = *= /= %= += -= <<= >>= &= ^= |=.  */
  if ((u = 2) != 1)
    abort ();
  if (u != 1)
    abort ();
  if ((u *= -1) != 1)
    abort ();
  if (u != 1)
    abort ();
  if ((u /= 2) != 0)
    abort ();
  if ((u += 3) != 1)
    abort ();
  if ((u -= 1) != 0)
    abort ();
  u = 1;
  if ((u <<= 4) != 1)
    abort ();
  if ((u >>= 1) != 0)
    abort ();
  u = 1;
  if ((u &= 0) != 0)
    abort ();
  if ((u |= 2) != 1)
    abort ();
  if ((u ^= 3) != 1)
    abort ();
  /* Test comma expressions.  */
  u = 1;
  if ((4, u) != 1)
    abort ();
  /* Test bitfields.  */
  {
    int i;
    for (i = 0; i < sizeof (struct foo); i++)
      *((unsigned char *)&sf + i) = (unsigned char) -1;
    sf.a = 1;
    if (sf.a != 1)
      abort ();
    sf.b = 1;
    if (sf.b != 1)
      abort ();
    sf.c = 1;
    if (sf.c != 1)
      abort ();
    sf.a = 0;
    if (sf.a != 0)
      abort ();
    sf.b = 0;
    if (sf.b != 0)
      abort ();
    sf.c = 0;
    if (sf.c != 0)
      abort ();
  }
  exit (0);
}
