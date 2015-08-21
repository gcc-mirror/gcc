/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-std=c99 -pedantic-errors" } */

/* Test ## behavior and corner cases thoroughly.  The macro expander
   failed many of these during development.  */

#ifndef __WCHAR_TYPE__
#define __WCHAR_TYPE__ int
#endif
typedef __WCHAR_TYPE__ wchar_t;

extern int strcmp (const char *, const char *);
#if DEBUG
extern int puts (const char *);
#else
#define puts(X)
#endif
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)

#define EMPTY
#define str(x) #x
#define xstr(x) str(x)
#define glue(x, y) x ## y
#define xglue(x, y) glue (x, y)
#define glue3(x, y, z) x ## y ## z
#define glue_var(x, ...) x ## __VA_ARGS__

#define __muldi3 __NDW(mul, 3 = 50)
#define __NDW(a,b) __ ## a ## di ## b
#define m3 NDW()
#define NDW(x) m3 ## x = 50
#define five 5
#define fifty int fif ## ty

/* Defines a function called glue, returning what it is passed.  */
int glue (glue,) (int x)
{
  return x;
}

int main ()
{
  /* m3 and __muldi3 would sometimes cause an infinite loop.  Ensure
     we only expand fifty once.  */
  fifty = 50, m3, __muldi3;

  /* General glue and macro expanding test.  */
  int five0 = xglue (glue (fi, ve), 0);

  /* Tests only first and last tokens are pasted, and pasting to form
     the != operator.  Should expand to: if (five0 != 50).  */
  if (glue3 (fi, ve0 !,= glue (EMPTY 5, 0)))
    err ("five0 != 50");

  /* Test varags pasting, and pasting to form the >> operator.  */
  if (glue_var(50 >, > 1 != 25))
    err ("Operator >> pasting");

  /* The LHS should not attempt to expand twice, and thus becomes a
     call to the function glue.  */
  if (glue (gl, ue) (12) != 12)
    err ("Recursive macros");

  /* Test placemarker pasting.  The glued lines should all appear
     neatly in the same column and below each other, though we don't
     test that here.  */
  {
    int glue3(a, b, ) = 1, glue3(a,,) = 1;
    glue3(a, , b)++;
    glue3(, a, b)++;
    glue3(,a,)++;
    glue3(,,a)++;
    if (a != 3 || ab != 3 glue3(,,))
      err ("Placemarker pasting");
  }

  /* Test that macros in arguments are not expanded.  */
  {
    int glue (EMPTY,1) = 123, glue (T, EMPTY) = 123;
    if (EMPTY1 != 123 || TEMPTY != 123)
      err ("Pasted arguments macro expanding");
  }

  /* Test various paste combinations.  */
  {
    const wchar_t* wc_array = glue(L, "wide string");
    wchar_t wc = glue(L, 'w');
    const char * hh = xstr(xglue(glue(%, :), glue(%, :)));
    int array glue (<, :) 1 glue (:, >) = glue(<, %) 1 glue(%, >);
    int x = 4;

    if (array[0] != 1)
      err ("Digraph pasting");

    x glue (>>, =) 1;		/* 2 */
    x glue (<<, =) 1;		/* 4 */
    x glue (*, =) 2;		/* 8 */
    x glue (+, =) 100;		/* 108 */
    x glue (-, =) 50;		/* 58 */
    x glue (/, =) 2;		/* 29 */
    x glue (%, =) 20;		/* 9 */
    x glue (&, =) 254;		/* 8 */
    x glue (|, =) 16;		/* 24 */
    x glue (^, =) 18;		/* 10 */
    
    if (x != 10 || 0 glue (>, =) 1 glue (|, |) 1 glue (<, =) 0)
      err ("Various operator pasting");
    if (strcmp (hh, "%:%:"))
      err ("Pasted digraph spelling");
    if ((glue (., 1) glue (!, =) .1))
      err ("Pasted numbers 1");
    /* glue3 here will only work if we paste left-to-right.  If a
       future implementation does not do this, change the test.  */
    if (glue3 (1.0e, +, 1) != 10.0)
      err ("Pasted numbers 2");
  }

  return 0;
}
