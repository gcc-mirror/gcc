/* Verify that DECL_INLINE gets copied between DECLs properly.  */
/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-not "xyzzy" } } */

/* Test that declaration followed by definition inlines.  */
static inline int xyzzy0 (int);
static int xyzzy0 (int x) { return x; }
int test0 (void)
{
  return xyzzy0 (5);
}

/* Test that definition following declaration inlines.  */
static int xyzzy1 (int);
static inline int xyzzy1 (int x) { return x; }
int test1 (void)
{
  return xyzzy1 (5);
}

/* Test that redeclaration inside a function body inlines.  */
extern inline int xyzzy2 (int x) { return x; }
int test2 (void)
{
  extern int xyzzy2 (int);
  return xyzzy2 (5);
}
