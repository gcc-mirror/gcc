/* PR middle-end/15345, c/16450 */
/* Test whether unused nested functions aren't emitted into the assembly.  */
/* { dg-do compile } */
/* { dg-options "-g0" } */

int
fn1 (int x)
{
  int i = x;
  inline __attribute__((always_inline)) int
  should_not_appear1 (void)
  {
    return i;
  }
  return should_not_appear1 ();
}

int
fn2 (int x)
{
  int i = x;
  inline __attribute__((always_inline)) int
  should_not_appear2 (void)
  {
    return i;
  }
  return x;
}

extern void check (void *p);

int
fn3 (int x)
{
  int i = x;
  inline int
  should_appear1 (void)
  {
    char *p = __builtin_alloca (i);
    check (p);
    return i;
  }
  return should_appear1 ();
}

int
fn4 (int x)
{
  int i = x;
  inline int
  should_not_appear3 (void)
  {
    char *p = __builtin_alloca (i);
    check (p);
    return i;
  }
  return 0 ? should_not_appear3 () : 1;
}

int
fn5 (int x)
{
  int i = x;
  inline int
  should_appear2 (void)
  {
    return i;
  }
  check (should_appear2);
  return i;
}

/* { dg-final { scan-assembler-not "should_not_appear" } } */
