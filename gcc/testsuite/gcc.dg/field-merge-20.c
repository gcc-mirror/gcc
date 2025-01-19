/* { dg-do run } */
/* { dg-options "-O1" } */

/* tree-optimization/118409 */

/* Check that tests involving a sign bit of a storage unit are handled
   correctly.  The compares are turned into xor tests by earlier passes, and
   ifcombine has to propagate the sign bit mask to the right hand of the
   compare extracted from the xor, otherwise we'll retain unwanted bits for the
   compare.  */

typedef struct {
    int p : __CHAR_BIT__;
    int d : 1;
    int b : __CHAR_BIT__ - 2;
    int e : 1;
} g;

g a = {.d = 1, .e = 1}, c = {.b = 1, .d = 1, .e = 1};

__attribute__((noipa))
int f1 ()
{
  if (a.d == c.d
      && a.e == c.e)
    return 0;
  return -1;
}

__attribute__((noipa))
int f2 ()
{
  if (a.d != c.d
      || a.e != c.e)
    return -1;
  return 0;
}

__attribute__((noipa))
int f3 ()
{
  if (c.d == a.d
      && c.e == a.e)
    return 0;
  return -1;
}

__attribute__((noipa))
int f4 ()
{
  if (c.d != a.d
      || c.e != a.e)
    return -1;
  return 0;
}

int main() {
  if (f1 () < 0
      || f2 () < 0
      || f3 () < 0
      || f4 () < 0)
    __builtin_abort();
  return 0;
}
