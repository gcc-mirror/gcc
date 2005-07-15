/* { dg-do compile } */
/* { dg-options -O2 } */

/* Basic tests for flatten attribute, check we end up
   with only the flattened function bodies.  */

static int foobar(int i);
static int bar(int i);

int __attribute__((flatten)) leaf0a(int i)
{
  return bar(i);
}
int __attribute__((flatten)) leaf0b(int i)
{
  return foobar(i);
}
int __attribute__((flatten)) leaf1(int i)
{
  return bar(foobar(i));
}
int __attribute__((flatten)) leaf2(int i)
{
  int j;
  j = foobar(i);
  return bar(j);
}

static int foobar(int i)
{
  return i-1;
}
static int bar(int i)
{
  return i + foobar(i);
}


static int g(int i)
{
  return i*5+1;
}
static int f(int i)
{
  return g(i);
}
int __attribute__((flatten)) leaf3(int i)
{
  int j;
  j = f(i);
  j += f(i);
  return j;
}

/* { dg-final { scan-assembler-not "g:" } } */
/* { dg-final { scan-assembler-not "f:" } } */
/* { dg-final { scan-assembler-not "bar:" } } */
