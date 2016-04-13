/* { dg-do compile } */

_Complex float
test1 (_Complex float f)
{
  __asm__ ("" : "+r" (__real f));
  return f;
}

_Complex float
test2 (_Complex float f)
{
  __asm__ ("" : "=r" (__real f));
  return f;
}

struct X { int i; };

struct X 
test3 (struct X x)
{
  __asm__ ("" : "=r" (x.i));
  return x;
}

struct X
test4 (struct X x)
{
  __asm__ ("" : "+r" (x.i));
  return x;
}

struct X 
test5 (struct X x)
{
  __asm__ ("" : "=r" (x));
  return x;
}

struct X
test6 (struct X x)
{
  __asm__ ("" : "+r" (x));
  return x;
}
