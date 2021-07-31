/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-march=core2 -mregparm=3" { target ia32 } } */

int foo(int x)
{
  x--;
  if (x == -1)
    x = 16;
  return x;
}

int bar(int x)
{
  if (x == 0)
    x = 16;
  else x--;
  return x;
}

#ifdef __x86_64__
long long fool(long long x)
{
  x--;
  if (x == -1)
    x = 16;
  return x;
}

long long barl(long long x)
{
  if (x == 0)
    x = 16;
  else x--;
  return x;
}
#endif

short foos(short x)
{
  x--;
  if (x == -1)
    x = 16;
  return x;
}

short bars(short x)
{
  if (x == 0)
    x = 16;
  else x--;
  return x;
}

/* { dg-final { scan-assembler-not "lea(l|q)" } } */
/* { dg-final { scan-assembler-not "test(l|q|w)" } } */

