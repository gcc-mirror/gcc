/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-march=pentiumpro -mregparm=3" { target ia32 } } */

int foo_m1(int x)
{
  x--;
  if (x == 0)
    x = 16;
  return x;
}

int foo_m2(int x)
{
  x -= 2;
  if (x == 0)
    x = 16;
  return x;
}

int foo_p1(int x)
{
  x++;
  if (x == 0)
    x = 16;
  return x;
}

int foo_p2(int x)
{
  x += 2;
  if (x == 0)
    x = 16;
  return x;
}


#ifdef __x86_64__
long long fool_m1(long long x)
{
  x--;
  if (x == 0)
    x = 16;
  return x;
}

long long fool_m2(long long x)
{
  x -= 2;
  if (x == 0)
    x = 16;
  return x;
}

long long fool_p1(long long x)
{
  x++;
  if (x == 0)
    x = 16;
  return x;
}

long long fool_p2(long long x)
{
  x += 2;
  if (x == 0)
    x = 16;
  return x;
}
#endif /* __X86_64__ */

short foos_m1(short x)
{
  x--;
  if (x == 0)
    x = 16;
  return x;
}

short foos_m2(short x)
{
  x -= 2;
  if (x == 0)
    x = 16;
  return x;
}

short foos_p1(short x)
{
  x++;
  if (x == 0)
    x = 16;
  return x;
}

short foos_p2(short x)
{
  x += 2;
  if (x == 0)
    x = 16;
  return x;
}

/* { dg-final { scan-assembler-not "mov(l|q)\[ \\t\]*%(e|r)(cx|di), %(e|r)ax" } } */

