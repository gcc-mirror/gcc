// PR c++/4401
// This testcase was miscompiled on 64-bit platforms, resulting to
// operating on a[0x100000000] instead of a[0].
// { dg-do run }
// { dg-options "-O2" }

char *a;
char b[] = "AAAA";

extern "C" void abort (void);
extern "C" void exit (int);

void foo (void)
{
  unsigned int i, j;

  i = 2;
  j = 3;
  a[i + 1 - j] += i;
}

int main (void)
{
  a = b;
  foo ();
  if (b[0] != 'A' + 2)
    abort ();
  exit (0);
}
