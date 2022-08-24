/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -mtune=generic -march=i686" } */

/* As PR106322, verify this can execute well (not abort).  */

#define N 64
typedef unsigned short int uh;
typedef unsigned short int uw;
uh a[N];
uh b[N];
uh c[N];
uh e[N];

__attribute__ ((noipa)) void
foo ()
{
  for (int i = 0; i < N; i++)
    c[i] = ((uw) b[i] * (uw) a[i]) >> 16;
}

__attribute__ ((optimize ("-O0"))) void
init ()
{
  for (int i = 0; i < N; i++)
    {
      a[i] = (uh) (0x7ABC - 0x5 * i);
      b[i] = (uh) (0xEAB + 0xF * i);
      e[i] = ((uw) b[i] * (uw) a[i]) >> 16;
    }
}

__attribute__ ((optimize ("-O0"))) void
check ()
{
  for (int i = 0; i < N; i++)
    {
      if (c[i] != e[i])
	__builtin_abort ();
    }
}

int
main ()
{
  init ();
  foo ();
  check ();

  return 0;
}
