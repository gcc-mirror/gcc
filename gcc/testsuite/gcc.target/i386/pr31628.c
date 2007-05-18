/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-fPIC -O2" } */

typedef int tt, *lptt;

int __attribute__((__stdcall__)) bar(lptt);

int __attribute__((__stdcall__)) bar(tt *x)
{
  return 0;
}

int
foo (void)
{
  return bar (0);
}

int
main()
{
  return foo ();
}
