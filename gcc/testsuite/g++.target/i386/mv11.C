// { dg-do compile }
// { dg-options "-msse2" }

int foo () __attribute__ ((target("default")));
int foo () __attribute__ ((target("sse2")));

int
main ()
{
  return foo ();
}

int  __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("sse2")))
foo ()
{
  return 0;
}
