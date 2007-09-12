/* PR target/32338 */
/* { dg-do link } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

struct S
{
};

int
__attribute__((noinline))
foo (void)
{
  return 2;
}

int
__attribute__((noinline))
bar (void)
{
  return 4;
}

int
__attribute__((noinline))
fnl (void)
{
  return 6;
}

int
__attribute__((noinline))
baz (void)
{
  unsigned int len;
  len = fnl ();
  if (len > 512)
    return bar ();
  return foo ();
}

int
main (int argc, char *argv[])
{
  if (argc > 30)
    return baz ();
  return 0;
}
