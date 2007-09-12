/* PR target/32338 */
/* { dg-do link } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

struct S
{
};

int
__attribute__((noinline))
foo (struct S *d)
{
  return 2;
}

int
__attribute__((noinline))
bar (struct S *d)
{
  return 4;
}

int
__attribute__((noinline))
fnl (char const *q)
{
  return __builtin_strlen (q);
}

int
__attribute__((noinline))
baz (struct S *d, char const *q)
{
  unsigned int len;
  len = fnl (q);
  if (len > 512)
    return bar (d);
  return foo (d);
}

int
main (int argc, char *argv[])
{
  if (argc > 30)
    return baz ((void *) 0, "abcde");
  return 0;
}
