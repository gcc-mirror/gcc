/* Test array initializion by store_by_pieces.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

struct A { char c[10]; };
extern void abort (void);

void
__attribute__((noinline))
check (struct A * a, int b)
{
  const char *p;
  switch (b)
    {
    case 0:
      p = "abcdefghi";
      break;
    case 1:
      p = "j\0\0\0\0\0\0\0\0";
      break;
    case 2:
      p = "kl\0\0\0\0\0\0\0";
      break;
    case 3:
      p = "mnop\0\0\0\0\0";
      break;
    case 4:
      p = "qrstuvwx\0";
      break;
    default:
      abort ();
    }
  if (__builtin_memcmp (a->c, p, 10) != 0)
    abort ();
}

int
main (void)
{
  struct A a = { "abcdefghi" };
  check (&a, 0);
  struct A b = { "j" };
  check (&b, 1);
  struct A c = { "kl" };
  check (&c, 2);
  struct A d = { "mnop" };
  check (&d, 3);
  struct A e = { "qrstuvwx" };
  check (&e, 4);
  return 0;
}
