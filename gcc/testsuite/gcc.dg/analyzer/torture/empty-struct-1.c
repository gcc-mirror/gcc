struct empty {};

struct empty g;

extern void sink (struct empty e);

void test_1 (struct empty a)
{
  sink (a); /* { dg-bogus "uninit" } */
}
void test_2 ()
{
  struct empty a, b;
  b = a;
  g = b;
  sink (b); /* { dg-bogus "uninit" } */
  /* ...as there's nothing to initialize.  */
}
