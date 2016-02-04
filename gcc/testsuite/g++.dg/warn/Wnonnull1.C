// { dg-options -Wnonnull }

void g(void *) __attribute__ ((nonnull (1)));
void f(void *p)
{
  g(1 == 1 ? p : 0);
}
