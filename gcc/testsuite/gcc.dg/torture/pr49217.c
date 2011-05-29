/* { dg-do run } */
/* { dg-options "-fno-early-inlining" } */

extern void abort (void);
int i;
static void foo(void);
void __attribute__((noinline))
bar (void)
{
  if (!i)
    foo ();
}
static void
foo(void)
{
  i = 1;
  bar ();
}
int main()
{
  i = 0;
  bar();
  if (i != 1)
    abort ();
  return 0;
}
