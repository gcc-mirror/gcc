/* PR 3997 */
/* { dg-do run } */
/* { dg-require-alias "" } */

extern void abort (void);
extern void exit (int);

void foo(void)
{
  exit(0);
}

static void bar(void) __attribute__((alias("foo")));

int main()
{
  bar();
  abort ();
}
