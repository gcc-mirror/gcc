/* PR middle-end/27945 */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-require-effective-target alloca } */

extern int printf (const char *, ...);
extern void abort ();

int func(int levels) 
{
  struct bar {
    unsigned char	a;
    int			b[levels];
  } __attribute__ ((__packed__)) bar;

  struct foo {
    unsigned char	a;
    int			b[4];
  } __attribute__ ((__packed__)) foo;

  printf("foo %d\n", sizeof(foo));
  printf("bar %d\n", sizeof(bar));

  if (sizeof (foo) != sizeof (bar))
    abort ();
}

int main()
{
  func(4);
  return 0;
}
