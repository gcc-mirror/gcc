/* PR target/90193 *
/* { dg-do link } */
/* { dg-options "-O1" } */
/* { dg-require-effective-target tls } */

__thread int var;

static int
foo (void)
{
  asm goto ("jmp %l[l]\n\t" : : "m" (var) : : l);
  return 0;
l:
  return 1;
}

int
main ()
{
  return foo ();
}
