/* PR sanitizer/80932 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

int x = 1;

int
foo (void)
{
  return ((int) (2855545792U * x) - (int) (3269399503U * x)) * -5;
}

int
main ()
{
  foo ();
}
