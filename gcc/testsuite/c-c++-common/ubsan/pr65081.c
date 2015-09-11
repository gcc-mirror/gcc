/* PR sanitizer/65081 */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=object-size -fno-sanitize-recover=object-size" } */

struct S
{
  int a;
  char p[1];
};

struct S b;

struct S *
foo ()
{
  struct S *i = &b;
  return i + 1;
}

int
main (void)
{
  struct S *i = foo () - 1;
  i->a = 1;
}
