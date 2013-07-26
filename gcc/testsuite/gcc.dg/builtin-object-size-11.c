/* PR48985 */
/* { dg-do run } */
/* { dg-options "-std=gnu89" } */
/* { dg-skip-if "packed attribute missing for struct s" { "epiphany-*-*" } { "*" } { "" } } */

extern void abort (void);

struct s {
    int i;
    char c[];
} s = { 1, "01234" };

__SIZE_TYPE__ f (void) { return __builtin_object_size (&s.c, 0); }

int
main()
{
  if (f() != sizeof ("01234"))
    abort ();

  return 0;
}
