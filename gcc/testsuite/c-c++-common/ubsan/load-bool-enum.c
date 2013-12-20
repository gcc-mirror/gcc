/* { dg-do run } */
/* { dg-options "-fsanitize=bool,enum" } */

#ifndef __cplusplus
#define bool _Bool
#endif
enum A { B = -3, C = 2 } a;
bool b;

__attribute__((noinline, noclone)) enum A
foo (bool *p)
{
  *p = b;   /* { dg-output "load-bool-enum.c:13:\[^\n\r]*runtime error: load of value 4, which is not a valid value for type '(_B|b)ool'(\n|\r\n|\r)" } */
  return a; /* { dg-output "\[^\n\r]*load-bool-enum.c:14:\[^\n\r]*runtime error: load of value 9, which is not a valid value for type 'A'(\n|\r\n|\r)" { target c++ } } */
}

int
main ()
{
  char c = 4;
  int d = 9;
  if (sizeof (int) != sizeof (a) || sizeof (b) != 1)
    return 0;
  __builtin_memcpy (&a, &d, sizeof (int));
  __builtin_memcpy (&b, &c, 1);
  bool e;
  foo (&e);
  return 0;
}
