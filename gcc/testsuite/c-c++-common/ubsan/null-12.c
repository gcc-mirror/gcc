/* PR sanitizer/80797 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

struct S
{
  int i;
};

struct R
{
  struct T {
    int i;
  } *t;
} r;

int
main ()
{
  struct S *s = 0;
  struct S *s2[1] = { };

  int *v1 = &s->i;
  int *v2 = &(*s).i;
  int *v3 = &s2[0]->i;
  int *v4 = &s->i + 1;
  int *v5 = &r.t->i;

  asm ("" : : "r" (&v1) : "memory");
  asm ("" : : "r" (&v2) : "memory");
  asm ("" : : "r" (&v3) : "memory");
  asm ("" : : "r" (&v4) : "memory");
  asm ("" : : "r" (&v5) : "memory");

  return 0;
}

/* { dg-output "member access within null pointer of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*member access within null pointer of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*member access within null pointer of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*member access within null pointer of type 'struct S'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*member access within null pointer of type 'struct T'" } */
