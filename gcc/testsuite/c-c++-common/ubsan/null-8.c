/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

struct S {
  int i;
};

int
main (void)
{
  struct S *s = 0;
  return s->i;
}

/* { dg-output "member access within null pointer of type 'struct S'" } */
