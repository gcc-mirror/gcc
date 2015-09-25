/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

struct S
{
  unsigned long a[1];
  int l;
};

static inline unsigned long
fn (const struct S *s, int i)
{
  return s->a[i] / i;
}

int
main ()
{
  struct S s;
  fn (&s, 1);
}

/* { dg-output "index 1 out of bounds for type 'long unsigned int \\\[1\\\]'" } */
