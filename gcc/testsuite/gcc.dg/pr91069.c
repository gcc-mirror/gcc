/* { dg-do run } */
/* { dg-options "-std=gnu11" } */
/* { dg-require-effective-target double64 } */

typedef double v2df __attribute__((vector_size(2 * sizeof (double))));
typedef __INT64_TYPE__ v2di __attribute__((vector_size(2 * sizeof (__INT64_TYPE__))));

void foo (v2df *res, v2df *src)
{
  v2df x = *src;
  *res = __builtin_shuffle ((v2df) { 1.0, 0.0 }, x, (v2di) { 1, 3 });
}

int main()
{
  v2df x = (v2df) { 0.0, 2.0 };
  foo (&x, &x);
  if (x[0] != 0.0 || x[1] != 2.0)
    __builtin_abort ();
  return 0;
}
