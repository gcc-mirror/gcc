/* PR rtl-optimization/55023 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

extern void abort (void);
typedef long long int64_t;

struct foo {
    int x;
    int y;
};

int64_t foo(int64_t a, int64_t b, int64_t c)
{
    return a + b + c;
}

int64_t bar(int64_t a, struct foo bq, struct foo cq)
{
    int64_t b = bq.x + bq.y;
    int64_t c = cq.x + cq.y;
    return foo(a, b, c);
}

int main(void)
{
  int64_t a = 1;
  struct foo b = { 2, 3 };
  struct foo c = { 4, 5 };
  if (bar (a, b, c) != 15)
    abort ();
  return 0;
}
