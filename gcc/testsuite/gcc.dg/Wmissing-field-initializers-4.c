/* PR c/82283 */
/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers" } */

struct a {
  int b;
};

struct c {
  struct a d;
  int e;
};

void f (struct c *);

void
g (void)
{
  struct c h = {.d = (struct a){0}}; /* { dg-bogus "missing initializer" } */
  f(&h);
}

struct {
  struct {
    int a;
    int b;
  } c[1];
} d = {
  .c[0].a = 1,
  .c[0].b = 1, /* { dg-bogus "missing initializer" } */
};

struct test_t {
  int value1;
  int value2;
};

struct test_t test[] = {
  [0].value1 = 1,
  [0].value2 = 2, /* { dg-bogus "missing initializer" } */
  [1].value1 = 10,
  [1].value2 = 20 /* { dg-bogus "missing initializer" } */
};
