/* { dg-do run } */
/* { dg-options "-O2 -msse -fipa-ra" } */

typedef double v2df __attribute__((vector_size (16)));

static v2df __attribute__((noinline))
bar (v2df a)
{
  return a + (v2df){ 3.0, 3.0 };
}

v2df __attribute__((noinline))
foo (v2df y)
{
  return y + bar (y);
}

int
main (void)
{
  int success;
  union {
    v2df v;
    double d[2];
  } u;

  u.v = foo ((v2df){ 5.0, 5.0});
  success = (u.d[0] == 13.0
	     && u.d[1] == 13.0);

  return !success;
}
