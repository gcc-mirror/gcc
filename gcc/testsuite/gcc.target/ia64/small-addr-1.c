/* PR target/21632 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct S
{
  void *s[256];
};

struct T
{
  long t[23];
  struct S *u;
};

extern struct T __attribute__((model (small))) v;

void *
foo (void)
{
  return v.u->s[0];
}

