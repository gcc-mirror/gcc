/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */

struct B
{
  char a[12];
  int b;
};

struct B
f2 (void)
{
  struct B x = {};
  return x;
}

/* { dg-final { scan-assembler-not "(sub|add)(l|q)\[\\t \]*\\$\[0-9\]*,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-\[0-9\]+,\[^\\n\]*sp" } } */
