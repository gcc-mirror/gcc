/* PR rtl-optimization/51505 */
/* { dg-do compile } */
/* { dg-options "-O --param max-cse-insns=1" } */
struct S
{
char a[256];
};

int bar(struct S, char[16]);

void foo ()
{
  struct S u, s1, s2;
  char e[256];
  char i;
  e[i] = ~s1.a[i] & s2.a[i];
  if (bar(u, e))
    __builtin_abort ();
}
