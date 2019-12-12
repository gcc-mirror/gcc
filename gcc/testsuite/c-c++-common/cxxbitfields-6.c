/* PR middle-end/50141 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-allow-store-data-races" } */

struct S
{
  int i:8;
};

void bar (struct S, int);

void
foo (struct S s, int i)
{
  s.i = i;
  bar (s, i);
}
