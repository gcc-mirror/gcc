/* { dg-do compile } */
/* { dg-options "-O2 -fno-shrink-wrap" } */

typedef struct { int arr[16]; } S;

void g (S *);
void h (S);
void f(int x)
{
  S s;
  g (&s);
  h (s);
}

/* { dg-final { scan-assembler-not "\[ \t\]?str\[ \t\]x" } } */
